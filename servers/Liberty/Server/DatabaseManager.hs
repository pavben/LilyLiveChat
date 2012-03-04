{-# LANGUAGE OverloadedStrings #-}

module Liberty.Server.DatabaseManager (
  DatabaseHandleTVar,
  initializeDatabaseManager,
  runDatabaseManager,
  getSiteDataFromDb,
  queueSaveSiteData
) where
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Exception hiding (handle)
import Control.Monad
import Control.Monad.STM
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Data.Maybe
import Database.MongoDB
import Prelude hiding (catch, lookup)
import Liberty.Server.Types

-- public data
type DatabaseHandleTVar = TVar (Maybe DatabaseHandle)

-- private data
data DatabaseHandle = DatabaseHandle Pipe

initializeDatabaseManager :: IO (DatabaseHandleTVar, DatabaseOperationQueueChan)
initializeDatabaseManager = atomically $ do
  databaseHandleTVar <- newTVar Nothing -- initially, no connection
  databaseOperationQueueChan <- newTChan
  return (databaseHandleTVar, databaseOperationQueueChan)

runDatabaseManager :: DatabaseHandleTVar -> DatabaseOperationQueueChan -> IO ()
runDatabaseManager databaseHandleTVar databaseOperationQueueChan = do
  _ <- forkIO $ connectionKeepAliveLoop databaseHandleTVar
  _ <- forkIO $ databaseOperationQueueWorker databaseOperationQueueChan Nothing databaseHandleTVar
  databaseManagerLoop databaseHandleTVar

databaseOperationQueueWorker :: DatabaseOperationQueueChan -> Maybe SiteData -> DatabaseHandleTVar -> IO ()
databaseOperationQueueWorker databaseOperationQueueChan maybeRetrySiteData databaseHandleTVar = do
  -- currently, this can't be Nothing, but that can be implemented later to support clean shutdowns
  maybeSiteData <- case maybeRetrySiteData of
    Just siteData -> return $ Just siteData
    Nothing -> do
      msg <- atomically $ readTChan databaseOperationQueueChan
      case msg of
        SaveSite siteData -> return $ Just siteData

  case maybeSiteData of
    Just siteData -> do
      saveResult <- saveSiteData siteData databaseHandleTVar
      if saveResult then
        -- recurse to read the next message
        databaseOperationQueueWorker databaseOperationQueueChan Nothing databaseHandleTVar
      else do
        putStrLn $ "Error saving site: " ++ show siteData ++ ". Retrying in 5 seconds..."
        threadDelay (5 * 1000 * 1000)
        -- recurse passing the siteData for retry
        databaseOperationQueueWorker databaseOperationQueueChan (Just siteData) databaseHandleTVar
    Nothing -> return () -- just exit (currently not possible)

databaseManagerLoop :: DatabaseHandleTVar -> IO ()
databaseManagerLoop databaseHandleTVar = do
  atomically $ do
    databaseHandle <- readTVar databaseHandleTVar
    when (isJust databaseHandle) retry

  putStrLn "No active database connection available. Connecting..."

  maybeHandle <- connectToDatabase
  case maybeHandle of
    Just handle -> do
      putStrLn "Database connection established."
      atomically $ writeTVar databaseHandleTVar $ Just handle
    Nothing -> do
      putStrLn "Database connection failed. Waiting 10 seconds and trying again."
      threadDelay (10000 * 1000) -- otherwise, sleep for 10 seconds and try connecting again

  -- loop around and keep monitoring the connection state
  databaseManagerLoop databaseHandleTVar

notifyDatabaseFailure :: DatabaseHandleTVar -> IO ()
notifyDatabaseFailure databaseHandleTVar = do
  pipeToMaybeClose <- atomically $ do
    maybeHandle <- readTVar databaseHandleTVar
    case maybeHandle of
      Just (DatabaseHandle pipe) -> do
        writeTVar databaseHandleTVar Nothing
        return $ Just pipe
      Nothing -> return Nothing

  -- when Nothing, someone already reported this failure, so they would have closed the pipe
  -- close if not Nothing
  maybe (return ()) close pipeToMaybeClose

connectToDatabase :: IO (Maybe DatabaseHandle)
connectToDatabase =
  catch
  (do
    pipe <- runIOE $ connect' 5 (host "127.0.0.1")
    return $ Just $ DatabaseHandle pipe
  )
  (\(SomeException _) -> return Nothing)

connectionKeepAliveLoop :: DatabaseHandleTVar -> IO ()
connectionKeepAliveLoop databaseHandleTVar = do
  -- keepalive interval
  threadDelay $ 30000 * 1000

  res <- runAction databaseHandleTVar keepAliveAction
  case res of
    Just _ -> putStrLn "Keepalive succeeded"
    Nothing -> putStrLn "Keepalive failed!"
  connectionKeepAliveLoop databaseHandleTVar
  where
    -- just some random query
    keepAliveAction = find (select [] "keepalive")

runAction :: DatabaseHandleTVar -> Action IO a -> IO (Maybe a)
runAction databaseHandleTVar action = do
  maybeDatabaseHandle <- atomically $ readTVar databaseHandleTVar
  case maybeDatabaseHandle of
    Just databaseHandle -> case databaseHandle of
      DatabaseHandle pipe -> do
        isClosed' <- isClosed pipe
        if not isClosed' then do
          result <- access pipe master "liberty" action
          case result of
            Right res -> return $ Just res
            Left f -> do
              putStrLn $ "Database action failure: " ++ show f
              notifyDatabaseFailure databaseHandleTVar
              return Nothing
        else do
          putStrLn "Database pipe appears to be closed"
          notifyDatabaseFailure databaseHandleTVar
          return Nothing
    Nothing -> return Nothing

getSiteDataFromDb :: DatabaseHandleTVar -> IO (Maybe [SiteData])
getSiteDataFromDb databaseHandleTVar = do
  res <- runAction databaseHandleTVar (find (select [] "sites") >>= rest)
  case res of
    Just siteDocs ->
      return $ mapM (\siteDoc ->
        case (,,,,,,) <$>
          (asMaybeText $ lookup "siteId" siteDoc) <*>
          (asMaybeText $ lookup "name" siteDoc) <*>
          (lookup "expiryTimestamp" siteDoc :: Maybe Integer) <*>
          (lookup "nextOperatorId" siteDoc :: Maybe Integer) <*>
          (lookup "operators" siteDoc :: Maybe [Document]) <*>
          (lookup "nextAdminId" siteDoc :: Maybe Integer) <*>
          (lookup "admins" siteDoc :: Maybe [Document])
        of
          Just (siteId, name, expiryTimestamp, nextOperatorId, operators, nextAdminId, admins) ->
            let
              maybeOperatorDatas = mapM (\operatorDoc ->
                SiteOperatorData <$>
                  (lookup "operatorId" operatorDoc :: Maybe Integer) <*>
                  (asMaybeText $ lookup "username" operatorDoc) <*>
                  (asMaybeText $ lookup "password" operatorDoc) <*>
                  (asMaybeText $ lookup "name" operatorDoc) <*>
                  (asMaybeText $ lookup "color" operatorDoc) <*>
                  (asMaybeText $ lookup "title" operatorDoc) <*>
                  (asMaybeText $ lookup "icon" operatorDoc)
                ) operators
              maybeAdminDatas = mapM (\adminDoc ->
                SiteAdminData <$>
                  (lookup "adminId" adminDoc :: Maybe Integer) <*>
                  (asMaybeText $ lookup "username" adminDoc) <*>
                  (asMaybeText $ lookup "password" adminDoc)
                ) admins
            in
              case (,) <$> maybeOperatorDatas <*> maybeAdminDatas of
                Just (siteOperatorDatas, siteAdminDatas) -> Just $ SiteData siteId name expiryTimestamp nextOperatorId siteOperatorDatas nextAdminId siteAdminDatas [] [] [] 0
                Nothing -> Nothing
          Nothing -> Nothing
        ) siteDocs
    Nothing -> return Nothing

asMaybeText :: Maybe String -> Maybe Text
asMaybeText maybeString = fmap LT.pack maybeString

-- TODO: rewrite this to use Set for eliminating duplicates and pass siteDataTVar to the saver instead of siteData
queueSaveSiteData :: SiteDataTVar -> DatabaseOperationQueueChan -> STM ()
queueSaveSiteData siteDataTVar databaseOperationQueueChan = do
  siteData <- readTVar siteDataTVar
  writeTChan databaseOperationQueueChan $ SaveSite siteData

saveSiteData :: SiteData -> DatabaseHandleTVar -> IO Bool
saveSiteData siteData databaseHandleTVar = do
  actionResult <- runAction databaseHandleTVar (
    repsert (
      select
        ["siteId" := (asStringValue $ sdSiteId siteData)]
        "sites")
      [
        "siteId" := (asStringValue $ sdSiteId siteData),
        "name" := (asStringValue $ sdName siteData),
        "expiryTimestamp" := Int64 (fromInteger $ sdExpiryTimestamp siteData),
        "nextOperatorId" := Int32 (fromInteger $ sdNextOperatorId siteData),
        "operators" := Array (map (\siteOperatorData ->
          Doc [
            "operatorId" := Int32 (fromInteger $ sodOperatorId siteOperatorData),
            "username" := (asStringValue $ sodUsername siteOperatorData),
            "password" := (asStringValue $ sodPassword siteOperatorData),
            "name" := (asStringValue $ sodName siteOperatorData),
            "color" := (asStringValue $ sodColor siteOperatorData),
            "title" := (asStringValue $ sodTitle siteOperatorData),
            "icon" := (asStringValue $ sodIconUrl siteOperatorData)
            ]
          ) (sdOperators siteData)),
        "nextAdminId" := Int32 (fromInteger $ sdNextAdminId siteData),
        "admins" := Array (map (\siteAdminData ->
          Doc [
            "adminId" := Int32 (fromInteger $ sadAdminId siteAdminData),
            "username" := (asStringValue $ sadUsername siteAdminData),
            "password" := (asStringValue $ sadPassword siteAdminData)
            ]
          ) (sdAdmins siteData))
        ]
    )

  case actionResult of
    Just _ -> do
      putStrLn $ "Site data saved: " ++ show siteData
      return True
    Nothing -> do
      putStrLn $ "Error saving site data: " ++ show siteData
      return False
  
asStringValue :: Text -> Value
asStringValue text = String (u . LT.unpack $ text)

