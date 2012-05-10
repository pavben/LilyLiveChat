{-# LANGUAGE OverloadedStrings #-}

module Liberty.SiteDataService.DatabaseManager (
  DatabaseHandleTVar,
  initializeDatabaseManager,
  runDatabaseManager,
  GetSiteDataResult(..),
  getSiteDataFromDb,
  saveSiteDataToDb
) where
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception hiding (handle)
import Control.Monad
import Control.Monad.STM
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Data.Maybe
import Database.MongoDB
import Prelude hiding (catch, lookup)
import Liberty.SiteDataService.Types

-- public data
type DatabaseHandleTVar = TVar (Maybe DatabaseHandle)

-- private data
data DatabaseHandle = DatabaseHandle Pipe

initializeDatabaseManager :: IO (DatabaseHandleTVar)
initializeDatabaseManager = atomically $ do
  databaseHandleTVar <- newTVar Nothing -- initially, no connection
  return databaseHandleTVar

runDatabaseManager :: DatabaseHandleTVar -> IO ()
runDatabaseManager databaseHandleTVar = do
  _ <- forkIO $ connectionKeepAliveLoop databaseHandleTVar
  databaseManagerLoop databaseHandleTVar

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

data GetSiteDataResult = GSDRSuccess SiteData | GSDRNotFound | GSDRNotAvailable
getSiteDataFromDb :: SiteId -> DatabaseHandleTVar -> IO (GetSiteDataResult)
getSiteDataFromDb siteIdToLookup databaseHandleTVar = do
  res <- runAction databaseHandleTVar (findOne (select ["siteId" =: (LT.unpack siteIdToLookup)] "sites"))
  -- one layer of Maybe from runAction, and the other from findOne
  case res of
    Just maybeSiteDoc ->
      case maybeSiteDoc of
        Just siteDoc ->
          case (,,,,) <$>
            (asMaybeText $ lookup "siteId" siteDoc) <*>
            (asMaybeText $ lookup "name" siteDoc) <*>
            (lookup "nextOperatorId" siteDoc :: Maybe Integer) <*>
            (lookup "operators" siteDoc :: Maybe [Document]) <*>
            (asMaybeText $ lookup "adminPasswordHash" siteDoc)
          of
            Just (siteId, name, nextOperatorId, operators, adminPasswordHash) ->
              let
                maybeOperatorDatas = mapM (\operatorDoc ->
                  SiteOperatorData <$>
                    (lookup "operatorId" operatorDoc :: Maybe Integer) <*>
                    (asMaybeText $ lookup "username" operatorDoc) <*>
                    (asMaybeText $ lookup "passwordHash" operatorDoc) <*>
                    (asMaybeText $ lookup "name" operatorDoc) <*>
                    (asMaybeText $ lookup "color" operatorDoc) <*>
                    (asMaybeText $ lookup "title" operatorDoc) <*>
                    (asMaybeText $ lookup "icon" operatorDoc)
                  ) operators
              in
                case maybeOperatorDatas of
                  Just siteOperatorDatas -> return $ GSDRSuccess $ SiteData siteId name nextOperatorId siteOperatorDatas adminPasswordHash
                  Nothing -> return GSDRNotAvailable -- can't read operators
            Nothing -> return GSDRNotAvailable -- can't read site data
        Nothing -> return GSDRNotFound -- that siteId doesn't exist
    Nothing -> return GSDRNotAvailable -- runAction failed

asMaybeText :: Maybe String -> Maybe Text
asMaybeText maybeString = fmap LT.pack maybeString

saveSiteDataToDb :: SiteId -> SiteData -> DatabaseHandleTVar -> IO Bool
saveSiteDataToDb currentSiteId siteData databaseHandleTVar = do
  actionResult <- runAction databaseHandleTVar (
    repsert (
      select
        ["siteId" := (asStringValue $ currentSiteId)]
        "sites")
      [
        "siteId" := (asStringValue $ sdSiteId siteData),
        "name" := (asStringValue $ sdName siteData),
        "nextOperatorId" := Int32 (fromInteger $ sdNextOperatorId siteData),
        "operators" := Array (map (\siteOperatorData ->
          Doc [
            "operatorId" := Int32 (fromInteger $ sodOperatorId siteOperatorData),
            "username" := (asStringValue $ sodUsername siteOperatorData),
            "passwordHash" := (asStringValue $ sodPasswordHash siteOperatorData),
            "name" := (asStringValue $ sodName siteOperatorData),
            "color" := (asStringValue $ sodColor siteOperatorData),
            "title" := (asStringValue $ sodTitle siteOperatorData),
            "icon" := (asStringValue $ sodIconUrl siteOperatorData)
            ]
          ) (sdOperators siteData)),
        "adminPasswordHash" := (asStringValue $ sdAdminPasswordHash siteData)
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

