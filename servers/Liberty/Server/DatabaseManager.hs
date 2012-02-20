{-# LANGUAGE OverloadedStrings #-}

module Liberty.Server.DatabaseManager (
  DatabaseHandleTVar,
  initializeDatabaseManager,
  runDatabaseManager,
  getSiteDataFromDb
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
import Liberty.Server.Types

-- public data
type DatabaseHandleTVar = TVar (Maybe DatabaseHandle)

-- private data
data DatabaseHandle = DatabaseHandle Pipe

initializeDatabaseManager :: IO (DatabaseHandleTVar)
initializeDatabaseManager = atomically $ newTVar Nothing -- initially, no connection

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
  -- wait 5 seconds before checking
  threadDelay $ 30000 * 1000

  res <- runQuery databaseHandleTVar keepAliveAction
  case res of
    Just _ -> putStrLn "Keepalive succeeded"
    Nothing -> putStrLn "Keepalive failed!"
  connectionKeepAliveLoop databaseHandleTVar
  where
    -- just some random query
    keepAliveAction = find (select [] "keepalive")

runQuery :: DatabaseHandleTVar -> Action IO a -> IO (Maybe a)
runQuery databaseHandleTVar action = do
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
              putStrLn $ "Query failure: " ++ show f
              notifyDatabaseFailure databaseHandleTVar
              return Nothing
        else do
          putStrLn "Database pipe appears to be closed"
          notifyDatabaseFailure databaseHandleTVar
          return Nothing
    Nothing -> return Nothing

getSiteDataFromDb :: DatabaseHandleTVar -> IO (Maybe [SiteData])
getSiteDataFromDb databaseHandleTVar =
  let
    action = (find $ select [] "sites") >>= rest
  in do
    res <- runQuery databaseHandleTVar action
    case res of
      Just siteDocs -> do
        putStrLn $ "num siteDocs: " ++ show (length siteDocs)
        return $ mapM (\siteDoc ->
          case (,,) <$>
            (asMaybeText $ lookup "siteId" siteDoc) <*>
            (asMaybeText $ lookup "name" siteDoc) <*>
            (lookup "operators" siteDoc :: Maybe [Document])
          of
            Just (siteId, siteName, operatorsDocs) ->
              case mapM (\operatorDoc ->
                SiteOperatorInfo <$>
                  (asMaybeText $ lookup "username" operatorDoc) <*>
                  (asMaybeText $ lookup "password" operatorDoc) <*>
                  (asMaybeText $ lookup "name" operatorDoc) <*>
                  (asMaybeText $ lookup "color" operatorDoc) <*>
                  (asMaybeText $ lookup "title" operatorDoc) <*>
                  (asMaybeText $ lookup "icon" operatorDoc)
                ) operatorsDocs
              of
                Just siteOperatorInfos -> Just $ SiteData siteId siteName [] siteOperatorInfos [] 0
                Nothing -> Nothing
            Nothing -> Nothing
          ) siteDocs
      Nothing -> return Nothing

asMaybeText :: Maybe String -> Maybe Text
asMaybeText maybeString = fmap LT.pack maybeString

