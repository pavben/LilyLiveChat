{-# LANGUAGE OverloadedStrings #-}

module LibertyServ.DatabaseManager (
  DatabaseHandleTVar,
  LookupFailureReason(..),
  initializeDatabaseManager,
  runDatabaseManager,
  getSiteDataFromDb
) where
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception (SomeException, catch)
import Control.Monad.STM
import Control.Monad.Trans (liftIO)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Database.MongoDB
import Prelude hiding (catch)
import LibertyServ.Site

-- public data
type DatabaseHandleTVar = TVar (Maybe DatabaseHandle)
data LookupFailureReason = LookupFailureNotExist | LookupFailureTechnicalError

-- private data
data DatabaseHandle = DatabaseHandle Pipe

initializeDatabaseManager :: IO (DatabaseHandleTVar)
initializeDatabaseManager = atomically $ newTVar $ Nothing -- initially, no connection

runDatabaseManager :: DatabaseHandleTVar -> IO ()
runDatabaseManager databaseHandleTVar = do
  _ <- forkIO $ connectionKeepAliveLoop databaseHandleTVar
  databaseManagerLoop databaseHandleTVar

databaseManagerLoop :: DatabaseHandleTVar -> IO ()
databaseManagerLoop databaseHandleTVar = do
  atomically $ do
    databaseHandle <- readTVar databaseHandleTVar
    case databaseHandle of
      Just _ -> retry -- do nothing while the connection is active
      Nothing -> return ()

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

  case pipeToMaybeClose of
    Just pipe -> close pipe
    Nothing -> return () -- someone already reported this failure, so they would have closed the pipe

connectToDatabase :: IO (Maybe DatabaseHandle)
connectToDatabase =
  catch
  (do
    pipe <- runIOE $ connect' 5 (host "127.0.0.1")
    return $ Just $ DatabaseHandle pipe
  )
  (\ex -> do
    let _ = ex :: SomeException
    return Nothing
  )

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

getSiteDataFromDb :: DatabaseHandleTVar -> SiteId -> IO (Either LookupFailureReason SiteData)
getSiteDataFromDb databaseHandleTVar siteId =
  let
    action = (find $ select ["id" =: siteId] "sites") >>= rest
  in do
    res <- runQuery databaseHandleTVar action
    case res of
      Just docs -> do
        putStrLn $ "found " ++ show (length docs) ++ " docs"
        if length docs == 1 then
          let
            firstDoc = head $ docs
            siteId' = "id" `at` firstDoc :: SiteId
            siteNameStr = "name" `at` firstDoc :: String
          in do
            putStrLn "Ok :)"
            mapM_ print docs
            return $ Right $ SiteData siteId' (LT.pack siteNameStr)
        else do
          putStrLn "Wrong num of rows"
          return $ Left $ LookupFailureTechnicalError
      Nothing -> return $ Left $ LookupFailureTechnicalError

