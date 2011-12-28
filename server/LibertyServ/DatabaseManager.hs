{-# LANGUAGE OverloadedStrings #-}

module LibertyServ.DatabaseManager (
  DatabaseHandle,
  initializeDatabaseManager,
  runDatabaseManager
) where
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception (SomeException, catch)
import Control.Monad.STM
import Control.Monad.Trans (liftIO)
import Database.MongoDB
import Prelude hiding (catch)

data DatabaseHandle = DatabaseHandle Pipe

initializeDatabaseManager :: IO (TVar (Maybe DatabaseHandle))
initializeDatabaseManager = atomically $ newTVar $ Nothing -- initially, no connection

runDatabaseManager :: TVar (Maybe DatabaseHandle) -> IO ()
runDatabaseManager databaseHandleTVar = do
  _ <- forkIO $ connectionKeepAliveLoop databaseHandleTVar
  databaseManagerLoop databaseHandleTVar

databaseManagerLoop :: TVar (Maybe DatabaseHandle) -> IO ()
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

notifyDatabaseFailure :: TVar (Maybe DatabaseHandle) -> IO ()
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

connectionKeepAliveLoop :: TVar (Maybe DatabaseHandle) -> IO ()
connectionKeepAliveLoop databaseHandleTVar = do
  res <- runQuery databaseHandleTVar run
  case res of
    Just doc -> putStrLn "Keepalive succeeded"
    Nothing -> putStrLn "Keepalive failed!"

  -- wait 5 seconds and loop
  threadDelay $ 5000 * 1000
  connectionKeepAliveLoop databaseHandleTVar

runQuery :: TVar (Maybe DatabaseHandle) -> Action IO a -> IO (Maybe a)
runQuery databaseHandleTVar action = do
  databaseHandle <- atomically $ do
    maybeDatabaseHandle <- readTVar databaseHandleTVar
    case maybeDatabaseHandle of
      Just handle -> return handle
      Nothing -> retry -- wait until there is a handle available

  case databaseHandle of
    DatabaseHandle pipe -> do
      isClosed' <- isClosed pipe
      if not isClosed' then do
        result <- access pipe master "libertyusers" action
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

--run = find (select [] "users") >>= rest >>= printDocs
run :: Action IO [Document]
run = do
  findRet <- find (select [] "users")
  restRet <- rest findRet
  --printDocs restRet
  return restRet

