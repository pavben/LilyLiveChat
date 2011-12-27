{-# LANGUAGE OverloadedStrings #-}

module LibertyServ.DatabaseManager (
  DatabaseHandle,
  initializeDatabaseManager,
  runDatabaseManager,
  notifyDatabaseFailure -- temp
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
  handleToMaybeClose <- atomically $ do
    maybeHandle <- readTVar databaseHandleTVar
    case maybeHandle of
      Just (DatabaseHandle handle) -> do
        writeTVar databaseHandleTVar Nothing
        return $ Just handle
      Nothing -> return Nothing

  case handleToMaybeClose of
    Just handle -> close handle
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

{-
conn = do
  pipe <- runIOE $ connect (host "127.0.0.1")
  e <- access pipe master "libertyusers" run
  close pipe
  case e of
    Left f -> do
      putStrLn "failboat"
      return []
    Right v -> do
      print v
      close pipe
      return v

printDocs docs = liftIO $ mapM_ print docs

--run = find (select [] "users") >>= rest >>= printDocs
run = do
  findRet <- find (select [] "users")
  restRet <- rest findRet
  --printDocs restRet
  return restRet
-}

