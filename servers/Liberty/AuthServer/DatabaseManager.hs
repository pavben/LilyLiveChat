{-# LANGUAGE OverloadedStrings #-}

module Liberty.AuthServer.DatabaseManager (
  DatabaseHandleTVar,
  initializeDatabaseManager,
  runDatabaseManager,
  GetUserDataResult(..),
  getUserIdForGoogleIdentifier,
  saveUserDataToDb
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
import Liberty.AuthServer.Types

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
  threadDelay $ 60000 * 1000

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

data GetUserDataResult = GUDRSuccess UserData | GUDRNotFound | GUDRNotAvailable

getUserIdForGoogleIdentifier :: Text -> DatabaseHandleTVar -> IO GetUserDataResult
getUserIdForGoogleIdentifier googleIdentifierToSearch databaseHandleTVar = do
  res <- runAction databaseHandleTVar (findOne (select ["googleIdentifier" =: (LT.unpack googleIdentifierToSearch)] "users"))
  case res of
    Just maybeUserDoc ->
      case maybeUserDoc of
        Just userDoc ->
          case (,,) <$>
            (asMaybeText $ lookup "userId" userDoc) <*>
            (asMaybeText $ lookup "email" userDoc) <*>
            (asMaybeText $ lookup "googleIdentifier" userDoc)
          of
            Just (userId, email, googleIdentifier) -> return $ GUDRSuccess $ UserData userId email googleIdentifier
            Nothing -> return GUDRNotAvailable -- can't read user data
        Nothing -> return GUDRNotFound -- that identifier doesn't exist
    Nothing -> return GUDRNotAvailable -- runAction failed

asMaybeText :: Maybe String -> Maybe Text
asMaybeText maybeString = fmap LT.pack maybeString

saveUserDataToDb :: UserData -> DatabaseHandleTVar -> IO Bool
saveUserDataToDb userData databaseHandleTVar = do
  actionResult <- runAction databaseHandleTVar (
    repsert (
      select
        ["userId" := (asStringValue $ udUserId userData)]
        "users")
      [
        "userId" := (asStringValue $ udUserId userData),
        "email" := (asStringValue $ udEmail userData),
        "googleIdentifier" := (asStringValue $ udGoogleIdentifier userData)
      ]
    )

  case actionResult of
    Just _ -> do
      putStrLn $ "User data saved: " ++ show userData
      return True
    Nothing -> do
      putStrLn $ "Error saving user data: " ++ show userData
      return False
  
asStringValue :: Text -> Value
asStringValue text = String (LT.toStrict $ text)

