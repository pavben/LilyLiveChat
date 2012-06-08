module Liberty.AuthServer.SessionMap (
  initializeSessionMap,
  createSession
) where
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Map as Map
import Liberty.Common.RandomString
import Liberty.AuthServer.Types

initializeSessionMap :: IO SessionMapTVar
initializeSessionMap = do
  sessionMapTVar <- atomically $ newTVar Map.empty
  return $ sessionMapTVar

-- returns sessionId
createSession :: Text -> Text -> SessionMapTVar -> IO Text
createSession userId email sessionMapTVar =
  let
    createSession' len = do
      sessionId <- getRandomAlphanumericText len
      sessionCreateResult <- atomically $ do
        sessionMap <- readTVar sessionMapTVar
        case Map.lookup sessionId sessionMap of
          Nothing -> do
            writeTVar sessionMapTVar $ Map.insert sessionId (SessionEntry userId email) sessionMap
            return True
          Just _ -> return False
      
      if sessionCreateResult then do
        -- spawn a thread to expire the session
        _ <- forkIO $ do
          threadDelay (24 * 60 * 60 * 1000 * 1000) -- 24 hours
          putStrLn $ "Session " ++ LT.unpack sessionId ++ " expired."
          atomically $ do
            sessionMap <- readTVar sessionMapTVar
            writeTVar sessionMapTVar $ Map.delete sessionId sessionMap
        return sessionId
      else
        -- if the session id already exists, try creating a longer one
        createSession' (len + 1)
  in
    createSession' 32

