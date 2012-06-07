{-# LANGUAGE ScopedTypeVariables #-}

module Liberty.AuthServer.ServiceHandlers (
  handleMessage
) where
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import Prelude hiding (catch)
import Liberty.Common.Messages
import Liberty.Common.Messages.AuthServer
import Liberty.Common.ServiceServer
import Liberty.AuthServer.Types

handleMessage :: AuthServerMessageType -> ByteString -> ClientSendChan -> SessionMapTVar -> IO ()
handleMessage messageType encodedParams clientSendChan sessionMapTVar = do
  case messageType of
    ASMTVerifySessionId -> unpackAndHandle $ \(sessionId) -> handleASMTVerifySessionId sessionId clientSendChan sessionMapTVar
    _ -> do
      putStrLn "Client sent an unknown command"
      atomically $ closeClientSocket clientSendChan
  where
    unpackAndHandle handleFunction =
      case unpackMessage encodedParams of
        Just params -> handleFunction params
        Nothing -> do
          putStrLn "Client dropped due to message unpack failure."
          atomically $ closeClientSocket clientSendChan

handleASMTVerifySessionId :: Text -> ClientSendChan -> SessionMapTVar -> IO ()
handleASMTVerifySessionId sessionId clientSendChan sessionMapTVar = do
  sessionMap <- atomically $ readTVar sessionMapTVar
  case Map.lookup sessionId sessionMap of
    Just (SessionEntry userId email) -> atomically $ createAndSendMessage ASMTVerifySessionIdSuccess (userId, email) clientSendChan
    Nothing -> atomically $ createAndSendMessage ASMTVerifySessionIdFailure () clientSendChan

