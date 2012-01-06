module Liberty.WebGateway.Sessions (
  SessionId,
  createSessionMapTVar,
  createSession
) where
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Map (Map)
import qualified Data.Map as Map
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Liberty.WebGateway.RandomString
import Liberty.Common.NetworkMessage

type SessionId = ByteString
data ProxyConnectionState = ProxyConnectionActive | ProxyConnectionClosed
data SessionData = SessionData {
  sdProxySocket :: Socket,
  sdProxyConnectionState :: ProxyConnectionState,
  sdMessagesWaiting :: [Message]
}
type SessionEntryTVar = TVar SessionData
type SessionMapTVar = TVar (Map SessionId SessionEntryTVar)

createSessionMapTVar :: IO SessionMapTVar
createSessionMapTVar = atomically $ newTVar $ Map.empty

createSession :: SessionMapTVar -> Socket -> IO (Maybe SessionId)
createSession sessionMapTVar clientSocket = do
  maybeProxySocket <- establishProxyConnection
  case maybeProxySocket of
    Just proxySocket -> do
      sessionId <- tryCreateSessionUntilSuccess
      return $ Just sessionId
    Nothing -> do
      putStrLn "Failed to establish proxy connection -- session will not be issued"
      return Nothing

tryCreateSessionUntilSuccess :: SessionMapTVar -> Socket -> IO SessionId
tryCreateSessionUntilSuccess sessionMapTVar proxySocket = do
  newSessionId <- getRandomByteString128
  createResult <- atomically $ do
    sessionMap <- readTVar sessionMapTVar
    case Map.lookup newSessionId sessionMap of
      Just _ -> return False
      Nothing -> do
        Map.insert newSessionId (newTVar (SessionData proxySocket ProxyConnectionActive [])) sessionMap
        writeTVar sessionMapTVar sessionMap
        return True

  case createResult of
    True -> newSessionId
    False -> tryCreateSessionUntilSuccess sessionMapTVar proxySocket

establishProxyConnection :: IO (Maybe Socket)
establishProxyConnection = catch
  (do
    connectionSocket <- socket AF_INET Stream 0
    hostAddr <- inet_addr "127.0.0.1"
    connect connectionSocket (SockAddrInet 9801 hostAddr)
    return $ Just connectionSocket
  )
  (\(SomeException e) -> return Nothing)

