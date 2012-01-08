module Liberty.WebGateway.Sessions (
  SessionId,
  SessionData(..),
  SessionDataTVar,
  SessionMapTVar,
  createSessionMapTVar,
  createSession
) where
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import Liberty.WebGateway.RandomString
import Liberty.Common.NetworkMessage

type SessionId = Text
type SequenceNumber = Integer
type InSequence = SequenceNumber
type OutSequence = SequenceNumber
data ProxyConnectionState = ProxyConnectionActive | ProxyConnectionClosed
data SessionData = SessionData {
  sdCurrentWaiterSocket :: Maybe Socket,
  sdProxySocket :: Socket,
  sdProxyConnectionState :: ProxyConnectionState,
  sdLastInSequence :: InSequence,
  sdLastOutSequence :: OutSequence,
  sdMessagesWaiting :: [(OutSequence, Message)]
}
type SessionDataTVar = TVar SessionData
type SessionMapTVar = TVar (Map SessionId SessionDataTVar)

createSessionMapTVar :: IO SessionMapTVar
createSessionMapTVar = atomically $ newTVar $ Map.empty

createSession :: SessionMapTVar -> Socket -> IO (Maybe SessionId)
createSession sessionMapTVar clientSocket = do
  maybeProxySocket <- establishProxyConnection
  case maybeProxySocket of
    Just proxySocket -> do
      sessionId <- tryCreateSessionUntilSuccess sessionMapTVar proxySocket
      return $ Just sessionId
    Nothing -> do
      putStrLn "Failed to establish proxy connection -- session will not be issued"
      return Nothing

tryCreateSessionUntilSuccess :: SessionMapTVar -> Socket -> IO SessionId
tryCreateSessionUntilSuccess sessionMapTVar proxySocket = do
  newSessionId <- getRandomText128
  createResult <- atomically $ do
    sessionMap <- readTVar sessionMapTVar
    case Map.lookup newSessionId sessionMap of
      Just _ -> return False
      Nothing -> do
        newSessionEntry <- newTVar $ SessionData Nothing proxySocket ProxyConnectionActive 0 0 []
        writeTVar sessionMapTVar $ Map.insert newSessionId newSessionEntry sessionMap
        return True

  case createResult of
    True -> return newSessionId
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

