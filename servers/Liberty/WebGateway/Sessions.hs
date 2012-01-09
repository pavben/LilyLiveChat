module Liberty.WebGateway.Sessions (
  SessionId,
  SequenceNumber,
  InSequence,
  OutSequence,
  SessionData(..),
  SessionDataTVar,
  SessionMapTVar,
  createSessionMapTVar,
  createSession
) where
import Control.Concurrent
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
import Data.Word
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import Liberty.WebGateway.RandomString
import Liberty.Common.NetworkMessage
import Liberty.Common.Timeouts

type SessionId = Text
type SequenceNumber = Word32
type InSequence = SequenceNumber
type OutSequence = SequenceNumber
data SessionData = SessionData {
  sdProxySocket :: Maybe Socket,
  sdLastInSequence :: InSequence,
  sdLastOutSequence :: OutSequence,
  sdMessagesWaiting :: [(OutSequence, Message)],
  sdLongPollRequestAbortTVar :: TVar Bool,
  sdSessionTimeoutAbortTVar :: TVar Bool
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
      (sessionId, sessionDataTVar) <- tryCreateSessionUntilSuccess sessionMapTVar proxySocket
      _ <- forkIO $ proxySocketReaderLoop proxySocket LBS.empty sessionDataTVar
      return $ Just sessionId
    Nothing -> do
      putStrLn "Failed to establish proxy connection -- session will not be issued"
      return Nothing

-- TODO: Make sure that sClose by another thread will cause recv to return blank as this function relies on that for cleanup
proxySocketReaderLoop :: Socket -> ByteString -> SessionDataTVar -> IO ()
proxySocketReaderLoop proxySocket buffer sessionDataTVar =
  finally
    (do
      case parseMessage buffer of
        Just (maybeMessage, newBuffer) ->
          case maybeMessage of
            Just message -> do
              putStrLn $ "Received message from proxy socket: " ++ show message
              handleReceivedProxyMessage message sessionDataTVar
              proxySocketReaderLoop proxySocket newBuffer sessionDataTVar
            Nothing -> do
              putStrLn "No valid message in current buffer yet, so blocking on recv..."
              maybeReceivedData <- catch (do
                recvResult <- recv proxySocket 2048
                if not $ LBS.null recvResult then do
                  return $ Just recvResult
                else do
                  return Nothing
                )
                (\(SomeException ex) -> do
                  putStrLn $ "Proxy connection closing due to exception: " ++ show ex
                  return Nothing
                )

              case maybeReceivedData of
                Just receivedData ->
                  -- now that we've received some data, loop around and try parsing it
                  proxySocketReaderLoop proxySocket (LBS.append newBuffer receivedData) sessionDataTVar
                Nothing ->
                  putStrLn $ "Proxy connection ending -- recv returned nothing"
    )
    (do
      -- at this point, regardless of how the read was terminated, close the socket
      atomically $ do
        sessionData <- readTVar sessionDataTVar
        writeTVar sessionDataTVar $ sessionData { sdProxySocket = Nothing }
      sClose proxySocket
    )

handleReceivedProxyMessage :: Message -> SessionDataTVar -> IO ()
handleReceivedProxyMessage message sessionDataTVar = do
  -- NOTE: Integer overflow (Word32) after 2^32 messages received from proxySocket
  atomically $ do
    sessionData <- readTVar sessionDataTVar
    let newOutSequence = (sdLastOutSequence sessionData) + 1
    writeTVar sessionDataTVar $ sessionData {
      sdLastOutSequence = newOutSequence,
      sdMessagesWaiting = (sdMessagesWaiting sessionData) ++ [(newOutSequence, message)]
    }

tryCreateSessionUntilSuccess :: SessionMapTVar -> Socket -> IO (SessionId, SessionDataTVar)
tryCreateSessionUntilSuccess sessionMapTVar proxySocket = do
  newSessionId <- getRandomText128
  maybeSessionDataTVar <- atomically $ do
    sessionMap <- readTVar sessionMapTVar
    case Map.lookup newSessionId sessionMap of
      Just _ -> return Nothing
      Nothing -> do
        -- initially, no long poll request
        longPollRequestAbortTVar <- newTVar True
        -- TODO: set session timeout here
        -- initially, no session timeout
        sessionTimeoutAbortTVar <- newTVar True
        newSessionDataTVar <- newTVar $ SessionData (Just proxySocket) 0 0 [] longPollRequestAbortTVar sessionTimeoutAbortTVar
        writeTVar sessionMapTVar $ Map.insert newSessionId newSessionDataTVar sessionMap
        return $ Just newSessionDataTVar

  case maybeSessionDataTVar of
    Just sessionDataTVar -> return (newSessionId, sessionDataTVar)
    Nothing -> tryCreateSessionUntilSuccess sessionMapTVar proxySocket

establishProxyConnection :: IO (Maybe Socket)
establishProxyConnection = catch
  (do
    connectionSocket <- socket AF_INET Stream 0
    hostAddr <- inet_addr "127.0.0.1"
    connect connectionSocket (SockAddrInet 9801 hostAddr)
    return $ Just connectionSocket
  )
  (\(SomeException e) -> return Nothing)

{-
setSessionTimeout :: SessionMapTVar -> SessionId -> SessionDataTVar -> IO (TVar Bool)
setSessionTimeout sessionMapTVar sessionId sessionDataTVar =
  let
    timeoutAction = do
      putStrLn "Session cleanup triggered (TODO)"
      return ()
  in
    return $ setTimeout 10 timeoutAction

updateSessionCleanupTimer :: Int -> SessionDataTVar -> SessionMapTVar -> TVar Bool
updateSessionCleanupTimer numSeconds sessionDataTVar sessionMapTVar = do
  newLongPollStatusChildTVar <- atomically $ do
    -- read the session data
    sessionData <- readTVar sessionDataTVar
    -- read the current long poll status child tvar
    currentLongPollStatusChildTVar <- readTVar (sdLongPollStatusParentTVar sessionData)
    -- write False to the child, signalling that the old request has been replaced by this one
    writeTVar currentLongPollStatusChildTVar False
    -- create a new child tvar which will be associated with this request
    newLongPollStatusChildTVar' <- newTVar True
    -- write it to the parent tvar
    writeTVar (sdLongPollStatusParentTVar sessionData) newLongPollStatusChildTVar'
    return newLongPollStatusChildTVar'

  return newLongPollStatusChildTVar
-}

