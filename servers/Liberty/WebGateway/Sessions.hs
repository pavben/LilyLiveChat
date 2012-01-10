module Liberty.WebGateway.Sessions (
  SessionId,
  SequenceNumber,
  InSequence,
  OutSequence,
  SessionData(..),
  SessionDataTVar,
  SessionMapTVar,
  createSessionMapTVar,
  createSession,
  resetSessionTimeout
) where
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import Data.Word
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv)
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

createSession :: SessionMapTVar -> IO (Maybe SessionId)
createSession sessionMapTVar = do
  maybeProxySocket <- establishProxyConnection
  case maybeProxySocket of
    Just proxySocket -> do
      (sessionId, sessionDataTVar) <- tryCreateSessionUntilSuccess sessionMapTVar proxySocket
      _ <- forkIO $ proxySocketReaderLoop proxySocket LBS.empty sessionDataTVar
      return $ Just sessionId
    Nothing -> do
      putStrLn "Failed to establish proxy connection -- session will not be issued"
      return Nothing

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
                  putStrLn $ "Proxy connection ending -- recv returned nothing"
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
                Nothing -> return ()
        Nothing ->
          putStrLn "Proxy connection ending due to a protocol violation"
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
        -- initially, no session timeout
        sessionTimeoutAbortTVar <- newTVar True
        newSessionDataTVar <- newTVar $ SessionData (Just proxySocket) 0 0 [] longPollRequestAbortTVar sessionTimeoutAbortTVar
        writeTVar sessionMapTVar $ Map.insert newSessionId newSessionDataTVar sessionMap
        return $ Just newSessionDataTVar

  case maybeSessionDataTVar of
    Just sessionDataTVar -> do
      -- now that we have a sessionDataTVar, we can start the session timeout
      resetSessionTimeout sessionMapTVar newSessionId sessionDataTVar
      return (newSessionId, sessionDataTVar)
    Nothing -> tryCreateSessionUntilSuccess sessionMapTVar proxySocket

establishProxyConnection :: IO (Maybe Socket)
establishProxyConnection = do
  socketInitResult <- try (socket AF_INET Stream 0)
  case socketInitResult of
    Right proxySocket ->
      catch
        (do
          hostAddr <- inet_addr "127.0.0.1"
          connect proxySocket (SockAddrInet 9801 hostAddr)
          return $ Just proxySocket
        )
        (\(SomeException _) -> return Nothing)
    Left (SomeException _) -> return Nothing

resetSessionTimeout :: SessionMapTVar -> SessionId -> SessionDataTVar -> IO ()
resetSessionTimeout sessionMapTVar sessionId sessionDataTVar = do
  (oldSessionTimeoutAbortTVar, newSessionTimeoutAbortTVar) <- atomically $ do
    sessionData <- readTVar sessionDataTVar
    let oldSessionTimeoutAbortTVar = sdSessionTimeoutAbortTVar sessionData
    newSessionTimeoutAbortTVar' <- newTVar False
    writeTVar sessionDataTVar $ sessionData { sdSessionTimeoutAbortTVar = newSessionTimeoutAbortTVar' }
    return (oldSessionTimeoutAbortTVar, newSessionTimeoutAbortTVar')

  -- abort the old timeout
  void $ abortTimeout oldSessionTimeoutAbortTVar

  -- create the new timeout
  setTimeout 10 newSessionTimeoutAbortTVar $ do
    putStrLn "Session cleanup triggered"
    maybeProxySocket <- atomically $ do
      sessionData <- readTVar sessionDataTVar
      let maybeProxySocket' = sdProxySocket sessionData

      -- remove the session from the session map
      sessionMap <- readTVar sessionMapTVar
      writeTVar sessionMapTVar $ Map.delete sessionId sessionMap
      
      -- return the proxy socket so that it can be closed
      return maybeProxySocket'

    -- close the proxy socket, if any
    case maybeProxySocket of
      Just proxySocket -> sClose proxySocket
      Nothing -> return ()

    return ()
