module Liberty.Common.ServiceServer(
  ClientSendChanMessage(..),
  ClientSendChan,
  runServiceDispatcher
) where
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List
import qualified Data.MessagePack as MP
import qualified Network.BSD as BSD
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import Liberty.Common.Messages

-- ClientSendChan
data ClientSendChanMessage = SendMessage ByteString | CloseSocket
type ClientSendChan = TChan ClientSendChanMessage

type MessageHandlerFunction a b = a -> ByteString -> ClientSendChan -> b -> IO ()

runServiceDispatcher :: MessageType a => String -> MessageHandlerFunction a b -> b -> IO ()
runServiceDispatcher localServiceHost messageHandlerFunction paramsFromMain = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          hostEntry <- BSD.getHostByName localServiceHost
          initializeListenerSocket listenerSocket (BSD.hostAddress hostEntry) 9800
          acceptLoop listenerSocket messageHandlerFunction paramsFromMain
        )
        (sClose listenerSocket) -- close the listener socket regardless of exception being raised
      )
      (\ex -> handleException ex)
    Left ex -> handleException ex
  where
    handleException :: SomeException -> IO ()
    handleException ex = do
      putStrLn $ "Error in resolve/listen/bind/accept: " ++ show ex
      putStrLn "Retrying in 5 seconds..."
      -- on failure, wait and try binding again
      threadDelay (5000 * 1000)
      runServiceDispatcher localServiceHost messageHandlerFunction paramsFromMain

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> HostAddress -> PortNumber -> IO ()
initializeListenerSocket listenerSocket hostAddress portNumber = do
  putStrLn $ "Initializing service client listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber hostAddress
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: MessageType a => Socket -> MessageHandlerFunction a b -> b -> IO ()
acceptLoop listenerSocket messageHandlerFunction paramsFromMain = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  putStrLn $ "Client connected with address: " ++ show clientSockAddr
  _ <- forkIO $ initializeClient clientSocket messageHandlerFunction paramsFromMain
  -- and loop around
  acceptLoop listenerSocket messageHandlerFunction paramsFromMain

initializeClient :: MessageType a => Socket -> MessageHandlerFunction a b -> b -> IO ()
initializeClient clientSocket messageHandlerFunction paramsFromMain = do
  clientSendChan <- atomically $ newTChan
  finally
    (do
      _ <- forkIO $ clientSocketSendLoop clientSocket clientSendChan
      clientSocketReadLoop clientSocket LBS.empty clientSendChan messageHandlerFunction paramsFromMain
    )
    (do
      atomically $ writeTChan clientSendChan $ CloseSocket
      sClose clientSocket
    )

clientSocketSendLoop :: Socket -> ClientSendChan -> IO ()
clientSocketSendLoop clientSocket clientSendChan = do
  clientSendChanMessage <- atomically $ readTChan clientSendChan
  case clientSendChanMessage of
    SendMessage encodedMessage -> do
      sendSuccess <- catch
        (do
          sendAll clientSocket encodedMessage
          return True
        )
        (\(SomeException ex) -> do
          putStrLn $ "Exception on sendAll: " ++ show ex ++ " -- closing socket."
          return False
        )
      if sendSuccess then
        clientSocketSendLoop clientSocket clientSendChan
      else
        sClose clientSocket
    CloseSocket -> do
      putStrLn "Got CloseSocket message. Closing client socket."
      sClose clientSocket

clientSocketReadLoop :: MessageType a => Socket -> ByteString -> ClientSendChan -> MessageHandlerFunction a b -> b -> IO ()
clientSocketReadLoop clientSocket buffer clientSendChan messageHandlerFunction paramsFromMain =
  case parseMessage buffer of
    Just (maybeMessage, newBuffer) ->
      case maybeMessage of
        Just (messageType, encodedParams) -> do
          --putStrLn $ "Msg: " ++ show messageType ++ ", Encoded Params: " ++ show encodedParams
          messageHandlerFunction messageType encodedParams clientSendChan paramsFromMain
          clientSocketReadLoop clientSocket newBuffer clientSendChan messageHandlerFunction paramsFromMain
        Nothing -> do
          putStrLn "No valid message in current buffer yet"
          maybeReceivedData <- catch (do
            recvResult <- recv clientSocket 2048
            if not $ LBS.null recvResult then do
              return $ Just recvResult
            else do
              return Nothing
            )
            (\(SomeException ex) -> do
              putStrLn $ "Client disconnecting due to exception: " ++ show ex
              return Nothing
            )

          case maybeReceivedData of
            Just receivedData ->
              -- now that we've received some data, loop around and try parsing it
              clientSocketReadLoop clientSocket (LBS.append newBuffer receivedData) clientSendChan messageHandlerFunction paramsFromMain
            Nothing ->
              putStrLn $ "Client disconnecting -- recv returned nothing"
    Nothing ->
      putStrLn "Client disconnecting due to a protocol violation"

createAndSendMessage :: (MessageType a, MP.Packable b) => a -> b -> ClientSendChan -> STM ()
createAndSendMessage messageType params clientSendChan =
  case createMessage messageType params of
    Just encodedMessage -> do
      writeTChan clientSendChan $ SendMessage encodedMessage
    Nothing -> return ()

closeClientSocket :: ClientSendChan -> STM ()
closeClientSocket clientSendChan =
  writeTChan clientSendChan CloseSocket

