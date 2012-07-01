module Liberty.VisitorChatInterface.ProxyConnection (
  ProxySendChanMessage(..),
  ProxySendChan,
  initializeProxyConnection,
  createAndSendMessage,
  closeProxySocket
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
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import Liberty.Common.Messages

-- ProxySendChan
data ProxySendChanMessage = SendMessage ByteString | CloseSocket
type ProxySendChan = TChan ProxySendChanMessage

type MessageHandlerFunction a b = a -> ByteString -> ProxySendChan -> b -> IO ()

initializeProxyConnection :: MessageType a => Socket -> MessageHandlerFunction a b -> b -> IO () -> IO ProxySendChan
initializeProxyConnection proxySocket messageHandlerFunction paramsFromMain onCloseCallback = do
  proxySendChan <- atomically $ newTChan
  _ <- forkIO $ do
    finally
      (do
        _ <- forkIO $ proxySocketSendLoop proxySocket proxySendChan
        proxySocketReadLoop proxySocket LBS.empty proxySendChan messageHandlerFunction paramsFromMain
      )
      (do
        atomically $ writeTChan proxySendChan $ CloseSocket
        sClose proxySocket
      )

    onCloseCallback

  return proxySendChan

proxySocketSendLoop :: Socket -> ProxySendChan -> IO ()
proxySocketSendLoop proxySocket proxySendChan = do
  proxySendChanMessage <- atomically $ readTChan proxySendChan
  case proxySendChanMessage of
    SendMessage encodedMessage -> do
      sendSuccess <- catch
        (do
          sendAll proxySocket encodedMessage
          return True
        )
        (\(SomeException ex) -> do
          putStrLn $ "Exception on sendAll: " ++ show ex ++ " -- closing socket."
          return False
        )
      if sendSuccess then
        proxySocketSendLoop proxySocket proxySendChan
      else
        sClose proxySocket
    CloseSocket -> do
      putStrLn "Got CloseSocket message. Closing proxy socket."
      sClose proxySocket

proxySocketReadLoop :: MessageType a => Socket -> ByteString -> ProxySendChan -> MessageHandlerFunction a b -> b -> IO ()
proxySocketReadLoop proxySocket buffer proxySendChan messageHandlerFunction paramsFromMain =
  case parseMessage buffer of
    Just (maybeMessage, newBuffer) ->
      case maybeMessage of
        Just (messageType, encodedParams) -> do
          --putStrLn $ "Msg: " ++ show messageType ++ ", Encoded Params: " ++ show encodedParams
          messageHandlerFunction messageType encodedParams proxySendChan paramsFromMain
          proxySocketReadLoop proxySocket newBuffer proxySendChan messageHandlerFunction paramsFromMain
        Nothing -> do
          putStrLn "No valid message in current buffer yet"
          maybeReceivedData <- catch (do
            recvResult <- recv proxySocket 2048
            if not $ LBS.null recvResult then do
              return $ Just recvResult
            else do
              return Nothing
            )
            (\(SomeException ex) -> do
              putStrLn $ "Proxy disconnecting due to exception: " ++ show ex
              return Nothing
            )

          case maybeReceivedData of
            Just receivedData ->
              -- now that we've received some data, loop around and try parsing it
              proxySocketReadLoop proxySocket (LBS.append newBuffer receivedData) proxySendChan messageHandlerFunction paramsFromMain
            Nothing ->
              putStrLn $ "Proxy disconnecting -- recv returned nothing"
    Nothing ->
      putStrLn "Proxy disconnecting due to a protocol violation"

createAndSendMessage :: (MessageType a, MP.Packable b) => a -> b -> ProxySendChan -> STM ()
createAndSendMessage messageType params proxySendChan =
  case createMessage messageType params of
    Just encodedMessage -> do
      writeTChan proxySendChan $ SendMessage encodedMessage
    Nothing -> return ()

closeProxySocket :: ProxySendChan -> STM ()
closeProxySocket proxySendChan =
  writeTChan proxySendChan CloseSocket

