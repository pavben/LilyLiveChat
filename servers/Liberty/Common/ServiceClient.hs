module Liberty.Common.ServiceClient(
  ServiceConnectionData(..),
  serviceRequest
) where
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.STM
import qualified Data.Aeson as J
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.MessagePack as MP
import Data.Text.Lazy (Text)
import Data.Word
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import Liberty.Common.Messages
import Liberty.Common.Timeouts
import Debug.Trace

-- TODO: Make it IPv6 instead
data ServiceConnectionData a = ServiceConnectionData {
  sdcHost :: String,
  sdcPort :: PortNumber
}

-- TODO: timeout
serviceRequest :: (MessageType a, MP.Packable b) => ServiceConnectionData a -> a -> b -> IO (Maybe (a, ByteString))
serviceRequest serviceConnectionData messageType messageParams = do
  maybeServiceSocket <- establishConnection (sdcHost serviceConnectionData) (sdcPort serviceConnectionData)
  case maybeServiceSocket of
    Just serviceSocket -> do
      putStrLn "Conn established"
      finally
        (do
          sendResult <- createAndSendMessage messageType messageParams serviceSocket
          case sendResult of
            True -> do
              receiveResult <- receiveOneMessage serviceSocket
              return receiveResult -- either Just (a, ByteString) or Nothing on failure
            False -> return Nothing
        )
        (sClose serviceSocket)
    Nothing -> do
      putStrLn "Could not establish service connection"
      return Nothing

establishConnection :: String -> PortNumber -> IO (Maybe Socket)
establishConnection host port = do
  socketInitResult <- try (socket AF_INET Stream 0)
  case socketInitResult of
    Right serviceSocket ->
      catch
        (do
          hostAddress <- inet_addr host
          connect serviceSocket (SockAddrInet port hostAddress)
          return $ Just serviceSocket
        )
        (\(SomeException ex) -> do
          putStrLn $ "Connect exception: " ++ show ex
          return Nothing)
    Left (SomeException _) -> return Nothing

createAndSendMessage :: (MessageType a, MP.Packable b) => a -> b -> Socket -> IO Bool
createAndSendMessage messageType messageParams serviceSocket =
  case createMessage messageType messageParams of
    Just encodedMessage -> do
      catch
        (do
          putStrLn $ "Encoded msg: " ++ show encodedMessage
          sendAll serviceSocket encodedMessage
          return True
        )
        (\(SomeException ex) -> do
          putStrLn $ "Exception on sendAll: " ++ show ex
          return False
        )
    Nothing -> return False

receiveOneMessage :: MessageType a => Socket -> IO (Maybe (a, ByteString))
receiveOneMessage serviceSocket =
  let
    readLoop buffer =
      case parseMessage buffer of
        Just (maybeMessage, newBuffer) ->
          case maybeMessage of
            Just message -> do
              --putStrLn $ "Msg: " ++ show message
              return $ Just message
            Nothing -> do
              putStrLn "No valid message in current buffer yet"
              maybeReceivedData <- catch (do
                recvResult <- recv serviceSocket 2048
                if not $ LBS.null recvResult then do
                  return $ Just recvResult
                else do
                  return Nothing
                )
                (\(SomeException ex) -> do
                  putStrLn $ "receiveOneMessage failing due to exception: " ++ show ex
                  return Nothing
                )

              case maybeReceivedData of
                Just receivedData ->
                  -- now that we've received some data, loop around and try parsing it
                  readLoop (LBS.append newBuffer receivedData)
                Nothing -> return Nothing -- failed to receive
        Nothing -> do
          putStrLn "receiveOneMessage failing due to a protocol violation"
          return Nothing
  in
    readLoop LBS.empty

