module Liberty.Common.ServiceClient(
  ServiceConnectionData(..),
  getLocalServiceConnectionData,
  ServiceTask(..),
  runServiceTask,
  serviceRequest,
  withServiceConnection,
  establishServiceConnection,
  sendMessageToService,
  receiveMessageFromService
) where
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.MessagePack as MP
import Network.BSD (getHostByName, hostAddress)
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import Liberty.Common.Messages

-- TODO: Make it IPv6 instead
data ServiceConnectionData = ServiceConnectionData {
  sdcHost :: String
}

getLocalServiceConnectionData :: String -> ServiceConnectionData
getLocalServiceConnectionData serviceName =
  let
    hostName = serviceName ++ ".local.lilylivechat.net"
  in
    ServiceConnectionData hostName

data ServiceTask a = ServiceTask (IO (Maybe a))

instance Monad ServiceTask where
  -- >>= :: ServiceTask x -> (x -> ServiceTask y) -> ServiceTask y
  -- a :: IO (Maybe x)
  -- b :: x -> ServiceTask y
  -- the 'case' statement is :: IO (Maybe y)
  ServiceTask a >>= b = ServiceTask $ a >>= \maybeX -> case maybeX of
    Just x -> runServiceTask (b x)
    Nothing -> return Nothing

  return a = ServiceTask (return (Just a))
  fail _ = ServiceTask (return Nothing)

instance MonadIO ServiceTask where
  -- ioA :: IO a
  liftIO ioA = ServiceTask $ ioA >>= \a -> return $ Just a

runServiceTask :: ServiceTask a -> IO (Maybe a)
runServiceTask (ServiceTask ioMaybeA) = ioMaybeA

catchST :: Exception e => ServiceTask a -> (e -> ServiceTask a) -> ServiceTask a
catchST tryThis handleError = ServiceTask $ liftIO $ catch (runServiceTask tryThis) (\e -> runServiceTask $ handleError e)

finallyST :: ServiceTask a -> ServiceTask b -> ServiceTask a
finallyST a b = ServiceTask $ liftIO $ finally (runServiceTask a) (runServiceTask b)

type ServiceHandle = Socket

-- TODO: timeout
serviceRequest :: (MessageType a, MP.Packable b) => ServiceConnectionData -> a -> b -> IO (Maybe (a, ByteString))
serviceRequest serviceConnectionData messageType messageParams =
  withServiceConnection serviceConnectionData $ \serviceHandle -> do
    sendMessageToService messageType messageParams serviceHandle

    receiveResult <- receiveMessageFromService serviceHandle
    return receiveResult -- either Just (a, ByteString) or Nothing on failure

withServiceConnection :: ServiceConnectionData -> (ServiceHandle -> ServiceTask a) -> IO (Maybe a)
withServiceConnection serviceConnectionData f = runServiceTask $ do
  serviceSocket <- establishServiceConnectionST serviceConnectionData
  liftIO $ putStrLn "Conn established"
  finallyST
    (f serviceSocket)
    (liftIO $ sClose serviceSocket)

establishServiceConnection :: ServiceConnectionData -> IO (Maybe Socket)
establishServiceConnection serviceConnectionData = do
  socketInitResult <- try (socket AF_INET Stream 0)
  case socketInitResult of
    Right serviceSocket ->
      catch
        (do
          hostEntry <- getHostByName (sdcHost serviceConnectionData)
          connect serviceSocket (SockAddrInet 9800 (hostAddress hostEntry))
          return $ Just serviceSocket
        )
        (\(SomeException ex) -> do
          putStrLn $ "Resolve/Connect exception: " ++ show ex
          return Nothing
        )
    Left (SomeException _) -> return Nothing

establishServiceConnectionST :: ServiceConnectionData -> ServiceTask Socket
establishServiceConnectionST serviceConnectionData = do
  socketInitResult <- liftIO $ try (socket AF_INET Stream 0)
  case socketInitResult of
    Right serviceSocket ->
      catchST
        (do
          hostEntry <- liftIO $ getHostByName (sdcHost serviceConnectionData)
          liftIO $ connect serviceSocket (SockAddrInet 9800 (hostAddress hostEntry))
          return serviceSocket
        )
        (\(SomeException ex) -> do
          liftIO $ putStrLn $ "Resolve/Connect exception: " ++ show ex
          fail ""
        )
    Left (SomeException _) -> fail ""

sendMessageToService :: (MessageType a, MP.Packable b) => a -> b -> Socket -> ServiceTask ()
sendMessageToService messageType messageParams serviceSocket =
  case createMessage messageType messageParams of
    Just encodedMessage -> do
      catchST
        (do
          liftIO $ putStrLn $ "Encoded msg: " ++ show encodedMessage
          liftIO $ sendAll serviceSocket encodedMessage
        )
        (\(SomeException ex) -> do
          fail $ "Exception on sendAll: " ++ show ex
        )
    Nothing -> fail "createMessage failed"

receiveMessageFromService :: MessageType a => Socket -> ServiceTask (a, ByteString)
receiveMessageFromService serviceSocket =
  let
    readLoop buffer =
      case parseMessage buffer of
        Just (maybeMessage, newBuffer) ->
          case maybeMessage of
            Just message -> return message
            Nothing -> do
              catchST
                (do
                  recvResult <- liftIO $ recv serviceSocket 2048
                  if not $ LBS.null recvResult then do
                    -- now that we've received some data, loop around and try parsing it
                    readLoop (LBS.append newBuffer recvResult)
                  else
                    fail "recv returned nothing"
                )
                (\(SomeException ex) -> do
                  liftIO $ putStrLn $ "receiveMessageFromService failing due to exception: " ++ show ex
                  fail ""
                )
        Nothing -> do
          liftIO $ putStrLn "receiveMessageFromService failing due to a protocol violation"
          fail ""
  in
    readLoop LBS.empty

