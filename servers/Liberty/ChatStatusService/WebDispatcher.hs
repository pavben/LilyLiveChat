{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Liberty.ChatStatusService.WebDispatcher (
  runWebDispatcher
) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Network.BSD as BSD
import Network.HTTP
import Network.Socket
import Network.URI
import Prelude hiding (catch)
import Liberty.Common.Messages
import Liberty.Common.Messages.ChatServer
import Liberty.Common.Messages.SiteLocatorService
import Liberty.Common.ServiceClient
import Liberty.Common.Utils

runWebDispatcher :: IO ()
runWebDispatcher = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          hostEntry <- BSD.getHostByName $ getLocalServiceHost "css"
          initializeListenerSocket listenerSocket (BSD.hostAddress hostEntry) 9700
          acceptLoop listenerSocket
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
      runWebDispatcher

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> HostAddress -> PortNumber -> IO ()
initializeListenerSocket listenerSocket hostAddress portNumber = do
  putStrLn $ "Initializing web listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber hostAddress
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> IO ()
acceptLoop listenerSocket = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  _ <- forkIO $ do
    putStrLn $ "Web client connected with address: " ++ show clientSockAddr
    handleStream <- socketConnection "" 0 clientSocket
    receiveHttpRequestLoop handleStream
    close handleStream
  -- and loop around
  acceptLoop listenerSocket

receiveHttpRequestLoop :: HandleStream ByteString -> IO ()
receiveHttpRequestLoop handleStream = do
  receiveResult <- receiveHTTP handleStream
  case receiveResult of
    Right request -> do
      putStrLn "Request:"
      print request
      let requestUriPath = C8.pack $ uriPath $ rqURI request
      let maybeCurrentPage = fmap LT.pack $ lookupHeader HdrReferer (rqHeaders request)
      case filter (not . LBS.null) $ C8.split '/' $ requestUriPath of
        [C8.unpack -> "chatstatus", LT.pack . C8.unpack -> siteId, LT.pack . C8.unpack -> visitorId] -> handleChatStatusRequest siteId visitorId maybeCurrentPage handleStream
        _ -> do
          putStrLn "Invalid request"
          respondHTTP handleStream (Response (4,0,0) "Bad Request" [] badRequestBody)
    Left connError -> do
      print connError

  where
    badRequestBody = C8.pack "You've followed an invalid link."

handleChatStatusRequest :: Text -> Text -> Maybe Text -> HandleStream ByteString -> IO ()
handleChatStatusRequest siteId visitorId maybeCurrentPage handleStream = do
  siteActive <- do
    -- locate the server that the site is on
    siteLocateResult <- locateSite siteId
    case siteLocateResult of
      SLSuccess serverId -> do
        putStrLn $ "Site located on server: " ++ LT.unpack serverId
        -- get ServiceConnectionData from the server name
        let chatServerConnectionData = getServiceConnectionDataForChatServer serverId
        serviceCallResult <- withServiceConnection chatServerConnectionData $ \serviceHandle -> do
          -- before checking the status, notify the chat server about this visitor navigating to the new page
          case maybeCurrentPage of
            Just currentPage -> sendMessageToService CSMTVisitorOnPage (visitorId, currentPage) serviceHandle
            Nothing -> return ()

          -- select the site
          sendMessageToService UnregisteredSelectSiteMessage (siteId) serviceHandle

          selectSiteResponse <- receiveMessageFromService serviceHandle
          case selectSiteResponse of
            (UnregisteredSiteSelectedMessage, unpackMessage -> Just (_ :: Text, True, _ :: Bool)) -> do
              -- site is active
              return True
            _ ->
              -- either invalid message format, invalid site id, unexpected message, or site not active
              return False
        
        case serviceCallResult of
          Just True -> return True
          _ -> return False
      SLNotAvailable -> do
        putStrLn "Site Locator Service not available"
        return False

  sendTextResponse handleStream $ C8.pack $ case siteActive of
    True -> "1"
    False -> "0" -- either site not active or service request error or some other error

sendTextResponse :: HandleStream ByteString -> ByteString -> IO ()
sendTextResponse handleStream byteString =
  let
    headers = [
      mkHeader HdrContentType "text/plain",
      mkHeader HdrContentLength $ show $ LBS.length byteString,
      mkHeader (HdrCustom "Access-Control-Allow-Origin") "*"
      ]
  in
    respondHTTP handleStream $ Response (2,0,0) "OK" headers byteString

