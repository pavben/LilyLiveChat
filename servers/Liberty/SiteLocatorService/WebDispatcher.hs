module Liberty.SiteLocatorService.WebDispatcher (
  runWebDispatcher
) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Network.HTTP
import Network.Socket
import Network.URI
import Prelude hiding (catch)
import Safe
import Liberty.Common.Utils

runWebDispatcher :: IO ()
runWebDispatcher = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          hostAddress <- inet_addr "192.168.1.100"
          initializeListenerSocket listenerSocket hostAddress 9800
          acceptLoop listenerSocket
        )
        (sClose listenerSocket) -- close the listener socket regardless of exception being raised
      )
      (\ex -> handleException ex)
    Left ex -> handleException ex
  where
    handleException :: SomeException -> IO ()
    handleException ex = do
      putStrLn $ "Error in listen/bind/accept: " ++ show ex
      putStrLn "Retrying in 5 seconds..."
      -- on failure, wait and try binding again
      threadDelay (5000 * 1000)
      runWebDispatcher

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> HostAddress -> PortNumber -> IO ()
initializeListenerSocket listenerSocket hostAddress portNumber = do
  putStrLn $ "Initializing client listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber hostAddress
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> IO ()
acceptLoop listenerSocket = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  putStrLn $ "Web client connected with address: " ++ show clientSockAddr
  handleStream <- socketConnection "" 0 clientSocket
  _ <- forkIO $ do
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
      case headMay $ filter (not . LBS.null) $ C8.split '/' $ requestUriPath of
        Just siteId -> do
          let targetServer = C8.pack "anivia"
          let requestUriQuery = C8.pack (uriQuery (rqURI request))
          let targetUrl = LBS.concat [C8.pack "http://", targetServer, C8.pack ".lilylivechat.net", requestUriPath, requestUriQuery]
          respondHTTP handleStream (Response (3,0,2) "Found" [mkHeader HdrLocation $ C8.unpack targetUrl] $ redirectBody targetUrl)
        Nothing -> do
          putStrLn "Invalid request"
          respondHTTP handleStream (Response (4,0,0) "Bad Request" [] badRequestBody)
    Left connError -> do
      print connError

  where
    redirectBody url = LBS.concat [C8.pack "Redirecting to <a href=\"", url, C8.pack "\">", url, C8.pack "</a>..."]
    badRequestBody = C8.pack "You've followed an invalid link."

