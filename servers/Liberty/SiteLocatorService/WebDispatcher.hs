{-# LANGUAGE ViewPatterns #-}

module Liberty.SiteLocatorService.WebDispatcher (
  runWebDispatcher
) where
import Control.Concurrent
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text.Lazy as LT
import Network.HTTP
import Network.Socket
import Network.URI
import Prelude hiding (catch)
import Safe
import Liberty.SiteLocatorService.SiteMap

runWebDispatcher :: SiteMapTVar -> IO ()
runWebDispatcher siteMapTVar = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          hostAddress <- inet_addr "192.168.1.100"
          initializeListenerSocket listenerSocket hostAddress 9700
          acceptLoop listenerSocket siteMapTVar
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
      runWebDispatcher siteMapTVar

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> HostAddress -> PortNumber -> IO ()
initializeListenerSocket listenerSocket hostAddress portNumber = do
  putStrLn $ "Initializing web client listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber hostAddress
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> SiteMapTVar -> IO ()
acceptLoop listenerSocket siteMapTVar = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  _ <- forkIO $ do
    putStrLn $ "Web client connected with address: " ++ show clientSockAddr
    handleStream <- socketConnection "" 0 clientSocket
    receiveHttpRequestLoop handleStream siteMapTVar
    close handleStream
  -- and loop around
  acceptLoop listenerSocket siteMapTVar

receiveHttpRequestLoop :: HandleStream ByteString -> SiteMapTVar -> IO ()
receiveHttpRequestLoop handleStream siteMapTVar = do
  receiveResult <- receiveHTTP handleStream
  case receiveResult of
    Right request -> do
      putStrLn "Request:"
      print request
      let requestUriPath = C8.pack $ uriPath $ rqURI request
      case headMay $ filter (not . LBS.null) $ C8.split '/' $ requestUriPath of
        Just (LT.pack . C8.unpack -> siteId) -> do
          maybeServerId <- lookupServerForSite siteId siteMapTVar
          case maybeServerId of
            Just serverId -> do
              let requestUriQuery = C8.pack (uriQuery (rqURI request))
              let targetUrl = LBS.concat [C8.pack "https://", C8.pack $ LT.unpack serverId, C8.pack ".lilylivechat.net", requestUriPath, requestUriQuery]
              respondHTTP handleStream (Response (3,0,2) "Found" [mkHeader HdrLocation $ C8.unpack targetUrl] $ redirectBody targetUrl)
            Nothing -> do
              putStrLn "No servers available"
              respondHTTP handleStream (Response (2,0,0) "OK" [] noServersAvailableBody)
        Nothing -> do
          putStrLn "Invalid request"
          respondHTTP handleStream (Response (4,0,0) "Bad Request" [] badRequestBody)
    Left connError -> do
      print connError

  where
    redirectBody url = LBS.concat [C8.pack "Redirecting to <a href=\"", url, C8.pack "\">", url, C8.pack "</a>..."]
    noServersAvailableBody = C8.pack "LilyLiveChat is currently unavailable. We'll be back online soon!"
    badRequestBody = C8.pack "You've followed an invalid link."

