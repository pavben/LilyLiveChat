module Liberty.SiteLocatorService.ServiceDispatcher (
  runServiceDispatcher
) where
import Control.Concurrent
import Control.Exception
import qualified Network.BSD as BSD
import Network.Socket
import Prelude hiding (catch)
import Liberty.SiteLocatorService.ServiceHandlers
import Liberty.SiteLocatorService.SiteMap
import Liberty.Common.Utils

runServiceDispatcher :: SiteMapTVar -> IO ()
runServiceDispatcher siteMapTVar = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          hostEntry <- BSD.getHostByName $ getLocalServiceHost "sl"
          initializeListenerSocket listenerSocket (BSD.hostAddress hostEntry) 9800
          acceptLoop listenerSocket siteMapTVar
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
      runServiceDispatcher siteMapTVar

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> HostAddress -> PortNumber -> IO ()
initializeListenerSocket listenerSocket hostAddress portNumber = do
  putStrLn $ "Initializing service client listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber hostAddress
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> SiteMapTVar -> IO ()
acceptLoop listenerSocket siteMapTVar = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  putStrLn $ "Client connected with address: " ++ show clientSockAddr
  _ <- forkIO $ initializeClient clientSocket siteMapTVar
  -- and loop around
  acceptLoop listenerSocket siteMapTVar

