module Liberty.ChatServer.ClientDispatcher (
  runClientDispatcher
) where
import Control.Concurrent
import Control.Exception
import qualified Network.BSD as BSD
import Network.Socket
import Prelude hiding (catch)
import Liberty.ChatServer.Client
import Liberty.ChatServer.SiteMap
import Liberty.ChatServer.Types
import Liberty.ChatServer.VisitorClientMap
import Liberty.Common.Utils

runClientDispatcher :: SiteMapTVar -> SiteDataSaverChan -> VisitorClientMapTVar -> IO ()
runClientDispatcher siteMapTVar siteDataSaverChan visitorClientMapTVar = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          hostEntry <- BSD.getHostByName $ getLocalServiceHost "anivia"
          initializeListenerSocket listenerSocket (BSD.hostAddress hostEntry) 9800
          acceptLoop listenerSocket siteMapTVar siteDataSaverChan visitorClientMapTVar
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
      runClientDispatcher siteMapTVar siteDataSaverChan visitorClientMapTVar

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> HostAddress -> PortNumber -> IO ()
initializeListenerSocket listenerSocket hostAddress portNumber = do
  putStrLn $ "Initializing client listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber hostAddress
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> SiteMapTVar -> SiteDataSaverChan -> VisitorClientMapTVar -> IO ()
acceptLoop listenerSocket siteMapTVar siteDataSaverChan visitorClientMapTVar = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  putStrLn $ "Client connected with address: " ++ show clientSockAddr
  _ <- forkIO $ initializeClient clientSocket siteMapTVar siteDataSaverChan visitorClientMapTVar
  -- and loop around
  acceptLoop listenerSocket siteMapTVar siteDataSaverChan visitorClientMapTVar

