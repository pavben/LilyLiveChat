module Liberty.SiteDataService.ClientDispatcher (
  runClientDispatcher
) where
import Control.Concurrent
import Control.Exception
import qualified Network.BSD as BSD
import Network.Socket
import Prelude hiding (catch)
import Liberty.SiteDataService.Client
import Liberty.SiteDataService.DatabaseManager
import Liberty.Common.Utils

runClientDispatcher :: DatabaseHandleTVar -> IO ()
runClientDispatcher databaseHandleTVar = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          hostEntry <- BSD.getHostByName $ getLocalServiceHost "sds"
          initializeListenerSocket listenerSocket (BSD.hostAddress hostEntry) 9800
          acceptLoop listenerSocket databaseHandleTVar
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
      runClientDispatcher databaseHandleTVar

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> HostAddress -> PortNumber -> IO ()
initializeListenerSocket listenerSocket hostAddress portNumber = do
  putStrLn $ "Initializing client listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber hostAddress
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> DatabaseHandleTVar -> IO ()
acceptLoop listenerSocket databaseHandleTVar = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  putStrLn $ "Client connected with address: " ++ show clientSockAddr
  _ <- forkIO $ initializeClient clientSocket databaseHandleTVar
  -- and loop around
  acceptLoop listenerSocket databaseHandleTVar

