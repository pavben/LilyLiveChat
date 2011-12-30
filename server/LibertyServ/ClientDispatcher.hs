module LibertyServ.ClientDispatcher (
  runClientDispatcher
) where
import Control.Concurrent
import Control.Exception
import Network.Socket
import Prelude hiding (catch)
import LibertyServ.Client
import LibertyServ.DatabaseManager
import LibertyServ.SiteMap

runClientDispatcher :: DatabaseHandleTVar -> SiteMapTVar -> IO ()
runClientDispatcher databaseHandleTVar siteMapTVar = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          initializeListenerSocket listenerSocket 9801
          acceptLoop listenerSocket databaseHandleTVar siteMapTVar
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
      runClientDispatcher databaseHandleTVar siteMapTVar

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> PortNumber -> IO ()
initializeListenerSocket listenerSocket portNumber = do
  putStrLn $ "Initializing client listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber iNADDR_ANY
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
acceptLoop listenerSocket databaseHandleTVar siteMapTVar = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  putStrLn $ "Client connected with address: " ++ show clientSockAddr
  _ <- forkIO $ initializeClient clientSocket databaseHandleTVar siteMapTVar
  -- and loop around
  acceptLoop listenerSocket databaseHandleTVar siteMapTVar

