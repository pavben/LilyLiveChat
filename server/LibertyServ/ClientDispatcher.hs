module LibertyServ.ClientDispatcher (
  runClientDispatcher
) where
import Control.Concurrent
import Network.Socket
import LibertyServ.Client

runClientDispatcher :: IO ()
runClientDispatcher = do
  listenerSocket <- initializeListenerSocket 9801
  acceptLoop listenerSocket

initializeListenerSocket :: PortNumber -> IO Socket
initializeListenerSocket portNumber = do
  -- TODO: This can throw an exception
  putStrLn $ "Initializing client listener socket on port " ++ show portNumber
  listenerSocket <- socket AF_INET Stream 0
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber iNADDR_ANY
  listen listenerSocket 1000
  return listenerSocket

acceptLoop :: Socket -> IO ()
acceptLoop listenerSocket = do
  -- TODO: Handle exceptions
  (clientSocket, clientSockAddr) <- accept listenerSocket
  putStrLn $ "Client connected with address: " ++ show clientSockAddr
  _ <- forkIO $ initializeClient clientSocket
  -- and loop around
  acceptLoop listenerSocket

