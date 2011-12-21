module LibertyServ.ClientDispatcher (
  runClientDispatcher
) where
import Network.Socket

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
  (clientSocket, clientSockAddr) <- accept listenerSocket
  --_ <- forkIO $ initializeClient clientSocket
  sClose clientSocket
