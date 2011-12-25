module LibertyServ.ClientDispatcher (
  runClientDispatcher
) where
import Control.Concurrent
import Network.Socket
import LibertyServ.Client

runClientDispatcher :: IO ()
runClientDispatcher = catch toRun onException
  where
    toRun = do
      listenerSocket <- initializeListenerSocket 9801
      acceptLoop listenerSocket
    onException e = do
      putStrLn $ "Error in listen/bind/accept: " ++ show e
      putStrLn "Retrying in 5 seconds..."
      -- on failure, wait and try binding again
      threadDelay (5000 * 1000)
      runClientDispatcher

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

