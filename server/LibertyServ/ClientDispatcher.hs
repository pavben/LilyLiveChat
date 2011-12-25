module LibertyServ.ClientDispatcher (
  runClientDispatcher
) where
import Control.Concurrent
import Control.Exception
import Network.Socket
import Prelude hiding (catch)
import LibertyServ.Client

runClientDispatcher :: IO ()
runClientDispatcher = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      finally
      (
        catch
        (do
          initializeListenerSocket listenerSocket 9801
          acceptLoop listenerSocket
        )
        (\ex -> handleException ex)
      )
      (sClose listenerSocket) -- close the listener socket regardless of exception being raised
    Left ex -> handleException ex
  where
    handleException ex = do
      let _ = ex :: IOException
      putStrLn $ "Error in listen/bind/accept: " ++ show ex
      putStrLn "Retrying in 5 seconds..."
      -- on failure, wait and try binding again
      threadDelay (5000 * 1000)
      runClientDispatcher

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> PortNumber -> IO ()
initializeListenerSocket listenerSocket portNumber = do
  putStrLn $ "Initializing client listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber iNADDR_ANY
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> IO ()
acceptLoop listenerSocket = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  putStrLn $ "Client connected with address: " ++ show clientSockAddr
  _ <- forkIO $ initializeClient clientSocket
  -- and loop around
  acceptLoop listenerSocket

