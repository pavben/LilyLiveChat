module Liberty.WebGateway.ConnectionDispatcher (
  runConnectionDispatcher
) where
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as C8
import Network.Socket
import Prelude hiding (catch)
import qualified Text.Regex.PCRE.ByteString.Lazy as PCRE
import Liberty.WebGateway.Connection
import Liberty.WebGateway.Sessions

runConnectionDispatcher :: SessionMapTVar -> IO ()
runConnectionDispatcher sessionMapTVar = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          httpRegex <- getHttpRegex
          hostAddress <- inet_addr "192.168.1.102"
          initializeListenerSocket listenerSocket hostAddress 9802
          acceptLoop listenerSocket sessionMapTVar httpRegex
        )
        (sClose listenerSocket) -- close the listener socket regardless of exception being raised
      )
      (\ex -> handleException ex)
    Left ex -> handleException ex
  where
    handleException :: SomeException -> IO ()
    handleException ex = do
      putStrLn $ "Error in listen/bind/accept/inet_addr/getHttpRegex: " ++ show ex
      putStrLn "Retrying in 5 seconds..."
      -- on failure, wait and try binding again
      threadDelay (5000 * 1000)
      runConnectionDispatcher sessionMapTVar

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> HostAddress -> PortNumber -> IO ()
initializeListenerSocket listenerSocket hostAddress portNumber = do
  putStrLn $ "Initializing connection listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber hostAddress
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> SessionMapTVar -> PCRE.Regex -> IO ()
acceptLoop listenerSocket sessionMapTVar httpRegex = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  putStrLn $ "Client connected with address: " ++ show clientSockAddr
  _ <- forkIO $ processConnection clientSocket sessionMapTVar httpRegex
  -- and loop around
  acceptLoop listenerSocket sessionMapTVar httpRegex

getHttpRegex :: IO (PCRE.Regex)
getHttpRegex = do
  compileResult <- PCRE.compile PCRE.compDotAll PCRE.execBlank (C8.pack "^(.*?) (.*?) HTTP.*?[Cc][Oo][Nn][Tt][Ee][Nn][Tt]-[Ll][Ee][Nn][Gg][Tt][Hh]:\\W*?(\\d+)\\W*?\r\n.*?\r\n\r\n")
  case compileResult of
    Right regex -> return regex
    Left _ -> error "Error compiling HTTP RegEx"

