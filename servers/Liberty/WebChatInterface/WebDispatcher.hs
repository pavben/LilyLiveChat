module Liberty.WebChatInterface.WebDispatcher (
  runWebDispatcher
) where
import Control.Arrow (second)
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad.STM
import qualified Data.Aeson as J
import Data.Attoparsec.Number as DAN
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.HashMap.Strict as HMS
import Data.List
import qualified Data.Map as Map
import qualified Data.MessagePack as MP
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Vector as V
import Network.HTTP
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Network.URI
import Prelude hiding (catch)
import Safe
import Liberty.WebChatInterface.MessageFormatConverter
import Liberty.WebChatInterface.Sessions
import Liberty.Common.Messages
import Liberty.Common.Messages.ChatServer
import Liberty.Common.Utils

runWebDispatcher :: SessionMapTVar -> IO ()
runWebDispatcher sessionMapTVar = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          hostAddress <- inet_addr "192.168.1.102"
          initializeListenerSocket listenerSocket hostAddress 9802
          acceptLoop listenerSocket sessionMapTVar
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
      runWebDispatcher sessionMapTVar

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> HostAddress -> PortNumber -> IO ()
initializeListenerSocket listenerSocket hostAddress portNumber = do
  putStrLn $ "Initializing client listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber hostAddress
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> SessionMapTVar -> IO ()
acceptLoop listenerSocket sessionMapTVar = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  _ <- forkIO $ do
    putStrLn $ "Web client connected with address: " ++ show clientSockAddr
    handleStream <- socketConnection "" 0 clientSocket
    receiveHttpRequestLoop handleStream sessionMapTVar
    close handleStream
  -- and loop around
  acceptLoop listenerSocket sessionMapTVar

receiveHttpRequestLoop :: HandleStream ByteString -> SessionMapTVar -> IO ()
receiveHttpRequestLoop handleStream sessionMapTVar = do
  receiveResult <- receiveHTTP handleStream
  case receiveResult of
    Right request -> do
      putStrLn "Request:"
      print request
      print $ rqBody request
      case J.decode $ rqBody request :: Maybe J.Object of
        Just requestJsonObject -> do
          {- new session: s[empty]
           - send: s, o, m
           - long poll: s, i
           -}
          print requestJsonObject
          case HMS.lookup (T.pack "s") requestJsonObject of
            Just (J.String sessionId) ->
              -- successfully read sessionId (potentially blank)
              if not $ T.null sessionId then do
                -- if not blank, locate the session
                -- this is tested after determining the request type, because the Nothing case handling depends on it
                maybeSessionDataTVar <- atomically $ do
                  sessionMap <- readTVar sessionMapTVar
                  return $ Map.lookup (LT.fromStrict sessionId) sessionMap
                -- determine if this request is a send or a long poll
                case (
                  HMS.lookup (T.pack "o") requestJsonObject,
                  HMS.lookup (T.pack "m") requestJsonObject,
                  HMS.lookup (T.pack "i") requestJsonObject
                  ) of
                  (Just (J.Number (DAN.I inSequence)), Just (J.Array messageArray), _) ->
                    -- send request
                    -- Note: Client's outSequence is our inSequence
                    case maybeSessionDataTVar of
                      Just sessionDataTVar -> handleSendCommand (V.toList messageArray) sessionDataTVar (fromIntegral inSequence :: InSequence) handleStream
                      Nothing -> return () -- if invalid session on a send, simply close the connection
                  (_, _, Just (J.Number (DAN.I inSequence))) -> do
                    -- long poll request
                    putStrLn "LPR"
                  _ -> return () -- not a proper send or long poll request
              else
                -- new session request
                handleNewSession sessionMapTVar handleStream
            _ -> return () -- cannot read sessionId
        Nothing -> return () -- cannot decode JSON object in request body
    Left connError -> do
      print connError

  where
    redirectBody url = LBS.concat [C8.pack "Redirecting to <a href=\"", url, C8.pack "\">", url, C8.pack "</a>..."]
    noServersAvailableBody = C8.pack "LilyLiveChat is currently unavailable. We'll be back online soon!"
    badRequestBody = C8.pack "You've followed an invalid link."

handleNewSession :: SessionMapTVar -> HandleStream ByteString -> IO ()
handleNewSession sessionMapTVar handleStream = do
  putStrLn "Request for a new session"
  createSessionResult <- createSession sessionMapTVar
  case createSessionResult of
    Just (sessionId, sessionDataTVar) -> do
      putStrLn "Created new session"
      sendJsonResponse handleStream $ J.object [(T.pack "sessionId", J.toJSON sessionId)]
      -- set the session timeout in case we don't receive any long poll requests
      resetSessionTimeout sessionDataTVar sessionId sessionMapTVar
      return ()
    Nothing -> do
      putStrLn "Failed to create a new session"
      return ()

handleSendCommand :: [J.Value] -> SessionDataTVar -> InSequence -> HandleStream ByteString -> IO ()
handleSendCommand jValues sessionDataTVar inSequence handleStream = do
  -- first, make sure lastInSequence is inSequence - 1
  initialSessionData <- atomically $ readTVar sessionDataTVar
  case sdProxySocket initialSessionData of
    Just proxySocket ->
      if inSequence == (sdLastInSequence initialSessionData) + 1 then do
        print jValues
        case createMessageFromJson jValues of
          Just encodedMessage -> do
            sendSuccess <- catch
              (sendAll proxySocket encodedMessage >> return True)
              (\(SomeException ex) -> putStrLn ("Exception during send to proxy socket: " ++ show ex) >> return False)

            case sendSuccess of
              True ->
                -- if written successfully, update lastInSequence
                atomically $ do
                  sessionData <- readTVar sessionDataTVar
                  writeTVar sessionDataTVar $ sessionData { sdLastInSequence = inSequence }
              False -> do
                -- set proxySocket to Nothing to indicate that this connection is dead
                sClose proxySocket
                atomically $ do
                  sessionData <- readTVar sessionDataTVar
                  writeTVar sessionDataTVar $ sessionData { sdProxySocket = Nothing }
            -- regardless of whether or not the send was successful, acknowledge receipt
            -- TODO: sendEmptyTextResponse clientSocket
          Nothing -> do
            putStrLn "Failed to encode message"
            return ()
      else
        -- invalid inSequence
        putStrLn "Closing proxy socket due to invalid inSequence from the client"
    Nothing -> putStrLn "No proxy socket available."

sendJsonResponse :: HandleStream ByteString -> J.Value -> IO ()
sendJsonResponse handleStream object =
  let
    encodedObject = J.encode object
    headers = [
      mkHeader HdrContentType "application/json",
      mkHeader HdrContentLength $ show $ LBS.length encodedObject, -- TODO: make sure Content-Length is the length in bytes, not UTF-8 characters
      Header (HdrCustom "Access-Control-Allow-Origin") "*"
      ]
  in
    respondHTTP handleStream $ Response (2,0,0) "OK" headers encodedObject

