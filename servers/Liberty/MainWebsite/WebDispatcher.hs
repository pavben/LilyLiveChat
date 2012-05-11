{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Liberty.MainWebsite.WebDispatcher (
  runWebDispatcher
) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Aeson as J
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text.Lazy as LT
import Network.HTTP
import Network.Socket
import Network.URI
import Prelude hiding (catch)
import System.Random
import Liberty.Common.Messages.ChatServer
import Liberty.Common.Messages.SiteLocatorService
import Liberty.Common.Utils

runWebDispatcher :: IO ()
runWebDispatcher = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          hostAddress <- inet_addr "192.168.1.103"
          initializeListenerSocket listenerSocket hostAddress 9700
          acceptLoop listenerSocket
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
      runWebDispatcher

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> HostAddress -> PortNumber -> IO ()
initializeListenerSocket listenerSocket hostAddress portNumber = do
  putStrLn $ "Initializing client listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber hostAddress
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> IO ()
acceptLoop listenerSocket = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  _ <- forkIO $ do
    putStrLn $ "Web client connected with address: " ++ show clientSockAddr
    handleStream <- socketConnection "" 0 clientSocket
    receiveHttpRequestLoop handleStream
    close handleStream
  -- and loop around
  acceptLoop listenerSocket

receiveHttpRequestLoop :: HandleStream ByteString -> IO ()
receiveHttpRequestLoop handleStream = do
  receiveResult <- receiveHTTP handleStream
  case receiveResult of
    Right request -> do
      putStrLn "Request:"
      print request
      let requestUriPath = C8.pack $ uriPath $ rqURI request
      case filter (not . LBS.null) $ C8.split '/' $ requestUriPath of
        [C8.unpack -> "cmd", C8.unpack -> command] -> case command of
          "createsite" -> handleCreateSiteCommand handleStream
          _ -> do
            putStrLn "Invalid request"
            respondHTTP handleStream (Response (4,0,0) "Bad Request" [] badRequestBody)
        _ -> do
          putStrLn "Invalid request"
          respondHTTP handleStream (Response (4,0,0) "Bad Request" [] badRequestBody)
    Left connError -> do
      print connError

  where
    badRequestBody = C8.pack "You've followed an invalid link."

handleCreateSiteCommand :: HandleStream ByteString -> IO ()
handleCreateSiteCommand handleStream = do
  -- the retry is only for the case where we generate a site id that is already in use
  -- all other cases will not retry
  runResult <- runWithRetry 5 $ do
    -- generate a site id
    newSiteId <- liftM (LT.pack . show) $ randomRIO (0 :: Integer, 2 ^ (16 :: Int))

    -- locate the server that the site should be currently placed on
    siteLocateResult <- locateSite newSiteId
    case siteLocateResult of
      SLSuccess serverId -> do
        -- get ServiceConnectionData from the server name
        maybeChatServerConnectionData <- getServiceConnectionDataForChatServer serverId
        case maybeChatServerConnectionData of
          Just chatServerConnectionData -> do
            -- connect to chat server

            -- login as super admin

            -- execute the create site command

            -- on success, return True; otherwise, False to retry
            
            -- remember to close the connection
            putStrLn "blah"
            return True
          Nothing -> do
            putStrLn "Unable to resolve chat server IP"
            return True
      SLNotAvailable -> do
        putStrLn "Site Locator Service not available"
        return True

  --unless runResult $ sendJsonResponse handleStream $ J.object [("sessionId", J.toJSON newSiteId)]
  unless runResult $ sendJsonResponse handleStream $ J.object [("success", J.toJSON False)]

sendJsonResponse :: HandleStream ByteString -> J.Value -> IO ()
sendJsonResponse handleStream object =
  let
    encodedObject = J.encode object
    headers = [
      mkHeader HdrContentType "application/json",
      mkHeader HdrContentLength $ show $ LBS.length encodedObject
      ]
  in
    respondHTTP handleStream $ Response (2,0,0) "OK" headers encodedObject

