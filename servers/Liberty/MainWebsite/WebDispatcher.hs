{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Liberty.MainWebsite.WebDispatcher (
  runWebDispatcher
) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as J
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text.Lazy as LT
import qualified Network.BSD as BSD
import Network.HTTP
import Network.Socket
import Network.URI
import Prelude hiding (catch)
import System.Random
import Liberty.Common.Messages.ChatServer
import Liberty.Common.Messages.SiteLocatorService
import Liberty.Common.ServiceClient
import Liberty.Common.Utils

runWebDispatcher :: IO ()
runWebDispatcher = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          hostEntry <- BSD.getHostByName $ getLocalServiceHost "mainweb"
          initializeListenerSocket listenerSocket (BSD.hostAddress hostEntry) 9700
          acceptLoop listenerSocket
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
  adminPassword <- liftM (LT.pack . show) $ randomRIO (10 ^ (6 :: Int) :: Integer, 10 ^ (10 :: Int))
  -- the retry is only for the case where we generate a site id that is already in use
  -- all other cases will not retry
  -- TODO PL: should still return a response, even if this fails
  _ <- runWithRetry 5 $ do
    -- generate a site id
    --siteId <- liftM (LT.pack . show) $ randomRIO (0 :: Integer, 5)
    siteId <- liftM (LT.pack . show) $ randomRIO (0 :: Integer, 2 ^ (16 :: Int))

    -- locate the server that the site should be currently placed on
    siteLocateResult <- locateSite siteId
    case siteLocateResult of
      SLSuccess serverId -> do
        putStrLn $ "Site located on server: " ++ LT.unpack serverId
        -- get ServiceConnectionData from the server name
        let chatServerConnectionData = getServiceConnectionDataForChatServer serverId
        serviceCallResult <- withServiceConnection chatServerConnectionData $ \serviceHandle -> do
          -- login as super admin
          sendMessageToService CSSALoginRequestMessage () serviceHandle

          loginResponse <- receiveMessageFromService serviceHandle
          case loginResponse of
            (CSSALoginSuccessMessage,_) -> do
              -- execute the create site command
              sendMessageToService CSSASiteCreateMessage (
                siteId,
                LT.append "Site " siteId,
                LT.empty, -- by default, no e-mail
                adminPassword) serviceHandle

              createResponse <- receiveMessageFromService serviceHandle
              case createResponse of
                (CSSASiteCreateSuccessMessage,_) -> do
                  liftIO $ sendJsonResponse handleStream $ J.object [
                    ("siteId", J.toJSON siteId),
                    ("adminPassword", J.toJSON adminPassword)
                    ]
                  return True
                (CSSASiteCreateDuplicateIdMessage,_) -> do
                  liftIO $ putStrLn "Generated site id is not unique."
                  return False
                (CSSASiteCreateUnavailableMessage,_) -> do
                  liftIO $ putStrLn "Chat Server cannot process the site creation at this time"
                  return True
                _ -> do
                  liftIO $ putStrLn "Unknown message received from Chat Server"
                  return True
            _ ->
              -- anything else is a failure
              return True

        case serviceCallResult of
          Just ret -> return ret
          Nothing -> return True
      SLNotAvailable -> do
        putStrLn "Site Locator Service not available"
        return True
  return ()

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

