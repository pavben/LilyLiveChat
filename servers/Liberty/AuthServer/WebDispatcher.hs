{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Liberty.AuthServer.WebDispatcher (
  runWebDispatcher
) where
import Control.Concurrent
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Network.BSD as BSD
import Network.HTTP
import qualified Network.HTTP.Conduit as NHC
import Network.Socket
import Network.URI
import Prelude hiding (catch)
import Web.Authenticate.OpenId
import qualified Web.Authenticate.OpenId.Providers as OpenIdProviders
import Liberty.Common.RandomString
import Liberty.Common.Utils
import Liberty.AuthServer.DatabaseManager
import Liberty.AuthServer.SessionMap
import Liberty.AuthServer.Types

runWebDispatcher :: SessionMapTVar -> DatabaseHandleTVar -> IO ()
runWebDispatcher sessionMapTVar databaseHandleTVar = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          hostEntry <- BSD.getHostByName $ getLocalServiceHost "auth"
          initializeListenerSocket listenerSocket (BSD.hostAddress hostEntry) 9700
          acceptLoop listenerSocket sessionMapTVar databaseHandleTVar
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
      runWebDispatcher sessionMapTVar databaseHandleTVar

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> HostAddress -> PortNumber -> IO ()
initializeListenerSocket listenerSocket hostAddress portNumber = do
  putStrLn $ "Initializing web client listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber hostAddress
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> SessionMapTVar -> DatabaseHandleTVar -> IO ()
acceptLoop listenerSocket sessionMapTVar databaseHandleTVar = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  _ <- forkIO $ do
    putStrLn $ "Web client connected with address: " ++ show clientSockAddr
    handleStream <- socketConnection "" 0 clientSocket
    receiveHttpRequestLoop handleStream sessionMapTVar databaseHandleTVar
    close handleStream
  -- and loop around
  acceptLoop listenerSocket sessionMapTVar databaseHandleTVar

receiveHttpRequestLoop :: HandleStream ByteString -> SessionMapTVar -> DatabaseHandleTVar -> IO ()
receiveHttpRequestLoop handleStream sessionMapTVar databaseHandleTVar = do
  receiveResult <- receiveHTTP handleStream
  case receiveResult of
    Right request -> do
      putStrLn "Request:"
      print request
      let requestUriPath = C8.pack $ uriPath $ rqURI request
      let requestUriQuery = uriQuery $ rqURI request
      case map C8.unpack $ filter (not . LBS.null) $ C8.split '/' $ requestUriPath of
        "auth" : authArgs -> do
          case authArgs of
            ["google", targetUrl] -> do
              forwardUrl <- NHC.withManager $ \manager -> do
                forwardUrl' <- getForwardUrl (T.pack OpenIdProviders.google) (T.append "https://lilylivechat.net/auth/google-validate?targetUrl=" $ T.pack targetUrl) (Just "https://*.lilylivechat.net") [
                  ("openid.ns.ax", "http://openid.net/srv/ax/1.0"),
                  ("openid.ax.mode", "fetch_request"),
                  ("openid.ax.required", "email"),
                  ("openid.ax.type.email", "http://schema.openid.net/contact/email"),
                  ("openid.ns.ui", "http://specs.openid.net/extensions/ui/1.0"),
                  ("openid.ui.mode", "popup")
                  ] manager
                return $ T.unpack forwardUrl'
              
              putStrLn $ "Redirecting to: " ++ forwardUrl
              respondWithRedirect forwardUrl
            ["google-validate"] ->
              case fmap (map (\(a,b) -> (T.pack $ C8.unpack a, T.pack $ C8.unpack b))) $ parseUriQueryString $ C8.drop 1 $ C8.pack $ requestUriQuery of
                Just pairsToValidate ->
                  case (lookup "targetUrl" pairsToValidate, lookup "openid.ext1.value.email" pairsToValidate) of
                    (Just (LT.fromStrict -> targetUrl), Just (LT.fromStrict -> email)) ->
                      catch
                        (do
                          (LT.fromStrict . identifier -> googleIdentifier, validatedPairs) <- NHC.withManager $ \manager -> authenticate pairsToValidate manager
                          print targetUrl
                          print email
                          print googleIdentifier
                          print validatedPairs
                          getUserDataResult <- getUserIdForGoogleIdentifier googleIdentifier databaseHandleTVar
                          case getUserDataResult of
                            GUDRSuccess userData -> do
                              -- found a matching googleIdentifier
                              -- create a new session
                              sessionId <- createSession (udUserId userData) (udEmail userData) sessionMapTVar
                              putStrLn $ "Created session " ++ (LT.unpack sessionId) ++ " for existing user " ++ (LT.unpack $ udUserId userData)
                              -- send it to the user as a cookie
                              respondWithSetCookieAndRedirect sessionId targetUrl
                            GUDRNotFound -> do
                              -- generate a user id
                              userId <- getRandomAlphanumericText 32
                              -- save the user data to the database
                              userDataSaveResult <- saveUserDataToDb (UserData userId email googleIdentifier) databaseHandleTVar
                              if userDataSaveResult then do
                                -- create a new session
                                sessionId <- createSession userId email sessionMapTVar
                                putStrLn $ "Created session " ++ (LT.unpack sessionId) ++ " for new user " ++ (LT.unpack userId)
                                -- send it to the user as a cookie
                                respondWithSetCookieAndRedirect sessionId targetUrl
                              else do
                                -- this is likely the unlikely case of a duplicate userId; let them retry
                                return ()
                            GUDRNotAvailable -> do
                              -- if the database is down, just close the connection
                              return ()
                        )
                        (\(SomeException e) -> do
                          putStrLn $ "Failed to authenticate: " ++ show e
                          respondWithBadRequest
                        )
                    _ -> do
                      putStrLn "Missing required field(s) in pairsToValidate"
                      respondWithBadRequest
                Nothing -> do
                  putStrLn "Invalid request"
                  respondWithBadRequest
            _ -> do
              putStrLn "Invalid request"
              respondWithBadRequest
        _ -> do
          putStrLn "Invalid request"
          respondHTTP handleStream (Response (4,0,0) "Bad Request" [] badRequestBody)
    Left connError -> do
      print connError

  where
    respondWithSetCookieAndRedirect :: LT.Text -> LT.Text -> IO ()
    respondWithSetCookieAndRedirect (LT.unpack -> sessionId) (LT.unpack -> targetUrl) = respondHTTP handleStream (Response (3,0,2) "Found" [
      mkHeader HdrSetCookie $ "sessionId=" ++ sessionId ++ "; domain=lilylivechat.net; path=/; max-age: " ++ (show (24 * 60 * 60 :: Integer)) ++ ";",
      mkHeader HdrLocation targetUrl
      ] $ redirectBody targetUrl)
    respondWithRedirect :: String -> IO ()
    respondWithRedirect targetUrl = respondHTTP handleStream (Response (3,0,2) "Found" [mkHeader HdrLocation targetUrl] $ redirectBody targetUrl)
    redirectBody url = LBS.concat $ map C8.pack ["Redirecting to <a href=\"", url, "\">", url, "</a>..."]
    respondWithBadRequest = respondHTTP handleStream (Response (4,0,0) "Bad Request" [] badRequestBody)
    badRequestBody = C8.pack "You've followed an invalid link."

