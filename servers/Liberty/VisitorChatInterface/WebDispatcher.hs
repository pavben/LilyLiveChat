{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Liberty.VisitorChatInterface.WebDispatcher (
  runWebDispatcher
) where
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.STM
import qualified Data.Aeson as J
import Data.Attoparsec.Number as DAN
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HMS
import Data.List
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Ord
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import qualified Network.BSD as BSD
import Network.HTTP
import Network.Socket
import Network.Socket.ByteString.Lazy (sendAll)
import Prelude hiding (catch)
import Liberty.VisitorChatInterface.Types
import Liberty.VisitorChatInterface.VisitorMap
import Liberty.Common.Messages
import Liberty.Common.Messages.ChatServer
import Liberty.Common.Timeouts
import Liberty.Common.Utils

runWebDispatcher :: VisitorMapTVar -> IO ()
runWebDispatcher visitorMapTVar = do
  eitherListenerSocket <- try $ socket AF_INET Stream 0 -- create the socket
  case eitherListenerSocket of
    Right listenerSocket ->
      catch
      (finally
        (do
          hostEntry <- BSD.getHostByName $ getLocalServiceHost "anivia"
          initializeListenerSocket listenerSocket (BSD.hostAddress hostEntry) 9700
          acceptLoop listenerSocket visitorMapTVar
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
      runWebDispatcher visitorMapTVar

-- Exceptions handled by caller
initializeListenerSocket :: Socket -> HostAddress -> PortNumber -> IO ()
initializeListenerSocket listenerSocket hostAddress portNumber = do
  putStrLn $ "Initializing client listener socket on port " ++ show portNumber
  setSocketOption listenerSocket ReuseAddr 1
  bindSocket listenerSocket $ SockAddrInet portNumber hostAddress
  listen listenerSocket 1000

-- Exceptions handled by caller
acceptLoop :: Socket -> VisitorMapTVar -> IO ()
acceptLoop listenerSocket visitorMapTVar = do
  (clientSocket, clientSockAddr) <- accept listenerSocket
  _ <- forkIO $ do
    putStrLn $ "Web client connected with address: " ++ show clientSockAddr
    handleStream <- socketConnection "" 0 clientSocket
    receiveHttpRequestLoop handleStream visitorMapTVar
    close handleStream
  -- and loop around
  acceptLoop listenerSocket visitorMapTVar

receiveHttpRequestLoop :: HandleStream ByteString -> VisitorMapTVar -> IO ()
receiveHttpRequestLoop handleStream visitorMapTVar = do
  receiveResult <- receiveHTTP handleStream
  case receiveResult of
    Right request -> do
      putStrLn "Request:"
      print request
      print $ rqBody request
      case rqMethod request of
        POST ->
          case J.decode $ rqBody request :: Maybe J.Object of
            Just requestJsonObject -> do
              {- new visitor: nothing required
               - returning visitor new session: v
               - send: v, s, o, m
               - long poll: v, s, i
               -}
              print requestJsonObject
              case (
                HMS.lookup "v" requestJsonObject,
                HMS.lookup "s" requestJsonObject,
                HMS.lookup "o" requestJsonObject,
                HMS.lookup "m" requestJsonObject,
                HMS.lookup "i" requestJsonObject
                ) of
                -- command
                -- Note: Client's outSequence is our inSequence
                (Just (J.String (LT.fromStrict -> visitorId)), Just (J.Number (DAN.I sessionId)), Just (J.Number (DAN.I inSequence)), Just (J.Array messageArray), _) ->
                  return ()
                -- long poll request
                (readMaybeString -> maybeVisitorId, readMaybeInteger -> maybeSessionId, Nothing, Just (J.Array messageArray), Just (J.Number (DAN.I outSequence))) -> do
                  maybeVisitorAndVisitorSession <- do
                    visitorMap <- atomically $ readTVar visitorMapTVar
                    case maybeVisitorId of
                      Just visitorId ->
                        case Map.lookup visitorId visitorMap of
                          Just visitorDataTVar -> do
                            -- TODO ULTRALOW: The above lookup is not atomic. We might end up creating a visitor session for a visitor that's been pulled from the map.
                            (visitorSessionId, visitorSessionDataTVar) <- createVisitorSession visitorDataTVar
                            return $ Just (visitorId, visitorDataTVar, visitorSessionId, visitorSessionDataTVar)
                          Nothing ->
                            -- visitorId provided, is invalid
                            -- in this case, treat is as no visitorId as long as there's also no visitorSessionId
                            case maybeSessionId of
                              Just _ -> return Nothing
                              Nothing -> do
                                (visitorId, visitorDataTVar) <- createVisitor visitorMapTVar
                                (visitorSessionId, visitorSessionDataTVar) <- createVisitorSession visitorDataTVar
                                return $ Just (visitorId, visitorDataTVar, visitorSessionId, visitorSessionDataTVar)
                      Nothing -> do
                        (visitorId, visitorDataTVar) <- createVisitor visitorMapTVar
                        (visitorSessionId, visitorSessionDataTVar) <- createVisitorSession visitorDataTVar
                        return $ Just (visitorId, visitorDataTVar, visitorSessionId, visitorSessionDataTVar)

                  case maybeVisitorAndVisitorSession of
                    Just (visitorId, visitorDataTVar, visitorSessionId, visitorSessionDataTVar) -> return () -- TODO
                    Nothing -> return () -- invalid visitorSessionId
              where
                readMaybeString (Just (J.String (LT.fromStrict -> s))) = Just s
                readMaybeString _ = Nothing
                readMaybeInteger (Just (J.Number (DAN.I i))) = Just i
                readMaybeInteger _ = Nothing
            Nothing -> return () -- cannot decode JSON object in request body
      {-
                -- new session request
                case lookupHeader (HdrCustom "X-Real-IP") $ rqHeaders request of
                  Just (LT.pack -> clientIp) ->
                    handleNewSession sessionMapTVar handleStream clientIp
                  Nothing ->
                    return () -- X-Real-IP header missing
      -}
        OPTIONS ->
          let
            headers = [
              mkHeader (HdrCustom "Access-Control-Allow-Origin") "*",
              mkHeader (HdrCustom "Access-Control-Allow-Methods") "POST",
              mkHeader (HdrCustom "Access-Control-Allow-Headers") "Content-Type",
              mkHeader (HdrCustom "Access-Control-Max-Age") "10"
              ]
          in
            respondHTTP handleStream $ Response (2,0,0) "OK" headers LBS.empty
    Left connError -> do
      print connError

{-
handleNewSession :: SessionMapTVar -> HandleStream ByteString -> Text -> IO ()
handleNewSession sessionMapTVar handleStream clientIp = do
  putStrLn "Request for a new session"
  putStrLn $ "Client's IP is: " ++ LT.unpack clientIp
  createSessionResult <- createSession sessionMapTVar
  case createSessionResult of
    Just (sessionId, sessionDataTVar) -> do
      putStrLn "Created new session"
      sendJsonResponse handleStream $ J.object [("sessionId", J.toJSON sessionId)]
      -- set the session timeout in case we don't receive any long poll requests
      resetSessionTimeout sessionDataTVar sessionId sessionMapTVar
      -- send the client IP to the server
      maybeProxySocket <- atomically $ do
        sessionData <- readTVar sessionDataTVar
        return $ sdProxySocket sessionData
      case maybeProxySocket of
        Just proxySocket ->
          case createMessage CSMTUnregisteredClientIp (clientIp) of
            Just encodedMessage ->
              catch
                (sendAll proxySocket encodedMessage)
                (\(SomeException ex) ->
                  putStrLn ("Exception during send to proxy socket: " ++ show ex)
                )
            Nothing -> do
              putStrLn "Failed to encode message"
              return ()
        Nothing -> do
          putStrLn $ "session was just created, but proxySocket is already Nothing?"
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
            sendJsonResponse handleStream (J.toJSON ())
          Nothing -> do
            putStrLn "Failed to encode message"
            return ()
      else
        -- invalid inSequence
        putStrLn "Closing proxy socket due to invalid inSequence from the client"
    Nothing -> putStrLn "No proxy socket available."

data LongPollWaitResult = LongPollWaitResultAborted | LongPollWaitResultActivity | LongPollWaitResultTimeout | LongPollWaitResultSuccess [(OutSequence, [J.Value])] Bool

handleLongPoll :: SessionDataTVar -> OutSequence -> HandleStream ByteString -> SessionMapTVar -> SessionId -> IO ()
handleLongPoll sessionDataTVar outSequence handleStream sessionMapTVar sessionId = do
  (myLongPollAbortTVar, myLongPollTimeoutTVar, myLongPollActivityTVar, previousLongPollAbortTVar, sessionTimeoutAbortTVar) <- atomically $ do
    sessionData <- readTVar sessionDataTVar
    myLongPollAbortTVar' <- newTVar False
    myLongPollTimeoutTVar' <- newTVar False
    myLongPollActivityTVar' <- newTVar False
    writeTVar sessionDataTVar $ sessionData { sdLongPollRequestAbortAndTimeoutTVars = (myLongPollAbortTVar', myLongPollTimeoutTVar') }
    let (previousLongPollAbortTVar', _) = sdLongPollRequestAbortAndTimeoutTVars sessionData
    let sessionTimeoutAbortTVar' = sdSessionTimeoutAbortTVar sessionData
    return (myLongPollAbortTVar', myLongPollTimeoutTVar', myLongPollActivityTVar', previousLongPollAbortTVar', sessionTimeoutAbortTVar')

  -- try to abort, even if there is nothing to
  void $ abortTimeout previousLongPollAbortTVar

  -- we also need to abort the session timeout as we don't want the session cleaned up
  -- note: this aborts the session timeout that was there when we took the snapshot, not
  --   necessarily the current session timeout
  --   this avoids the race condition of aborting a session timeout created by a more recent request
  void $ abortTimeout sessionTimeoutAbortTVar

  -- set the long poll timeout
  setTimeout 60 myLongPollAbortTVar $ do
    putStrLn "Long poll timeout!"
    -- set myLongPollTimeoutTVar to True, signalling that this long poll request has timed out
    atomically $ writeTVar myLongPollTimeoutTVar True

  -- spawn a thread to set the activity tvar if the connection is closed or sends any extra data
  _ <- forkIO $ setTVarOnConnectionActivity handleStream myLongPollActivityTVar

  waitResult <- atomically $ do
    sessionData <- readTVar sessionDataTVar
    -- filter the list to contain only the messages the client has not yet acknowledged receiving
    let filteredMessageList = filter (\p -> fst p > outSequence) (sdMessagesWaiting sessionData)
        sessionActive = isJust $ sdProxySocket sessionData

    abortedFlag <- readTVar myLongPollAbortTVar
    timeoutFlag <- readTVar myLongPollTimeoutTVar
    activityFlag <- readTVar myLongPollActivityTVar

    case (filteredMessageList, sessionActive, abortedFlag, timeoutFlag, activityFlag) of
      -- if this request has been replaced by a newer one
      (_, _, True, _, _) -> return LongPollWaitResultAborted
      -- if there was activity, we assume the client closed their browser
      (_, _, False, _, True) -> return LongPollWaitResultActivity
      -- if this request has timed out (and was not aborted)
      (_, _, False, True, False) -> return LongPollWaitResultTimeout
      -- if there is nothing to send, wait until there is
      ([], True, False, False, False) -> retry
      -- and in all other cases, there is something to send (the list or sessionActive or both)
      _ -> do
        writeTVar sessionDataTVar $ sessionData { sdMessagesWaiting = [] }
        return $ LongPollWaitResultSuccess filteredMessageList sessionActive

  case waitResult of
    LongPollWaitResultSuccess filteredMessageList sessionActive -> do
      sendLongPollJsonResponse handleStream filteredMessageList sessionActive
      putStrLn "Long poll response sent (if socket was open)"
      -- set a new session cleanup timeout
      resetSessionTimeout sessionDataTVar sessionId sessionMapTVar
    LongPollWaitResultTimeout -> do
      putStrLn "Long poll aborted by timeout"
      -- set a new session cleanup timeout, because this session has timed out and was not replaced by a newer one which would have to set the session timeout on its' exit
      sendLongPollJsonResponse handleStream [] True
      resetSessionTimeout sessionDataTVar sessionId sessionMapTVar
    LongPollWaitResultAborted -> putStrLn "Long poll aborted without timeout set"
    LongPollWaitResultActivity -> do
      putStrLn "Long poll aborted by client socket activity (likely disconnect)"
      deleteSession sessionDataTVar sessionId sessionMapTVar
  return ()
-}

standardHeaders :: [Header]
standardHeaders = [
  mkHeader (HdrCustom "Access-Control-Allow-Origin") "*",
  mkHeader (HdrCustom "Access-Control-Allow-Methods") "POST",
  mkHeader (HdrCustom "Access-Control-Allow-Headers") "Content-Type",
  mkHeader (HdrCustom "Access-Control-Max-Age") "10"
  ]

sendOptionsResponse :: HandleStream ByteString -> IO ()
sendOptionsResponse handleStream =
  let
    headers = standardHeaders
  in
    respondHTTP handleStream $ Response (2,0,0) "OK" headers LBS.empty

sendJsonResponse :: HandleStream ByteString -> J.Value -> IO ()
sendJsonResponse handleStream object =
  let
    encodedObject = J.encode object
    headers = standardHeaders ++ [
      mkHeader HdrContentType "application/json",
      mkHeader HdrContentLength $ show $ LBS.length encodedObject
      ]
  in
    respondHTTP handleStream $ Response (2,0,0) "OK" headers encodedObject

sendLongPollJsonResponse :: HandleStream ByteString -> [(Integer, [J.Value])] -> Bool -> IO ()
sendLongPollJsonResponse handleStream sequencesAndMessages sessionActive =
  let
    sequenceAndMessageToJson (outSequence, jValues) = J.toJSON outSequence : jValues
    arrayOfMessageArrays = map sequenceAndMessageToJson sequencesAndMessages
    m = [("m", J.toJSON arrayOfMessageArrays)]
    sessionEnded = if sessionActive then [] else [("sessionEnded", J.toJSON True)]
    objectData = m ++ sessionEnded
  in
    sendJsonResponse handleStream $ J.object objectData

setTVarOnConnectionActivity :: HandleStream ByteString -> TVar Bool -> IO ()
setTVarOnConnectionActivity handleStream tvar = do
  _ <- readBlock handleStream 1
  putStrLn "setTVarOnConnectionActivity triggered"
  atomically $ writeTVar tvar True
