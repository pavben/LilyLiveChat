module Liberty.WebGateway.Connection (
  processConnection
) where
import qualified Codec.Binary.Url as Url
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Lazy.IO as LIO
import Data.List
import Data.Ord
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import qualified Text.JSON as JSON
import qualified Text.Regex.PCRE.ByteString.Lazy as PCRE
import Liberty.Common.NetworkMessage
import Liberty.Common.Timeouts
import Liberty.Common.Utils
import Liberty.WebGateway.Sessions

-- TODO: DoS vulnerability: memory exhaustion by sending tons of random crap to buffer
processConnection :: Socket -> SessionMapTVar -> PCRE.Regex -> IO ()
processConnection clientSocket sessionMapTVar httpRegex =
  finally
    (socketLoop clientSocket sessionMapTVar httpRegex LBS.empty)
    (sClose clientSocket)

socketLoop :: Socket -> SessionMapTVar -> PCRE.Regex -> ByteString -> IO ()
socketLoop clientSocket sessionMapTVar httpRegex buffer =
  catch
    (do
      recvResult <- recv clientSocket 2048
      let newBuffer = LBS.append buffer recvResult
      shouldContinue <- if not $ LBS.null recvResult then do
        print recvResult
        executeResult <- PCRE.regexec httpRegex newBuffer
        case executeResult of
          Right matchResult -> case matchResult of
            Just (_, textMatched, textRemainder, matches) -> do
              putStrLn $ "Matches: " ++ show matches
              putStrLn $ "textMatched: " ++ show textMatched
              putStrLn $ "textRemainder: " ++ show textRemainder
              case matches of
                [requestMethod, requestPath, contentLengthStr] ->
                  if requestMethod == C8.pack "POST" && requestPath == C8.pack "/c" then do
                    case readMaybeInt contentLengthStr of
                      Just contentLength ->
                        if LBS.length textRemainder == fromIntegral contentLength then do
                          -- got all data
                          putStrLn "Got all data"
                          let urlEncodedArgs = map snd $ sortBy (comparing fst) $ map (\s -> (C8.takeWhile (/= '=') s, LBS.drop 1 $ C8.dropWhile (/= '=') s)) $ C8.split '&' textRemainder
                          case mapM convertToLBSMaybe $ map Url.decode $ map C8.unpack urlEncodedArgs of
                            Just rawArgs ->
                              case mapM decodeUtf8Maybe rawArgs of
                                Just textArgs -> processClientRequest textArgs clientSocket sessionMapTVar
                                Nothing -> putStrLn "Error decoding UTF-8 args"
                            Nothing -> putStrLn "Unable to URL-decode args"
                          return False -- just bail and close the connection
                        else do
                          putStrLn "Got the request header, but not all of the data has arrived yet"
                          return True
                      Nothing -> do
                        putStrLn "Client sent invalid content length"
                        return False
                  else do
                    putStrLn "Request method or path do not match"
                    return False
                _ -> error "Unexpected number of HTTP RegEx matches"
            Nothing -> do
              putStrLn "No match yet"
              return True
          Left _ -> do
            putStrLn "Error in exec"
            return False
      else do
        putStrLn $ "Client disconnecting -- recv returned nothing"
        return False

      case shouldContinue of
        True -> socketLoop clientSocket sessionMapTVar httpRegex newBuffer
        False -> return ()
    )
    (\(SomeException ex) -> putStrLn $ "Client disconnecting due to exception: " ++ show ex)
  where
    convertToLBSMaybe maybeUrlDecodedListOfWord8 =
      case maybeUrlDecodedListOfWord8 of
        Just listOfWord8 -> Just $ LBS.pack listOfWord8
        Nothing -> Nothing
    decodeUtf8Maybe s = do
      case LE.decodeUtf8' s of
        Right decodedStr -> Just decodedStr
        Left _ -> Nothing

processClientRequest :: [Text] -> Socket -> SessionMapTVar -> IO ()
processClientRequest texts clientSocket sessionMapTVar =
  case texts of
    sessionId:sequenceNumberT:messageComponents ->
      case parseIntegralCheckBounds sequenceNumberT of
        Just sequenceNumber -> do
          putStr "Session ID: "
          LIO.putStrLn sessionId
          putStr "Sequence number: "
          LIO.putStrLn sequenceNumberT
          if (sessionId == LT.pack "NEW") && (sequenceNumber == 0) && (null messageComponents) then
            handleNewSession sessionMapTVar clientSocket
          else do
            maybeSessionDataTVar <- atomically $ do
              sessionMap <- readTVar sessionMapTVar
              return $ Map.lookup sessionId sessionMap
            case maybeSessionDataTVar of
              Just sessionDataTVar ->
                case messageComponents of
                  messageTypeT:messageTexts ->
                    case parseIntegralCheckBounds messageTypeT of
                      Just messageTypeId ->
                        case messageIdToType messageTypeId of
                          Just messageType -> handleSendCommand messageType messageTexts sessionDataTVar sequenceNumber clientSocket
                          Nothing -> return ()
                      Nothing -> return ()
                  [] -> do
                    putStrLn "Long poll connection"
                    handleLongPoll sessionDataTVar sequenceNumber clientSocket sessionMapTVar sessionId
              Nothing -> do
                putStrLn "Invalid session (not found or expired)"
                return ()
        Nothing -> return ()
    _ -> putStrLn "Invalid message"

handleNewSession :: SessionMapTVar -> Socket -> IO ()
handleNewSession sessionMapTVar clientSocket = do
  putStrLn "Request for a new session"
  createSessionResult <- createSession sessionMapTVar
  case createSessionResult of
    Just (sessionId, sessionDataTVar) -> do
      putStrLn "Created new session"
      sendJsonResponse clientSocket $ JSON.toJSObject [("sessionId", JSON.showJSON $ LE.encodeUtf8 sessionId)]
      -- set the session timeout in case we don't receive any long poll requests
      resetSessionTimeout sessionMapTVar sessionId sessionDataTVar
      return ()
    Nothing -> do
      putStrLn "Failed to create a new session"
      return ()

handleSendCommand :: MessageType -> [Text] -> SessionDataTVar -> InSequence -> Socket -> IO ()
handleSendCommand messageType messageTexts sessionDataTVar inSequence clientSocket = do
  putStr "Msg Type: "
  putStrLn $ show messageType
  putStrLn "Args: "
  mapM_ LIO.putStrLn messageTexts
  -- first, make sure lastInSequence is inSequence - 1
  initialSessionData <- atomically $ readTVar sessionDataTVar
  case sdProxySocket initialSessionData of
    Just proxySocket ->
      if inSequence == (sdLastInSequence initialSessionData) + 1 then
        case createMessage (messageType, messageTexts) of
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
            sendEmptyTextResponse clientSocket
          Nothing -> do
            putStrLn "Failed to encode message"
            return ()
      else do
        -- invalid inSequence
        sClose proxySocket
        atomically $ do
          sessionData <- readTVar sessionDataTVar
          writeTVar sessionDataTVar $ sessionData { sdProxySocket = Nothing }
        return ()
    Nothing -> do
      putStrLn "No proxy socket available."
      return ()

handleLongPoll :: SessionDataTVar -> OutSequence -> Socket -> SessionMapTVar -> SessionId -> IO ()
handleLongPoll sessionDataTVar outSequence clientSocket sessionMapTVar sessionId = do
  (myLongPollAbortTVar, myLongPollTimeoutTVar, previousLongPollAbortTVar, sessionTimeoutAbortTVar) <- atomically $ do
    sessionData <- readTVar sessionDataTVar
    myLongPollAbortTVar' <- newTVar False
    myLongPollTimeoutTVar' <- newTVar False
    writeTVar sessionDataTVar $ sessionData { sdLongPollRequestAbortAndTimeoutTVars = (myLongPollAbortTVar', myLongPollTimeoutTVar') }
    let (previousLongPollAbortTVar', _) = sdLongPollRequestAbortAndTimeoutTVars sessionData
    let sessionTimeoutAbortTVar' = sdSessionTimeoutAbortTVar sessionData
    return (myLongPollAbortTVar', myLongPollTimeoutTVar', previousLongPollAbortTVar', sessionTimeoutAbortTVar')

  -- try to abort, even if there is nothing to
  void $ abortTimeout previousLongPollAbortTVar

  -- we also need to abort the session timeout as we don't want the session cleaned up
  -- note: this aborts the session timeout that was there when we took the snapshot, not
  --   necessarily the current session timeout
  --   this avoids the race condition of aborting a session timeout created by a more recent request
  void $ abortTimeout sessionTimeoutAbortTVar

  -- set the long poll timeout
  setTimeout 30 myLongPollAbortTVar $ do
    putStrLn "Long poll timeout!"
    -- set myLongPollTimeoutTVar to True, signalling that this long poll request has timed out
    atomically $ writeTVar myLongPollTimeoutTVar $ True

  eitherResponse <- atomically $ do
    sessionData <- readTVar sessionDataTVar
    -- filter the list to contain only the messages the client has not yet acknowledged receiving
    let filteredMessageList = filter (\p -> fst p > outSequence) (sdMessagesWaiting sessionData)
        sessionActive =
          case sdProxySocket sessionData of
            Just _ -> True
            Nothing -> False

    abortedFlag <- readTVar myLongPollAbortTVar
    timeoutFlag <- readTVar myLongPollTimeoutTVar

    case (filteredMessageList, sessionActive, abortedFlag, timeoutFlag) of
      -- if this request has been replaced by a newer one
      (_, _, True, _) -> return $ Left False -- indicate that the request was aborted by a newer request
      -- if this request has timed out (and was not aborted)
      (_, _, False, True) -> return $ Left True -- indicate that the request was aborted by a timeout
      -- if there is nothing to send, wait until there is
      ([], True, False, False) -> retry
      -- and in all other cases, there is something to send (the list or sessionActive or both)
      _ -> do
        writeTVar sessionDataTVar $ sessionData { sdMessagesWaiting = [] }
        return $ Right (filteredMessageList, sessionActive)

  case eitherResponse of
    Right (filteredMessageList, sessionActive) -> do
      sendLongPollJsonResponse clientSocket filteredMessageList sessionActive
      putStrLn "Long poll response sent (if socket was open)"
      -- set a new session cleanup timeout
      resetSessionTimeout sessionMapTVar sessionId sessionDataTVar
    Left True -> do
      putStrLn "Long poll aborted by timeout"
      -- set a new session cleanup timeout, because this session has timed out and was not replaced by a newer one which would have to set the session timeout on its' exit
      resetSessionTimeout sessionMapTVar sessionId sessionDataTVar
    Left False -> do
      putStrLn "Long poll aborted without timeout set"
      return ()

  return ()

sendLongPollJsonResponse :: Socket -> [(OutSequence, Message)] -> Bool -> IO ()
sendLongPollJsonResponse clientSocket messagesAndSequences sessionActive =
  let
    messageAndSequenceToByteString (outSequence, (messageType, messageTexts)) =
      JSON.showJSON outSequence : JSON.showJSON (messageTypeToId messageType) : map (JSON.showJSON . LE.encodeUtf8) messageTexts
    arrayOfMessageArrays = map messageAndSequenceToByteString messagesAndSequences
    objectData = m ++ sessionEnded
    m = [("m", JSON.showJSONs arrayOfMessageArrays)]
    sessionEnded = if sessionActive then [] else [("sessionEnded", JSON.showJSON True)]
    jsObject = JSON.toJSObject objectData
  in do
    print $ JSON.encode jsObject
    sendJsonResponse clientSocket jsObject
    return ()

sendJsonResponse :: JSON.JSON a => Socket -> JSON.JSObject a -> IO ()
sendJsonResponse clientSocket jsObject = do
  let encodedData = C8.pack $ JSON.encode jsObject
  --let encodedData = C8.pack $ "{\"m\":[1,3,\"Joe\"]}"
  print $ encodedData
  sendAllIgnoreExceptions clientSocket $ LBS.concat [C8.pack
    (
      "HTTP/1.1 200 OK" ++
      "Server: Liberty.WebGateway\r\n" ++
      "Access-Control-Allow-Origin: *\r\n" ++
      "Content-Length: " ++ show (LBS.length encodedData) ++ "\r\n" ++
      "Content-Type: application/json\r\n\r\n"
    ),
    encodedData]

sendEmptyTextResponse :: Socket -> IO ()
sendEmptyTextResponse clientSocket = do
  sendAllIgnoreExceptions clientSocket $ C8.pack (
      "HTTP/1.1 200 OK" ++
      "Server: Liberty.WebGateway\r\n" ++
      "Access-Control-Allow-Origin: *\r\n" ++
      "Content-Length: 0\r\n" ++
      "Content-Type: text/plain\r\n\r\n"
    )

sendAllIgnoreExceptions :: Socket -> ByteString -> IO ()
sendAllIgnoreExceptions s byteString =
  catch (sendAll s byteString) (\(SomeException _) -> return ())

readMaybeInt :: ByteString -> Maybe Int
readMaybeInt s = do
  case C8.readInt s of
    Just (num, remainder) ->
      if LBS.null remainder then
        Just num
      else
        Nothing
    Nothing -> Nothing

