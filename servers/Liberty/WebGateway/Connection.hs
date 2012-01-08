module Liberty.WebGateway.Connection (
  processConnection
) where
import qualified Codec.Binary.Url as Url
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Text.Lazy.Read as LTR
import Data.List
import Data.Ord
import Data.Word
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import qualified Text.JSON as JSON
import qualified Text.Regex.PCRE.ByteString.Lazy as PCRE
import Liberty.Common.NetworkMessage
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
                [requestMethod, requestPath, contentLengthStr] -> do
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
    sessionId:inSequenceT:messageComponents ->
      case parseIntegralCheckBounds inSequenceT of
        Just inSequence -> do
          putStr "Session ID: "
          LIO.putStrLn sessionId
          putStr "In Sequence: "
          LIO.putStrLn inSequenceT
          if (sessionId == LT.pack "NEW") && (inSequence == 0) && (null messageComponents) then
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
                          Just messageType -> handleSendCommand messageType messageTexts sessionDataTVar inSequence clientSocket
                          Nothing -> return ()
                      Nothing -> return ()
                  [] -> do
                    putStrLn "Long poll connection"
                    return () -- TODO
              Nothing -> do
                putStrLn "Invalid session (not found or expired)"
                return ()
        Nothing -> return ()
    _ -> putStrLn "Invalid message"

handleNewSession :: SessionMapTVar -> Socket -> IO ()
handleNewSession sessionMapTVar clientSocket = do
  putStrLn "Request for a new session"
  maybeSessionId <- createSession sessionMapTVar clientSocket
  case maybeSessionId of
    Just sessionId -> do
      putStrLn "Created new session"
      sendJsonResponse clientSocket $ JSON.toJSObject [("sessionId", JSON.showJSON $ LE.encodeUtf8 sessionId)]
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
              (\(SomeException ex) -> do
                putStrLn ("Exception during send to proxy socket: " ++ show ex)
                sClose proxySocket
                return False)

            case sendSuccess of
              True ->
                -- if written successfully, update lastInSequence
                atomically $ do
                  sessionData <- readTVar sessionDataTVar
                  writeTVar sessionDataTVar $ sessionData { sdLastInSequence = inSequence }
              False -> do
                -- set proxySocket to Nothing to indicate that this connection is dead
                atomically $ do
                  sessionData <- readTVar sessionDataTVar
                  writeTVar sessionDataTVar $ sessionData { sdProxySocket = Nothing }
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
  sendEmptyResponse clientSocket

sendJsonResponse :: JSON.JSON a => Socket -> JSON.JSObject a -> IO ()
sendJsonResponse clientSocket jsObject = do
  let encodedData = C8.pack $ JSON.encode jsObject
  sendAll clientSocket $ LBS.concat [
    C8.pack (
      "HTTP/1.1 200 OK" ++
      "Server: Liberty.WebGateway\r\n" ++
      "Access-Control-Allow-Origin: *\r\n" ++
      "Content-Length: " ++ show (LBS.length encodedData) ++ "\r\n" ++
      "Content-Type: application/json\r\n\r\n"
    ),
    encodedData]

sendEmptyResponse :: Socket -> IO ()
sendEmptyResponse clientSocket = do
  sendAll clientSocket $ C8.pack (
      "HTTP/1.1 200 OK" ++
      "Server: Liberty.WebGateway\r\n" ++
      "Access-Control-Allow-Origin: *\r\n" ++
      "Content-Length: 0\r\n" ++
      "Content-Type: application/json\r\n\r\n"
    )

readMaybeInt :: ByteString -> Maybe Int
readMaybeInt s = do
  case C8.readInt s of
    Just (num, remainder) ->
      if LBS.null remainder then
        Just num
      else
        Nothing
    Nothing -> Nothing

