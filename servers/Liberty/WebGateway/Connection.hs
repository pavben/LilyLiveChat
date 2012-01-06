module Liberty.WebGateway.Connection (
  processConnection
) where
import qualified Codec.Binary.Url as Url
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Text.Lazy.Read as LTR
import Data.List
import Data.Ord
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import qualified Text.Regex.PCRE.ByteString.Lazy as PCRE
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
                        let urlEncodedArgs = map (\(x, y) -> y) $ sortBy (comparing fst) $ map (\s -> (C8.takeWhile (/= '=') s, LBS.drop 1 $ C8.dropWhile (/= '=') s)) $ C8.split '&' textRemainder
                        case mapM convertToLBSMaybe $ map Url.decode $ map C8.unpack urlEncodedArgs of
                          Just rawArgs ->
                            case mapM decodeUtf8Maybe rawArgs of
                              Just textArgs -> processClientRequest textArgs sessionMapTVar
                              Nothing -> putStrLn "Error decoding UTF-8 args"
                          Nothing -> putStrLn "Unable to URL-decode args"
                        return False
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

processClientRequest :: [Text] -> SessionMapTVar -> IO ()
processClientRequest texts sessionMapTVar =
  case texts of
    [singleText] -> putStrLn "Single text"
    sessionId:messageTypeStr:args -> do
      putStr "Session ID: "
      LIO.putStrLn sessionId
      putStr "Msg Type: "
      LIO.putStrLn messageTypeStr
      putStrLn "Args: "
      mapM_ LIO.putStrLn args
    _ -> putStrLn "Invalid message"

readMaybeInt :: ByteString -> Maybe Int
readMaybeInt s = do
  case C8.readInt s of
    Just (num, remainder) ->
      if LBS.null remainder then
        Just num
      else
        Nothing
    Nothing -> Nothing

