module Liberty.WebGateway.Connection (
  processConnection
) where
import qualified Codec.Binary.Url as Url
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import qualified Text.Regex.PCRE.ByteString.Lazy as PCRE

-- TODO: DoS vulnerability: memory exhaustion by sending tons of random crap to buffer
processConnection :: Socket -> PCRE.Regex -> IO ()
processConnection clientSocket httpRegex =
  finally
    (socketLoop clientSocket httpRegex LBS.empty)
    (sClose clientSocket)

socketLoop :: Socket -> PCRE.Regex -> ByteString -> IO ()
socketLoop clientSocket httpRegex buffer =
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
        True -> socketLoop clientSocket httpRegex newBuffer
        False -> return ()
    )
    (\(SomeException ex) -> putStrLn $ "Client disconnecting due to exception: " ++ show ex)

readMaybeInt :: ByteString -> Maybe Int
readMaybeInt s = do
  case C8.readInt s of
    Just (num, remainder) ->
      if LBS.null remainder then
        Just num
      else
        Nothing
    Nothing -> Nothing

