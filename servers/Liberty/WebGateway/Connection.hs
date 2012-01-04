module Liberty.WebGateway.Connection (
  processConnection
) where
import qualified Codec.Binary.Url as Url
import Data.Array
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import qualified Text.Regex.PCRE.ByteString.Lazy as PCRE

processConnection :: Socket -> IO ()
processConnection socket = do
  somedataX <- recv socket 2048
  print somedataX
  --let somedataX = C8.pack $ "POST /q HTTP/1.1\r\nContent-Type: blah\r\n\r\nmessageTypeId=1&a=te%20st\r\n"
  compileResult <- PCRE.compile PCRE.compDotAll PCRE.execBlank (C8.pack "^(.*?) /(.*?) HTTP.*?\r\n\r\n(.*?)\r\n$")
  case compileResult of
    Right regex -> do
      executeResult <- PCRE.regexec regex somedataX
      case executeResult of
        Right matchResult -> case matchResult of
          Just (_, _, _, matches) -> print matches
          Nothing -> putStrLn "No match!"
        Left _ -> putStrLn "Error in exec"
    Left _ -> putStrLn "Error in compile"
  sClose socket

