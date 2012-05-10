{-# LANGUAGE ScopedTypeVariables #-}

module Main
where
import Control.Monad
import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Lazy.IO as LTI
import Data.Word
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Liberty.Common.NetworkMessage

main :: IO ()
main = do
  s <- socket AF_INET Stream 0
  hostAddr <- inet_addr "127.0.0.1"
  connect s (SockAddrInet 9800 hostAddr)
  putStrLn "Connected"
  case createMessage (CustomerJoinMessage, [LE.decodeUtf8 (LC8.pack "1"), LE.decodeUtf8 (LC8.pack "Angry Bear"), LE.decodeUtf8 (LC8.pack "#111111"), LE.decodeUtf8 (LC8.pack "images/angry-bear.png")]) of
    Just x -> sendAll s x

