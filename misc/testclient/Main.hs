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

type MessageType = Word8

main :: IO ()
main = do
  s <- socket AF_INET Stream 0
  hostAddr <- inet_addr "127.0.0.1"
  connect s (SockAddrInet 9801 hostAddr)
  putStrLn "Connected"
  case createMessage 1 [LE.decodeUtf8 (LC8.pack "2"), LE.decodeUtf8 (LC8.pack "Angry Bear"), LE.decodeUtf8 (LC8.pack "#111111")] of
    Just x -> sendAll s x

createMessage :: MessageType -> [Text] -> Maybe ByteString
createMessage messageType params = case sequence $ map textToBytestringWithLen params of
  Just encodedParams -> Just $ LBS.concat [LBS.singleton messageType, LBS.concat encodedParams, LBS.replicate 4 0]
  Nothing -> Nothing

textToBytestringWithLen :: Text -> Maybe ByteString
textToBytestringWithLen text =
  case textToBytestringAndLen text of
    Just (bytestring, len :: Word32) -> Just $ runPut $ do
      putWord32be len
      putLazyByteString bytestring
    Nothing -> Nothing

textToBytestringAndLen :: (Integral a, Bounded a) => Text -> Maybe (ByteString, a)
textToBytestringAndLen text =
  let
    bytestring = LE.encodeUtf8 text
    maybeLen = fromIntegralCheckBounds $ LBS.length bytestring
  in
    case maybeLen of
      Just len -> Just (bytestring, len)
      Nothing -> Nothing

fromIntegralCheckBounds :: (Integral a, Integral b, Bounded b) => a -> Maybe b
fromIntegralCheckBounds x | toInteger (maxBound `asTypeOf` i) < toInteger x = Nothing
                    | toInteger (minBound `asTypeOf` i) > toInteger x = Nothing
                    | otherwise = Just i
  where
    i = fromIntegral x

