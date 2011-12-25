module LibertyServ.Client (
  initializeClient
) where
import Control.Exception
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Lazy.IO as LTI
import Data.Word
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)

type MessageType = Word8

initializeClient :: Socket -> IO ()
initializeClient clientSocket = do
  putStrLn "client init"
  clientSocketLoop clientSocket

clientSocketLoop :: Socket -> IO ()
clientSocketLoop clientSocket = do
  recvResult <- recv clientSocket 2048
  putStrLn $ "Len: " ++ show (LBS.length recvResult)
  putStrLn $ show $ parseMessage recvResult
  --clientSocketLoop clientSocket

parseMessage :: ByteString -> Maybe (MessageType, [Text])
parseMessage message =
  let
    f = do
      -- TODO: exceptions
      maybeMessageType <- safeGet 1 getWord8
      case maybeMessageType of
        Just messageType -> do
          maybeTexts <- readTexts
          case maybeTexts of
            Just texts -> return $ Just (messageType, texts)
            Nothing -> return Nothing
        Nothing -> return Nothing
  in runGet f message

readTexts :: Get (Maybe [Text])
readTexts = do
  maybeTexts <- readChunks []
  case maybeTexts of
    Just texts -> return $ Just $ reverse $ texts
    Nothing -> return Nothing

readChunks :: [Text] -> Get (Maybe [Text])
readChunks texts = do
  nextChunk <- readNextChunk
  case nextChunk of
    Chunk text -> readChunks (text : texts)
    NothingToRead -> return $ Just texts
    InvalidInput -> return Nothing

data ReadNextChunk = Chunk Text | NothingToRead | InvalidInput

readNextChunk :: Get (ReadNextChunk)
readNextChunk = do
  maybeChunkLength <- safeGet 4 getWord32be
  case maybeChunkLength of
    Just 0 -> return NothingToRead
    Just chunkLength -> do
      let chunkLengthAsInt64 = fromIntegral chunkLength
      maybeByteString <- safeGet chunkLengthAsInt64 $ getLazyByteString chunkLengthAsInt64
      case maybeByteString of
        Just byteString -> case LE.decodeUtf8' byteString of
          Right text -> return $ Chunk text
          Left _ -> return InvalidInput
        Nothing -> return InvalidInput
    Nothing -> return InvalidInput

safeGet bytesRequired getFunction = do
  r <- remaining
  if r >= bytesRequired then do
    v <- getFunction
    return $ Just v
  else
    return Nothing

fromIntegralCheckBounds :: (Integral a, Integral b, Bounded b) => a -> Maybe b
fromIntegralCheckBounds x | toInteger (maxBound `asTypeOf` i) < toInteger x = Nothing
                    | toInteger (minBound `asTypeOf` i) > toInteger x = Nothing
                    | otherwise = Just i
  where
    i = fromIntegral x

