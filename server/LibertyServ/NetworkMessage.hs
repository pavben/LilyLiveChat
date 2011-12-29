{-# LANGUAGE ScopedTypeVariables #-}

module LibertyServ.NetworkMessage (
  MessageType(..),
  messageTypeToId,
  messageIdToType,
  createMessage,
  parseMessage
) where
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as LE
import Data.Word
import LibertyServ.Utils

-- common
data MessageType = GuestJoinMessage

messageTypeToId :: MessageType -> Word8
messageTypeToId messageType = case messageType of
  GuestJoinMessage -> 1

messageIdToType :: Word8 -> Maybe MessageType
messageIdToType messageId = case messageId of
  1 -> Just GuestJoinMessage
  _ -> Nothing

-- createMessage and dependencies
createMessage :: MessageType -> [Text] -> Maybe ByteString
createMessage messageType params = case sequence $ map textToBytestringWithLen params of
  Just encodedParams -> Just $ LBS.concat [LBS.singleton (messageTypeToId messageType), LBS.concat encodedParams, LBS.replicate 4 0]
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

-- parseMessage and dependencies
data ReadNextChunk = Chunk Text | NothingToRead | InvalidInput

-- Exceptions handled by caller
parseMessage :: ByteString -> (Maybe (Word8, [Text]), ByteString)
parseMessage buffer =
  let
    f = do
      maybeMessageTypeId <- safeGet 1 getWord8
      case maybeMessageTypeId of
        Just messageTypeId -> do
          maybeTexts <- readTexts
          case maybeTexts of
            Just texts -> bytesRead >>= \bytesRead' -> return (Just (messageTypeId, texts), LBS.drop bytesRead' buffer)
            Nothing -> return (Nothing, buffer)
        Nothing -> return (Nothing, buffer)
  in runGet f buffer

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

readNextChunk :: Get (ReadNextChunk)
readNextChunk = do
  maybeChunkLength <- safeGet 4 getWord32be
  case maybeChunkLength of
    Just 0 -> return NothingToRead
    Just chunkLength -> do
      let chunkLengthAsInt64 = fromIntegral chunkLength :: Int64
      maybeByteString <- safeGet chunkLengthAsInt64 $ getLazyByteString chunkLengthAsInt64
      case maybeByteString of
        Just byteString -> case LE.decodeUtf8' byteString of
          Right text -> return $ Chunk text
          Left _ -> return InvalidInput
        Nothing -> return InvalidInput
    Nothing -> return InvalidInput

safeGet :: Int64 -> Get a -> Get (Maybe a)
safeGet bytesRequired getFunction = do
  r <- remaining
  if r >= bytesRequired then do
    v <- getFunction
    return $ Just v
  else
    return Nothing

