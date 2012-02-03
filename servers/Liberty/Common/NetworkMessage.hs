{-# LANGUAGE ScopedTypeVariables #-}

module Liberty.Common.NetworkMessage (
  MessageType(..),
  Message,
  EncodedMessage,
  messageIdToType,
  messageTypeToId,
  createMessage,
  parseMessage
) where
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as LE
import Data.Word
import Liberty.Common.Utils
import Liberty.Common.Types

messageTypesAndIds = [
  (CustomerJoinMessage, 1),
  (CustomerInLinePositionMessage, 2),
  (CustomerNowTalkingToMessage, 3),
  (CustomerSendChatMessage, 4),
  (CustomerReceiveChatMessage, 5),
  (CustomerEndingChatMessage, 6),
  (SomethingWentWrongMessage, 7),
  (OperatorLoginRequestMessage, 8),
  (OperatorLoginSuccessMessage, 9),
  (OperatorLoginFailedMessage, 10),
  (OperatorLineStatusDetailsMessage, 11),
  (OperatorLineStatusEmptyMessage, 12),
  (OperatorAcceptNextChatSessionMessage, 13),
  (OperatorNowTalkingToMessage, 14)
  ]

messageTypeById :: Map Word8 MessageType
messageTypeById = Map.fromList $ map swap messageTypesAndIds

messageIdToType :: Word8 -> Maybe MessageType
messageIdToType = flip Map.lookup messageTypeById

messageIdByType :: Map MessageType Word8
messageIdByType = Map.fromList messageTypesAndIds

messageTypeToId :: MessageType -> Word8
messageTypeToId = (messageIdByType !)

-- createMessage and dependencies
createMessage :: Message -> Maybe EncodedMessage
createMessage (messageType, params) = 
  case fromIntegerCheckBounds $ fromIntegral $ length params :: Maybe Word8 of
    Just numParams ->
      case sequence $ map textToBytestringWithLen params of
        Just encodedParams -> Just $ LBS.concat [LBS.singleton (messageTypeToId messageType), LBS.singleton numParams, LBS.concat encodedParams]
        Nothing -> Nothing
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
    maybeLen = fromIntegerCheckBounds $ toInteger $ LBS.length bytestring
  in fmap ((,) bytestring) maybeLen

-- parseMessage and dependencies

-- Exceptions handled by caller
parseMessage :: ByteString -> Maybe (Maybe Message, ByteString)
parseMessage buffer = flip runGet buffer $ do
  maybeMessageTypeId <- safeGet 1 getWord8
  case maybeMessageTypeId of
    Just messageTypeId -> case messageIdToType messageTypeId of
      Just messageType -> do
	maybeNumParams <- safeGet 1 getWord8
	case maybeNumParams of
	  Just numParams -> do
	    maybeTexts <- readTexts numParams
	    case maybeTexts of
	      Just texts -> bytesRead >>= \bytesRead' -> return $ Just (Just (messageType, texts), LBS.drop bytesRead' buffer)
	      Nothing -> return $ Just (Nothing, buffer) -- not received all the texts yet
	  Nothing -> return $ Just (Nothing, buffer) -- could not read the number of parameters byte
      Nothing -> return $ Nothing -- invalid message type / protocol not followed
    Nothing -> return $ Just (Nothing, buffer) -- messageTypeId byte not yet received

readTexts :: Word8 -> Get (Maybe [Text])
readTexts numParams = do
  maybeTexts <- readChunks [] numParams
  return $ fmap reverse maybeTexts

readChunks :: [Text] -> Word8 -> Get (Maybe [Text])
readChunks texts paramsRemaining =
  if paramsRemaining > 0 then do
    nextChunk <- readNextChunk
    case nextChunk of
      Just text -> readChunks (text : texts) (paramsRemaining - 1)
      Nothing -> return Nothing
  else
    return $ Just texts

readNextChunk :: Get (Maybe Text)
readNextChunk = do
  maybeChunkLength <- safeGet 4 getWord32be
  case maybeChunkLength of
    Just chunkLength -> do
      let chunkLengthAsInt64 = fromIntegral chunkLength :: Int64
      maybeByteString <- safeGet chunkLengthAsInt64 $ getLazyByteString chunkLengthAsInt64
      return $ fmap (eitherToMaybe . fmap LE.decodeUtf8') maybeByteString
    Nothing -> return Nothing

safeGet :: Int64 -> Get a -> Get (Maybe a)
safeGet bytesRequired getFunction = do
  r <- remaining
  if r >= bytesRequired then
    fmap Just getFunction
  else
    return Nothing

