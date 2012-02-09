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
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as LE
import Data.Tuple
import Data.Word
import Liberty.Common.Utils
import Liberty.Common.Types

messageIdsAndTypes :: [(Word8, MessageType)]
messageIdsAndTypes = [
    (1, CustomerJoinMessage),
    (2, CustomerInLinePositionMessage),
    (3, CustomerNowTalkingToMessage),
    (4, CustomerSendChatMessage),
    (5, CustomerReceiveChatMessage),
    (6, CustomerEndingChatMessage),
    (7, SomethingWentWrongMessage),
    (8, OperatorLoginRequestMessage),
    (9, OperatorLoginSuccessMessage),
    (10, OperatorLoginFailedMessage),
    (11, OperatorLineStatusDetailsMessage),
    (12, OperatorLineStatusEmptyMessage),
    (13, OperatorAcceptNextChatSessionMessage),
    (14, OperatorNowTalkingToMessage),
    (15, OperatorReceiveChatMessage),
    (16, OperatorSendChatMessage),
    (17, OperatorEndingChatMessage)
  ]

messageTypeById :: Map Word8 MessageType
messageTypeById = Map.fromList messageIdsAndTypes

messageIdToType :: Word8 -> Maybe MessageType
messageIdToType = flip Map.lookup messageTypeById

messageIdByType :: Map MessageType Word8
messageIdByType = Map.fromList $ map swap messageIdsAndTypes

messageTypeToId :: MessageType -> Word8
messageTypeToId = (Map.!) messageIdByType

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
    Just (byteString, len :: Word32) -> Just $ runPut $ do
      putWord32be len
      putLazyByteString byteString
    Nothing -> Nothing

textToBytestringAndLen :: (Integral a, Bounded a) => Text -> Maybe (ByteString, a)
textToBytestringAndLen text =
  let
    byteString = LE.encodeUtf8 text
    maybeLen = fromIntegerCheckBounds $ toInteger $ LBS.length byteString
  in fmap ((,) byteString) maybeLen

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
      return $ join $ fmap eitherToMaybe $ fmap LE.decodeUtf8' maybeByteString
    Nothing -> return Nothing

safeGet :: Int64 -> Get a -> Get (Maybe a)
safeGet bytesRequired getFunction = do
  r <- remaining
  if r >= bytesRequired then
    fmap Just getFunction
  else
    return Nothing

