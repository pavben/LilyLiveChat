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
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as LE
import Data.Word
import Liberty.Common.Utils
import Liberty.Common.Types

messageTypeToId :: MessageType -> Word8
messageTypeToId messageType = case messageType of
  CustomerJoinMessage -> 1
  CustomerInLinePositionMessage -> 2
  CustomerNowTalkingToMessage -> 3
  CustomerSendChatMessage -> 4
  CustomerReceiveChatMessage -> 5
  CustomerEndingChatMessage -> 6
  SomethingWentWrongMessage -> 7
  OperatorLoginRequestMessage -> 8
  OperatorLoginSuccessMessage -> 9
  OperatorLoginFailedMessage -> 10
  OperatorLineStatusDetailsMessage -> 11
  OperatorLineStatusEmptyMessage -> 12
  OperatorAcceptNextChatSessionMessage -> 13
  OperatorNowTalkingToMessage -> 14

messageIdToType :: Word8 -> Maybe MessageType
messageIdToType messageId = case messageId of
  1 -> Just CustomerJoinMessage
  2 -> Just CustomerInLinePositionMessage
  3 -> Just CustomerNowTalkingToMessage
  4 -> Just CustomerSendChatMessage
  5 -> Just CustomerReceiveChatMessage
  6 -> Just CustomerEndingChatMessage
  7 -> Just SomethingWentWrongMessage
  8 -> Just OperatorLoginRequestMessage
  9 -> Just OperatorLoginSuccessMessage
  10 -> Just OperatorLoginFailedMessage
  11 -> Just OperatorLineStatusDetailsMessage
  12 -> Just OperatorLineStatusEmptyMessage
  13 -> Just OperatorAcceptNextChatSessionMessage
  14 -> Just OperatorNowTalkingToMessage
  _ -> Nothing

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
  in
    case maybeLen of
      Just len -> Just (bytestring, len)
      Nothing -> Nothing

-- parseMessage and dependencies

-- Exceptions handled by caller
parseMessage :: ByteString -> Maybe (Maybe Message, ByteString)
parseMessage buffer =
  let
    f = do
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
  in runGet f buffer

readTexts :: Word8 -> Get (Maybe [Text])
readTexts numParams = do
  maybeTexts <- readChunks [] numParams
  case maybeTexts of
    Just texts -> return $ Just $ reverse $ texts
    Nothing -> return Nothing

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
      case maybeByteString of
        Just byteString -> case LE.decodeUtf8' byteString of
          Right text -> return $ Just text
          Left _ -> return Nothing
        Nothing -> return Nothing
    Nothing -> return Nothing

safeGet :: Int64 -> Get a -> Get (Maybe a)
safeGet bytesRequired getFunction = do
  r <- remaining
  if r >= bytesRequired then do
    v <- getFunction
    return $ Just v
  else
    return Nothing

