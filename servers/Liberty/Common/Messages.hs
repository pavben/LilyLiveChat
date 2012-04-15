{-# LANGUAGE ScopedTypeVariables #-}

module Liberty.Common.Messages (
  MessageType(..),
  createMessage,
  parseMessage,
  unpackMessage
) where
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.MessagePack as MP
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as LE
import Data.Word
import Liberty.Common.Utils
import Liberty.Common.Types

class MessageType a where
  messageTypeById :: MessageType a => Map Int a
  messageIdToType :: MessageType a => Int -> Maybe a
  messageIdByType :: MessageType a => Map a Int
  messageTypeToId :: MessageType a => a -> Int

createMessage :: (MessageType a, MP.Packable b) => a -> b -> Maybe ByteString
createMessage messageType params =
  case fromIntegerCheckBounds $ fromIntegral $ LBS.length packedMessageIdAndParams :: Maybe Word32 of
    Just messageLen -> Just $ runPut $ do
      putWord32be messageLen
      putLazyByteString packedMessageIdAndParams
    Nothing -> Nothing
  where packedMessageIdAndParams = MP.pack (messageTypeToId messageType, MP.pack params)

parseMessage :: MessageType a => ByteString -> Maybe (Maybe (a, ByteString), ByteString)
parseMessage buffer = flip runGet buffer $ do
  maybeMessageLength <- safeGet 4 getWord32be
  case maybeMessageLength of
    Just messageLength -> do
      let messageLengthAsInt64 = fromIntegral messageLength :: Int64
      maybePackedMessageIdAndParams <- safeGet messageLengthAsInt64 $ getLazyByteString messageLengthAsInt64
      case maybePackedMessageIdAndParams of
        Just packedMessageIdAndParams ->
          -- try unpacking (MessageType as Int, Params as ByteString)
          case MP.tryUnpack packedMessageIdAndParams of
            Right (messageId :: Int, paramsAsByteString :: ByteString) ->
              case messageIdToType messageId of
                Just messageType -> bytesRead >>= \bytesRead' -> return $ Just (Just (messageType, paramsAsByteString), LBS.drop bytesRead' buffer)
                Nothing -> return Nothing -- invalid message type; protocol violation
            Left _ -> return Nothing -- message was not packed in the (Int, ByteString) format; protocol violation
        Nothing -> return $ Just (Nothing, buffer) -- not received the entire packedMessageIdAndParams yet
    Nothing -> return $ Just (Nothing, buffer) -- messageLength not yet received

unpackMessage :: MP.Unpackable a => ByteString -> Maybe a
unpackMessage byteString = eitherToMaybe $ MP.tryUnpack byteString

safeGet :: Int64 -> Get a -> Get (Maybe a)
safeGet bytesRequired getFunction = do
  r <- remaining
  if r >= bytesRequired then
    fmap Just getFunction
  else
    return Nothing

