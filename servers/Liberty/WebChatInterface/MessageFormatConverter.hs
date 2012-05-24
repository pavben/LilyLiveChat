{-# LANGUAGE ScopedTypeVariables #-}

module Liberty.WebChatInterface.MessageFormatConverter (
  createMessageFromJson,
  messageToJson
) where
import qualified Data.Aeson as J
import Data.Attoparsec.Number as DAN
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import qualified Data.MessagePack as MP
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Prelude hiding (catch)
import Liberty.Common.Messages
import Liberty.Common.Messages.ChatServer
import Debug.Trace

-- JSON to encoded MessagePack message
createMessageFromJson :: [J.Value] -> Maybe ByteString
createMessageFromJson jValues =
  case jValues of
    J.Number (DAN.I messageTypeId) : messageParamsAsJson ->
      case messageIdToType (fromInteger messageTypeId) :: Maybe ChatServerMessageType of
        Just messageType -> jsonToMP messageType messageParamsAsJson
        Nothing -> Nothing -- message id not recognized
    _ -> Nothing -- no message type or message type not a number

jsonToMP :: ChatServerMessageType -> [J.Value] -> Maybe ByteString

jsonToMP UnregisteredSelectSiteMessage [J.String siteId] =
  createMessage UnregisteredSelectSiteMessage (siteId)

jsonToMP CustomerJoinMessage [J.String visitorId, J.String currentPage, J.String referrer] =
  let
    maybeVisitorId = if not $ T.null visitorId then Just visitorId else Nothing
    maybeCurrentPage = if not $ T.null currentPage then Just currentPage else Nothing
    maybeReferrer = if not $ T.null referrer then Just referrer else Nothing
  in
    createMessage CustomerJoinMessage (maybeVisitorId, maybeCurrentPage, maybeReferrer)

jsonToMP CustomerSendChatMessage [J.String text] =
  createMessage CustomerSendChatMessage (text)

jsonToMP CustomerEndingChatMessage [] =
  createMessage CustomerEndingChatMessage ()

jsonToMP OperatorLoginRequestMessage [J.String username, J.String password] =
  createMessage OperatorLoginRequestMessage (username, password)

jsonToMP OperatorAcceptNextChatSessionMessage [] =
  createMessage OperatorAcceptNextChatSessionMessage ()

jsonToMP OperatorSendChatMessage [J.Number (DAN.I sessionId), J.String text] =
  createMessage OperatorSendChatMessage (fromInteger sessionId :: Int, text)

jsonToMP OperatorEndingChatMessage [J.Number (DAN.I sessionId)] =
  createMessage OperatorEndingChatMessage (fromInteger sessionId :: Int)

jsonToMP AdminLoginRequestMessage [J.String password] =
  createMessage AdminLoginRequestMessage (password)

jsonToMP AdminOperatorCreateMessage [J.String username, J.String password, J.String name, J.String color, J.String title, J.String iconUrl] =
  createMessage AdminOperatorCreateMessage (username, password, name, color, title, iconUrl)

jsonToMP AdminOperatorReplaceMessage [J.Number (DAN.I operatorId), J.String username, J.String password, J.String name, J.String color, J.String title, J.String iconUrl] =
  createMessage AdminOperatorReplaceMessage (fromInteger operatorId :: Int, username, password, name, color, title, iconUrl)

jsonToMP AdminOperatorDeleteMessage [J.Number (DAN.I operatorId)] =
  createMessage AdminOperatorDeleteMessage (fromInteger operatorId :: Int)

jsonToMP AdminSetSiteNameMessage [J.String name] =
  createMessage AdminSetSiteNameMessage (name)

jsonToMP AdminSetAdminPasswordMessage [J.String password] =
  createMessage AdminSetAdminPasswordMessage (password)

jsonToMP _ _ = Nothing

-- MessagePack format to JSON
messageToJson :: ChatServerMessageType -> ByteString -> Maybe [J.Value]

messageToJson UnregisteredSiteSelectedMessage encodedParams =
  unpackAndHandle encodedParams $ \(siteName :: Text, isActive :: Bool) -> [J.toJSON (messageTypeToId UnregisteredSiteSelectedMessage), J.toJSON siteName, J.toJSON isActive]

messageToJson UnregisteredSiteInvalidMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId UnregisteredSiteInvalidMessage)]

messageToJson CustomerJoinSuccessMessage encodedParams =
  unpackAndHandle encodedParams $ \(color :: Text) -> [J.toJSON (messageTypeToId CustomerJoinSuccessMessage), J.toJSON color]

messageToJson CustomerInLinePositionMessage encodedParams =
  unpackAndHandle encodedParams $ \(position :: Int) -> [J.toJSON (messageTypeToId CustomerInLinePositionMessage), J.toJSON position]

messageToJson CustomerNowTalkingToMessage encodedParams =
  unpackAndHandle encodedParams $ \(name :: Text, color :: Text, title :: Text, iconUrl :: Text) -> [J.toJSON (messageTypeToId CustomerNowTalkingToMessage), J.toJSON name, J.toJSON color, J.toJSON title, J.toJSON iconUrl]

messageToJson CustomerReceiveChatMessage encodedParams =
  unpackAndHandle encodedParams $ \(text :: Text) -> [J.toJSON (messageTypeToId CustomerReceiveChatMessage), J.toJSON text]

messageToJson SomethingWentWrongMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId SomethingWentWrongMessage)]

messageToJson CustomerChatEndedMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId CustomerChatEndedMessage)]

messageToJson CustomerNoOperatorsAvailableMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId CustomerNoOperatorsAvailableMessage)]

messageToJson CSUnavailableMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId CSUnavailableMessage)]

messageToJson OperatorLoginSuccessMessage encodedParams =
  unpackAndHandle encodedParams $ \(name :: Text, color :: Text, title :: Text, iconUrl :: Text) -> [J.toJSON (messageTypeToId OperatorLoginSuccessMessage), J.toJSON name, J.toJSON color, J.toJSON title, J.toJSON iconUrl]

messageToJson OperatorLoginFailedMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId OperatorLoginFailedMessage)]

messageToJson OperatorLineStatusDetailsMessage encodedParams =
  unpackAndHandle encodedParams $ \(nextCustomerColor :: Text, numberOfWaiters :: Int) -> [J.toJSON (messageTypeToId OperatorLineStatusDetailsMessage), J.toJSON nextCustomerColor, J.toJSON numberOfWaiters]

messageToJson OperatorLineStatusEmptyMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId OperatorLineStatusEmptyMessage)]

messageToJson OperatorNowTalkingToMessage encodedParams =
  unpackAndHandle encodedParams $ \(sessionId :: Int, color :: Text, maybeCurrentPage :: Maybe Text, maybeReferrer :: Maybe Text) ->
  let
    currentPage = fromMaybe LT.empty maybeCurrentPage
    referrer = fromMaybe LT.empty maybeReferrer
  in
    [J.toJSON (messageTypeToId OperatorNowTalkingToMessage), J.toJSON sessionId, J.toJSON color, J.toJSON currentPage, J.toJSON referrer]

messageToJson OperatorReceiveChatMessage encodedParams =
  unpackAndHandle encodedParams $ \(sessionId :: Int, text :: Text) -> [J.toJSON (messageTypeToId OperatorReceiveChatMessage), J.toJSON sessionId, J.toJSON text]

messageToJson OperatorChatEndedMessage encodedParams =
  unpackAndHandle encodedParams $ \(sessionId :: Int) -> [J.toJSON (messageTypeToId OperatorChatEndedMessage), J.toJSON sessionId]

messageToJson CSMTOperatorCustomerOnPage encodedParams =
  unpackAndHandle encodedParams $ \(sessionId :: Int, currentPage :: Text) -> [J.toJSON (messageTypeToId CSMTOperatorCustomerOnPage), J.toJSON sessionId, J.toJSON currentPage]

messageToJson AdminLoginSuccessMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminLoginSuccessMessage)]

messageToJson AdminLoginFailedMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminLoginFailedMessage)]

messageToJson AdminSiteInfoMessage encodedParams =
  unpackAndHandle encodedParams $ \(siteId :: Text, name :: Text) -> [J.toJSON (messageTypeToId AdminSiteInfoMessage), J.toJSON siteId, J.toJSON name]

messageToJson AdminSetSiteNameSuccessMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminSetSiteNameSuccessMessage)]

messageToJson AdminOperatorDetailsStartMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorDetailsStartMessage)]

messageToJson AdminOperatorDetailsMessage encodedParams =
  unpackAndHandle encodedParams $ \(operatorId :: Int, username :: Text, name :: Text, color :: Text, title :: Text, iconUrl :: Text) -> [J.toJSON (messageTypeToId AdminOperatorDetailsMessage), J.toJSON operatorId, J.toJSON username, J.toJSON name, J.toJSON color, J.toJSON title, J.toJSON iconUrl]

messageToJson AdminOperatorDetailsEndMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorDetailsEndMessage)]

messageToJson AdminOperatorCreateSuccessMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorCreateSuccessMessage)]

messageToJson AdminOperatorCreateDuplicateUsernameMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorCreateDuplicateUsernameMessage)]

messageToJson AdminOperatorReplaceSuccessMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorReplaceSuccessMessage)]

messageToJson AdminOperatorReplaceDuplicateUsernameMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorReplaceDuplicateUsernameMessage)]

messageToJson AdminOperatorReplaceInvalidIdMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorReplaceInvalidIdMessage)]

messageToJson AdminOperatorDeleteSuccessMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorDeleteSuccessMessage)]

messageToJson AdminOperatorDeleteFailedMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorDeleteFailedMessage)]

messageToJson AdminSetAdminPasswordSuccessMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminSetAdminPasswordSuccessMessage)]

messageToJson _ _ = Nothing

unpackAndHandle :: MP.Unpackable a => ByteString -> (a -> b) -> Maybe b
unpackAndHandle encodedParams handleFunction =
  case unpackMessage encodedParams of
    Just params -> Just $ handleFunction params
    -- TODO: remove the trace
    Nothing -> trace "unpackMessage failed" Nothing -- either the encoded params were invalid or they did not match the params we expect

