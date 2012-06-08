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

jsonToMP CSMTUnregisteredActivateOperator [J.String sessionId, J.Number (DAN.I operatorId), J.String activationToken] =
  createMessage CSMTUnregisteredActivateOperator (sessionId, fromInteger operatorId :: Int, activationToken)

jsonToMP CSMTUnregisteredActivateAdmin [J.String sessionId] =
  createMessage CSMTUnregisteredActivateAdmin (sessionId)

jsonToMP CSMTUnregisteredIsOperatorActivated [J.Number (DAN.I operatorId)] =
  createMessage CSMTUnregisteredIsOperatorActivated (fromInteger operatorId :: Int)

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

jsonToMP OperatorLoginRequestMessage [J.String sessionId] =
  createMessage OperatorLoginRequestMessage (sessionId)

jsonToMP OperatorAcceptNextChatSessionMessage [] =
  createMessage OperatorAcceptNextChatSessionMessage ()

jsonToMP OperatorSendChatMessage [J.Number (DAN.I sessionId), J.String text] =
  createMessage OperatorSendChatMessage (fromInteger sessionId :: Int, text)

jsonToMP OperatorEndingChatMessage [J.Number (DAN.I sessionId)] =
  createMessage OperatorEndingChatMessage (fromInteger sessionId :: Int)

jsonToMP AdminLoginRequestMessage [J.String sessionId] =
  createMessage AdminLoginRequestMessage (sessionId)

jsonToMP AdminOperatorCreateMessage [J.String name, J.String color, J.String title, J.String iconUrl, J.String email] =
  createMessage AdminOperatorCreateMessage (name, color, title, iconUrl, email)

jsonToMP AdminOperatorReplaceMessage [J.Number (DAN.I operatorId), J.String name, J.String color, J.String title, J.String iconUrl] =
  createMessage AdminOperatorReplaceMessage (fromInteger operatorId :: Int, name, color, title, iconUrl)

jsonToMP AdminOperatorDeleteMessage [J.Number (DAN.I operatorId)] =
  createMessage AdminOperatorDeleteMessage (fromInteger operatorId :: Int)

jsonToMP CSMTAdminSetSiteInfoMessage [J.String siteName] =
  createMessage CSMTAdminSetSiteInfoMessage (siteName)

jsonToMP CSMTAdminSendOperatorWelcomeEmail [J.String operatorId, J.String email] =
  createMessage CSMTAdminSendOperatorWelcomeEmail (operatorId, email)

jsonToMP _ _ = Nothing

-- MessagePack format to JSON
messageToJson :: ChatServerMessageType -> ByteString -> Maybe [J.Value]

messageToJson UnregisteredSiteSelectedMessage encodedParams =
  unpackAndHandle encodedParams $ \(siteName :: Text, isActive :: Bool, hasAuthentication :: Bool) -> [J.toJSON (messageTypeToId UnregisteredSiteSelectedMessage), J.toJSON siteName, J.toJSON isActive, J.toJSON hasAuthentication]

messageToJson UnregisteredSiteInvalidMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId UnregisteredSiteInvalidMessage)]

messageToJson CSMTUnregisteredActivateOperatorSuccess encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId CSMTUnregisteredActivateOperatorSuccess)]

messageToJson CSMTUnregisteredActivateOperatorFailure encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId CSMTUnregisteredActivateOperatorFailure)]

messageToJson CSMTUnregisteredActivateAdminSuccess encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId CSMTUnregisteredActivateAdminSuccess)]

messageToJson CSMTUnregisteredActivateAdminFailure encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId CSMTUnregisteredActivateAdminFailure)]

messageToJson CSMTUnregisteredIsOperatorActivatedResponse encodedParams =
  unpackAndHandle encodedParams $ \(isActivated :: Bool) -> [J.toJSON (messageTypeToId CSMTUnregisteredIsOperatorActivatedResponse), J.toJSON isActivated]

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

messageToJson CSMTWrongChatServer encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId CSMTWrongChatServer)]

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
  unpackAndHandle encodedParams $ \(siteId :: Text, planId :: Int, siteName :: Text, isActivated :: Bool) -> [J.toJSON (messageTypeToId AdminSiteInfoMessage), J.toJSON siteId, J.toJSON planId, J.toJSON siteName, J.toJSON isActivated]

messageToJson CSMTAdminSetSiteInfoSuccessMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId CSMTAdminSetSiteInfoSuccessMessage)]

messageToJson AdminOperatorDetailsStartMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorDetailsStartMessage)]

messageToJson AdminOperatorDetailsMessage encodedParams =
  unpackAndHandle encodedParams $ \(operatorId :: Int, name :: Text, color :: Text, title :: Text, iconUrl :: Text) -> [J.toJSON (messageTypeToId AdminOperatorDetailsMessage), J.toJSON operatorId, J.toJSON name, J.toJSON color, J.toJSON title, J.toJSON iconUrl]

messageToJson AdminOperatorDetailsEndMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorDetailsEndMessage)]

messageToJson AdminOperatorCreateSuccessMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorCreateSuccessMessage)]

messageToJson CSMTAdminSendOperatorWelcomeEmailSuccess encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId CSMTAdminSendOperatorWelcomeEmailSuccess)]

messageToJson AdminOperatorReplaceSuccessMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorReplaceSuccessMessage)]

messageToJson AdminOperatorReplaceInvalidIdMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorReplaceInvalidIdMessage)]

messageToJson AdminOperatorDeleteSuccessMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorDeleteSuccessMessage)]

messageToJson AdminOperatorDeleteFailedMessage encodedParams =
  unpackAndHandle encodedParams $ \() -> [J.toJSON (messageTypeToId AdminOperatorDeleteFailedMessage)]

messageToJson _ _ = Nothing

unpackAndHandle :: MP.Unpackable a => ByteString -> (a -> b) -> Maybe b
unpackAndHandle encodedParams handleFunction =
  case unpackMessage encodedParams of
    Just params -> Just $ handleFunction params
    -- TODO: remove the trace
    Nothing -> trace "unpackMessage failed" Nothing -- either the encoded params were invalid or they did not match the params we expect

