module Liberty.Common.Types (
  MessageType(..),
  Message,
  EncodedMessage
) where
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)

-- common
data MessageType = CustomerJoinMessage
                 | CustomerInLinePositionMessage
                 | CustomerNowTalkingToMessage
                 | CustomerSendChatMessage
                 | CustomerReceiveChatMessage
                 | CustomerEndingChatMessage
                 | SomethingWentWrongMessage
                 | OperatorLoginRequestMessage
                 | OperatorLoginSuccessMessage
                 | OperatorLoginFailedMessage
                 | OperatorLineStatusDetailsMessage
                 | OperatorLineStatusEmptyMessage
                 | OperatorAcceptNextChatSessionMessage
                 | OperatorNowTalkingToMessage
                 | OperatorReceiveChatMessage
                 | OperatorSendChatMessage
                 | OperatorEndingChatMessage
                 | OperatorChatEndedMessage
                 | CustomerChatEndedMessage
                 | UnregisteredSelectSiteMessage
                 | UnregisteredSiteSelectedMessage
                 | UnregisteredSiteInvalidMessage
                 | CustomerNoOperatorsAvailableMessage
                 | AdminLoginRequestMessage
                 | AdminOperatorCreateMessage
                 | AdminOperatorReplaceMessage
                 | AdminOperatorDeleteMessage
                 | AdminLoginSuccessMessage
                 | AdminLoginFailedMessage
                 | AdminSiteInfoMessage
                 | AdminOperatorDetailsStartMessage
                 | AdminOperatorDetailsMessage
                 | AdminOperatorDetailsEndMessage
                 | AdminOperatorCreateSuccessMessage
                 | AdminOperatorCreateFailedMessage
                 | AdminOperatorReplaceSuccessMessage
                 | AdminOperatorReplaceFailedMessage
                 | AdminAdminDetailsStartMessage
                 | AdminAdminDetailsMessage
                 | AdminAdminDetailsEndMessage
                 | SALoginRequestMessage
                 | SASiteCreateMessage
                 | SASiteDeleteMessage
                 | SAGetSiteInfoMessage
                 | SASetExpiryTimestampMessage
                 | SALoginSuccessMessage
                 | SALoginFailedMessage
                 | SASiteCreateSuccessMessage
                 | SASiteCreateFailedMessage
                 | SASiteDeleteSuccessMessage
                 | SASiteDeleteFailedMessage
                 | SASiteInfoMessage
                 | SASiteInfoFailedMessage
                 | SASetExpiryTimestampSuccessMessage
                 | AdminAdminCreateMessage
                 | AdminAdminReplaceMessage
                 | AdminAdminDeleteMessage
                 | AdminAdminCreateSuccessMessage
                 | AdminAdminCreateFailedMessage
                 | AdminAdminReplaceSuccessMessage
                 | AdminAdminReplaceFailedMessage
                 | AdminOperatorDeleteSuccessMessage
                 | AdminOperatorDeleteFailedMessage
                 | AdminAdminDeleteSuccessMessage
                 | AdminAdminDeleteFailedMessage
  deriving (Show, Eq, Ord)

type Message = (MessageType, [Text])
type EncodedMessage = ByteString

