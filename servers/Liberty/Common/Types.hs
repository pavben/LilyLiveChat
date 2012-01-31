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
  deriving (Show)

type Message = (MessageType, [Text])
type EncodedMessage = ByteString

