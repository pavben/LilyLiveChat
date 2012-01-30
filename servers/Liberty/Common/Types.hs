module Liberty.Common.Types (
  MessageType(..),
  Message,
  EncodedMessage
) where
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)

-- common
data MessageType = CustomerJoinMessage
                 | InLinePositionMessage
                 | NowTalkingToMessage
                 | ChatMessage
                 | AppendToChatLogMessage
                 | EndChatMessage
                 | SomethingWentWrongMessage
                 | OperatorLoginRequestMessage
                 | OperatorLoginSuccessMessage
                 | OperatorLoginFailedMessage
                 | LineStatusUpdateMessage
                 | LineIsEmptyMessage
                 | AcceptNextChatSessionMessage
  deriving (Show)

type Message = (MessageType, [Text])
type EncodedMessage = ByteString

