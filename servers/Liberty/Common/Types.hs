module Liberty.Common.Types (
  MessageType(..),
  Message,
  EncodedMessage
) where
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)

-- common
data MessageType = GuestJoinMessage
                 | InLinePositionUpdateMessage
                 | NowTalkingToMessage
                 | ChatMessage
                 | AppendToChatLogMessage
                 | EndChatMessage
                 | SomethingWentWrongMessage
  deriving (Show)

type Message = (MessageType, [Text])
type EncodedMessage = ByteString

