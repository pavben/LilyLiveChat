module Liberty.Server.Messages.ClientSendChan (
  ClientSendChanMessage(..),
  ClientSendChan
) where
import Control.Concurrent.STM.TChan
import Liberty.Common.NetworkMessage

data ClientSendChanMessage = SendMessage EncodedMessage | CloseSocket
type ClientSendChan = TChan ClientSendChanMessage

