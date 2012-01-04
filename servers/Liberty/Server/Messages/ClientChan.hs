module Liberty.Server.Messages.ClientChan (
  ClientChanMessage(..),
  ClientChan
) where
import Control.Concurrent.STM.TChan
import Liberty.Common.NetworkMessage

data ClientChanMessage = SendMessageToClient EncodedMessage | Disconnect
type ClientChan = TChan ClientChanMessage

