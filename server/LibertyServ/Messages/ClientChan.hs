module LibertyServ.Messages.ClientChan (
  ClientChanMessage(..),
  ClientChan
) where
import Control.Concurrent.STM.TChan
import LibertyCommon.NetworkMessage

data ClientChanMessage = SendMessageToClient EncodedMessage | Disconnect
type ClientChan = TChan ClientChanMessage

