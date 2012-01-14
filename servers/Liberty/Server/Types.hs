module Liberty.Server.Types (
  ClientData(..),
  ClientGuestData'(..),
  OtherClientData(..),
  ClientDataTVar,
  SiteId,
  SiteData(..),
  SiteDataTVar,
  ClientSendChanMessage(..),
  ClientSendChan,
  module Liberty.Common.Types
) where
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.Text.Lazy (Text)
import Network.Socket (Socket)
import Liberty.Common.Types

-- Client
data ClientData = ClientData {
  cdSocket :: Socket,
  cdSendChan :: ClientSendChan,
  cdOtherData :: OtherClientData
}
data OtherClientData = ClientUnregistered | ClientGuestData ClientGuestData'
data ClientGuestData' = ClientGuestData' {
  cgdSiteId :: SiteId,
  cgdName :: Text,
  cgdColor :: Text,
  cgdIconUrl :: Text
}
type ClientDataTVar = TVar ClientData

-- Site
type SiteId = Integer
data SiteData = SiteData {
  sdSiteId :: SiteId,
  sdName :: Text,
  sdGuestsWaiting :: [ClientDataTVar],
  sdOperators :: [ClientDataTVar]
} deriving (Show)

type SiteDataTVar = TVar SiteData

-- ClientSendChan
data ClientSendChanMessage = SendMessage EncodedMessage | CloseSocket
type ClientSendChan = TChan ClientSendChanMessage

-- Show instances
instance Show (TVar a) where
  show _ = "TVar"

