module Liberty.Server.Types (
  ClientData(..),
  OtherClientData(..),
  ClientGuestData(..),
  GuestCurrentState(..),
  Operator(..),
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
} deriving (Show)
data OtherClientData = OCDClientUnregistered | OCDClientGuestData ClientGuestData
  deriving (Show)
data ClientGuestData = ClientGuestData {
  cgdName :: Text,
  cgdColor :: Text,
  cgdIconUrl :: Text,
  cgdSiteDataTVar :: SiteDataTVar,
  cgdCurrentState :: GuestCurrentState
} deriving (Show)
data GuestCurrentState = GCSWaiting [Text] | GCSTalkingTo Operator
  deriving (Show)
data Operator = OperatorClient -- TODO: Add OperatorMailman
  deriving (Show)
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

instance Show (TChan a) where
  show _ = "TChan"

