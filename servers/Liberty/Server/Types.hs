module Liberty.Server.Types (
  ClientData(..),
  OtherClientData(..),
  ClientGuestData(..),
  ChatSession(..),
  ChatSessionTVar,
  ChatOperatorEntry(..),
  ChatLogEntry(..),
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
  cgdChatSession :: TVar ChatSession
} deriving (Show)
data ChatSession = ChatSession {
  csGuestClientDataTVar :: ClientDataTVar,
  csOperator :: ChatOperatorEntry,
  csLog :: [ChatLogEntry]
} deriving (Show)
type ChatSessionTVar = TVar ChatSession
data ChatOperatorEntry = ChatOperatorNobody | ChatOperatorClient (TVar ClientData)
  deriving (Show)
data ChatLogEntry = CLEJoin Text Text -- name, color
  | CLEMessage Text Text Text -- name, color, text
  deriving (Show)
type ClientDataTVar = TVar ClientData

-- Site
type SiteId = Integer
data SiteData = SiteData {
  sdSiteId :: SiteId,
  sdName :: Text,
  sdSessionsWaiting :: [ChatSessionTVar],
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

