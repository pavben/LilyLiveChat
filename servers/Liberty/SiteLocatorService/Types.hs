module Liberty.SiteLocatorService.Types (
  SiteId,
  ServerId,
  SiteEntry(..),
  SiteEntryTVar,
  SiteMap,
  SiteMapTVar,
  ClientSendChanMessage(..),
  ClientSendChan
) where
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)

-- Site
type SiteId = ByteString
type ServerId = ByteString
data SiteEntry = SiteEntry {
  seSiteId :: SiteId,
  seServerId :: ServerId
} deriving (Show)

type SiteEntryTVar = TVar SiteEntry

type SiteMap = Map SiteId SiteEntryTVar
type SiteMapTVar = TVar SiteMap

-- ClientSendChan
data ClientSendChanMessage = SendMessage ByteString | CloseSocket
type ClientSendChan = TChan ClientSendChanMessage

