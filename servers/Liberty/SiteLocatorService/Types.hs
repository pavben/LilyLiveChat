module Liberty.SiteLocatorService.Types (
  SiteId,
  ServerId,
  SiteEntry(..),
  SiteEntryTVar,
  SiteMap,
  SiteMapTVar
) where
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


