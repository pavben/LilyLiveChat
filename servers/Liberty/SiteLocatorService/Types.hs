module Liberty.SiteLocatorService.Types (
  SiteId,
  ServerId,
  SiteEntry(..),
  SiteEntryTVar,
  SiteMap,
  SiteMapTVar
) where
import Control.Concurrent.STM.TVar
import Data.Text.Lazy (Text)
import Data.Map (Map)

-- Site
type SiteId = Text
type ServerId = Text
data SiteEntry = SiteEntry {
  seSiteId :: SiteId,
  seServerId :: ServerId
} deriving (Show)

type SiteEntryTVar = TVar SiteEntry

type SiteMap = Map SiteId SiteEntryTVar
type SiteMapTVar = TVar SiteMap

