module LibertyServ.SiteMap (
) where
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Map (Map)
import qualified Data.Map as Map
import LibertyServ.Site

data SiteEntry = SiteEntryLoaded SiteData | SiteEntryLoading
  deriving (Show)

initializeSiteMap :: IO (TVar (Map SiteId (TVar SiteEntry)))
initializeSiteMap = atomically $ newTVar $ Map.empty

--lookupSite :: SiteId -> IO (Maybe (TVar SiteEntry)))
--lookupSite siteId =
