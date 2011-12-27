module LibertyServ.SiteMap (
) where
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Map (Map)
import qualified Data.Map as Map

type SiteId = Integer
data SiteEntry = SiteData | SiteLoading
  deriving (Show)

initializeSiteMap :: IO (TVar (Map SiteId (TVar SiteEntry)))
initializeSiteMap = atomically $ newTVar $ Map.empty

--lookupSite :: SiteId -> IO (Maybe (TVar SiteEntry)))
--lookupSite siteId =
