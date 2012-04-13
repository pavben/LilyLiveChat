module Liberty.SiteLocatorService.SiteMap (
  SiteMapTVar,
  initializeSiteMap,
  lookupServerForSite
) where
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Map as Map
import Liberty.SiteLocatorService.Types

initializeSiteMap :: IO SiteMapTVar
initializeSiteMap = do
  siteMapTVar <- atomically $ newTVar Map.empty
  return $ siteMapTVar

lookupServerForSite :: SiteId -> SiteMapTVar -> IO (Maybe ServerId)
lookupServerForSite siteId siteMapTVar = do
  atomically $ do
    siteMap <- readTVar siteMapTVar
    -- lookup the site in the SiteMap
    case Map.lookup siteId siteMap of
      -- if found, return the server id
      Just siteEntryTVar -> do
        siteEntry <- readTVar siteEntryTVar
        return $ Just $ seServerId siteEntry
      -- if not found, assign a server and return it
      Nothing ->
        case chooseAvailableServer of
          Just serverId -> do
            siteEntryTVar <- newTVar $ SiteEntry siteId serverId
            writeTVar siteMapTVar $ Map.insert siteId siteEntryTVar siteMap
            return $ Just serverId
          Nothing -> return Nothing

-- TODO: Server 'anivia' is hardcoded and considered to be always online
chooseAvailableServer :: Maybe ServerId
chooseAvailableServer = Just $ C8.pack "anivia"

