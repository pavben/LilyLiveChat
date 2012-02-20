module Liberty.Server.SiteMap (
  SiteMapTVar,
  initializeSiteMap,
) where
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Map (Map)
import qualified Data.Map as Map
import Liberty.Server.DatabaseManager
import Liberty.Server.Types

-- public data
type SiteMapTVar = TVar (Map SiteId SiteDataTVar)

initializeSiteMap :: DatabaseHandleTVar -> IO (Maybe SiteMapTVar)
initializeSiteMap databaseHandleTVar = do
  putStrLn "Attempting to load SiteMap from the database"
  maybeSiteDatas <- getSiteDataFromDb databaseHandleTVar
  case maybeSiteDatas of
    Just siteDatas -> do
      siteDataKeyValues <- mapM (\siteData -> do
        siteDataTVar <- atomically $ newTVar siteData
        return (sdSiteId siteData, siteDataTVar)
        ) siteDatas
      siteMapTVar <- atomically $ newTVar $ Map.fromList siteDataKeyValues
      return $ Just siteMapTVar
    Nothing -> return $ Nothing

