module LibertyServ.SiteMap (
  SiteMapTVar,
  initializeSiteMap,
  lookupSite
) where
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as LT
import LibertyServ.DatabaseManager
import LibertyServ.Site

-- public data
type SiteMapTVar = TVar (Map SiteId (TVar SiteEntry))

-- private data
data SiteEntry = SiteEntryLoaded SiteData | SiteEntryLoading | SiteEntryLoadFailed LookupFailureReason

initializeSiteMap :: IO (SiteMapTVar)
initializeSiteMap = atomically $ newTVar $ Map.empty

lookupSite :: DatabaseHandleTVar -> SiteMapTVar -> SiteId -> IO (Either LookupFailureReason SiteData)
lookupSite databaseHandleTVar siteMapTVar siteId = do
  (threadResponsibleForLoading, siteEntryTVar) <- atomically $ do
    -- lookup siteId in siteMap
    siteMap <- readTVar siteMapTVar
    case Map.lookup siteId siteMap of
      Just siteEntryTVar -> do
        -- if found, read it
        siteEntry <- readTVar siteEntryTVar
        case siteEntry of
          -- if it currently has a failed value, set it to loading and signal that a load is to be performed
          SiteEntryLoadFailed _ -> do
            writeTVar siteEntryTVar $ SiteEntryLoading
            return (True, siteEntryTVar)
          _ -> return (False, siteEntryTVar)
      Nothing -> do
        newSiteEntryTVar <- newTVar $ SiteEntryLoading
        writeTVar siteMapTVar $ Map.insert siteId newSiteEntryTVar siteMap
        return (True, newSiteEntryTVar)
  
  if threadResponsibleForLoading then do
    _ <- forkIO $ loadSiteDataFromDb databaseHandleTVar siteId siteEntryTVar
    return ()
  else
    return ()

  siteEntry <- atomically $ do
    siteEntry' <- readTVar siteEntryTVar
    case siteEntry' of
      SiteEntryLoading -> retry -- try again when this entry updates (is loaded or fails to load)
      _ -> return siteEntry'

  case siteEntry of
    SiteEntryLoaded siteData -> return $ Right siteData
    SiteEntryLoadFailed lookupFailureReason -> return $ Left lookupFailureReason
    SiteEntryLoading -> error "Unexpected siteEntry value of SiteEntryLoading -- concurrency error"

loadSiteDataFromDb :: DatabaseHandleTVar -> SiteId -> TVar SiteEntry -> IO ()
loadSiteDataFromDb databaseHandleTVar siteId siteEntryTVar = do
  result <- getSiteDataFromDb databaseHandleTVar siteId
  atomically $ writeTVar siteEntryTVar $ case result of
    Right siteData -> SiteEntryLoaded siteData
    Left lookupFailureReason -> SiteEntryLoadFailed lookupFailureReason
  return ()

