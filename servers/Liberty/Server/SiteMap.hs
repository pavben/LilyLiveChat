module Liberty.Server.SiteMap (
  SiteMapTVar,
  initializeSiteMap,
  lookupSite,
  LookupFailureReason(..)
) where
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.Map (Map)
import qualified Data.Map as Map
import Liberty.Server.DatabaseManager
import Liberty.Server.Types

-- public data
type SiteMapTVar = TVar (Map SiteId (TVar SiteEntry))

-- private data
data SiteEntry = SiteEntryLoaded SiteDataTVar | SiteEntryLoading | SiteEntryLoadFailed LookupFailureReason

initializeSiteMap :: IO SiteMapTVar
initializeSiteMap = atomically $ newTVar Map.empty

-- TODO: make idle sites expire
lookupSite :: DatabaseHandleTVar -> SiteMapTVar -> SiteId -> IO (Either LookupFailureReason SiteDataTVar)
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
            writeTVar siteEntryTVar SiteEntryLoading
            return (True, siteEntryTVar)
          _ -> return (False, siteEntryTVar)
      Nothing -> do
        newSiteEntryTVar <- newTVar SiteEntryLoading
        writeTVar siteMapTVar $ Map.insert siteId newSiteEntryTVar siteMap
        return (True, newSiteEntryTVar)
  
  when threadResponsibleForLoading $ void $ forkIO $ loadSiteDataFromDb databaseHandleTVar siteId siteEntryTVar

  siteEntry <- atomically $ do
    siteEntry' <- readTVar siteEntryTVar
    case siteEntry' of
      SiteEntryLoading -> retry -- try again when this entry updates (is loaded or fails to load)
      _ -> return siteEntry'

  case siteEntry of
    SiteEntryLoaded siteDataTVar -> return $ Right siteDataTVar
    SiteEntryLoadFailed lookupFailureReason -> return $ Left lookupFailureReason
    SiteEntryLoading -> error "Unexpected siteEntry value of SiteEntryLoading -- concurrency error"

loadSiteDataFromDb :: DatabaseHandleTVar -> SiteId -> TVar SiteEntry -> IO ()
loadSiteDataFromDb databaseHandleTVar siteId siteEntryTVar = do
  result <- getSiteDataFromDb databaseHandleTVar siteId
  case result of
    Right siteData -> do
      siteDataTVar <- atomically $ newTVar siteData
      atomically $ writeTVar siteEntryTVar $ SiteEntryLoaded siteDataTVar
    Left lookupFailureReason -> atomically $ writeTVar siteEntryTVar $ SiteEntryLoadFailed lookupFailureReason
  return ()

