module Liberty.ChatServer.SiteMap (
  SiteMapTVar,
  initializeSiteMap,
  createSite,
  SiteLookupError(..),
  lookupSite,
  lookupSiteLocal
) where
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import Liberty.Common.Messages.SiteDataService
import Liberty.Common.Utils
import Liberty.ChatServer.SiteDataSaver
import Liberty.ChatServer.Types

-- public data
type SiteMapTVar = TVar (Map SiteId SiteMapEntryTMVar)
type SiteMapEntryTMVar = TMVar (Either SiteLookupError SiteDataTVar)

initializeSiteMap :: IO (SiteMapTVar)
initializeSiteMap =
  atomically $ newTVar Map.empty

createSite :: SiteDataTVar -> SiteMapTVar -> SiteDataSaverChan -> STM ()
createSite siteDataTVar siteMapTVar siteDataSaverChan = do
  -- get the siteId
  siteData <- readTVar siteDataTVar
  let siteId = sdSiteId siteData
  -- create the site entry TMVar
  siteMapEntryTMVar <- newTMVar $ Right siteDataTVar
  -- insert it into the site map
  siteMap <- readTVar siteMapTVar
  writeTVar siteMapTVar $ Map.insert siteId siteMapEntryTMVar siteMap
  -- save the new site to SDS
  queueSaveSiteData siteDataTVar siteDataSaverChan

data SiteLookupError = SLENotFound | SLENotAvailable
lookupSite :: SiteId -> SiteMapTVar -> IO (Either SiteLookupError SiteDataTVar)
lookupSite siteId siteMapTVar = do
  (siteMapEntryTMVar, mustInitiateLookup) <- atomically $ do
    siteMap <- readTVar siteMapTVar
    case Map.lookup siteId siteMap of
      Just siteMapEntryTMVar ->
        -- this is a TMVar that can block and then have a SiteLookupError or the SiteDataTVar
        -- we don't want to block while holding the read lock on siteMapTVar, so just return it here
        return (siteMapEntryTMVar, False)
      Nothing -> do
        -- no entry (not even an in-progress one)
        -- then we create one and return that
        siteMapEntryTMVar <- newEmptyTMVar
        writeTVar siteMapTVar $ Map.insert siteId siteMapEntryTMVar siteMap
        return (siteMapEntryTMVar, True)

  -- if mustInitiateLookup, forkIO to do that
  if mustInitiateLookup then do
    -- if the map lookup failed, query SDS
    siteDataGetResult <- getSiteDataFromSDS siteId
    atomically $ do
      newSiteMapEntry <- case siteDataGetResult of
        GSDSuccess rawSiteData -> do
          siteDataTVar <- newTVar $ getSiteDataFromRaw rawSiteData
          return $ Right $ siteDataTVar
        GSDNotFound -> return $ Left SLENotFound
        GSDNotAvailable -> return $ Left SLENotAvailable
        -- TODO: handle the "you're not the authoritative server for this site" error

      putTMVar siteMapEntryTMVar $ newSiteMapEntry
  else
    return ()

  -- return the readTMVar value
  atomically (readTMVar siteMapEntryTMVar) >>= return

-- STM version of the lookupSite that performs only the local lookup (since we can't do a service call inside STM)
lookupSiteLocal :: SiteId -> SiteMapTVar -> STM (Maybe SiteDataTVar)
lookupSiteLocal siteId siteMapTVar = do
  siteMap <- readTVar siteMapTVar
  case Map.lookup siteId siteMap of
    Just siteMapEntryTMVar -> readTMVar siteMapEntryTMVar >>= (return . eitherToMaybe)
    Nothing -> return Nothing

getSiteDataFromRaw :: (SiteId, Text, Int, [(Int, Text, Text, Text, Text, Text, Text)], Text) -> SiteData
getSiteDataFromRaw (siteId, name, nextOperatorId, rawOperators, adminPasswordHash) =
  let
    operatorsToSiteData = flip map rawOperators $ \(
      operatorId,
      operatorUsername,
      operatorPasswordHash,
      operatorName,
      operatorColor,
      operatorTitle,
      operatorIconUrl
      ) ->
      SiteOperatorData (toInteger operatorId) operatorUsername operatorPasswordHash  operatorName  operatorColor  operatorTitle  operatorIconUrl
  in
    SiteData siteId name (toInteger nextOperatorId) operatorsToSiteData [] [] adminPasswordHash [] 0

