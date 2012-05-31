module Liberty.ChatServer.SiteDataSaver (
  initializeSiteDataSaver,
  queueSaveSiteData
) where
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Liberty.Common.Messages.SiteDataService
import Liberty.ChatServer.Types

initializeSiteDataSaver :: IO SiteDataSaverChan
initializeSiteDataSaver = do
  siteDataSaverChan <- atomically $ newTChan

  _ <- forkIO $ siteDataSaverWorker siteDataSaverChan Nothing

  return siteDataSaverChan

siteDataSaverWorker :: SiteDataSaverChan -> Maybe SiteData -> IO ()
siteDataSaverWorker siteDataSaverChan maybeRetrySiteData = do
  -- currently, this can't be Nothing, but that can be implemented later to support clean shutdowns
  maybeSiteData <- case maybeRetrySiteData of
    Just siteData -> return $ Just siteData
    Nothing -> do
      msg <- atomically $ readTChan siteDataSaverChan
      case msg of
        SaveSite siteData -> return $ Just siteData

  case maybeSiteData of
    Just siteData -> do
      saveSiteDataResult <- saveSiteDataToSDS (sdSiteId siteData) $ siteDataToMessage siteData
      case saveSiteDataResult of
        SSDSuccess ->
          -- recurse to read the next message
          siteDataSaverWorker siteDataSaverChan Nothing
        SSDNotAuthoritative ->
          -- if we are no longer the authority for this site, drop the change and continue to the next site
          -- TODO PL: also remove the site from our cache
          siteDataSaverWorker siteDataSaverChan Nothing
        SSDNotAvailable -> do
          putStrLn $ "Error saving site: " ++ show siteData ++ ". Retrying in 5 seconds..."
          threadDelay (5 * 1000 * 1000)
          -- recurse passing the siteData for retry
          siteDataSaverWorker siteDataSaverChan (Just siteData)
    Nothing -> return () -- just exit (currently not possible)
  where
    siteDataToMessage siteData = (
      sdSiteId siteData,
      getPlanIdForPlan (sdPlan siteData),
      sdName siteData,
      sdAdminEmail siteData,
      (fromInteger $ sdNextOperatorId siteData :: Int),
      map operatorToMessage (sdOperators siteData),
      sdAdminPasswordHash siteData)
    operatorToMessage siteOperatorData = (
      (fromInteger $ sodOperatorId siteOperatorData :: Int),
      sodUsername siteOperatorData,
      sodPasswordHash siteOperatorData,
      sodName siteOperatorData,
      sodColor siteOperatorData,
      sodTitle siteOperatorData,
      sodIconUrl siteOperatorData)

queueSaveSiteData :: SiteDataTVar -> SiteDataSaverChan -> STM ()
queueSaveSiteData siteDataTVar siteDataSaverChan = do
  siteData <- readTVar siteDataTVar
  writeTChan siteDataSaverChan $ SaveSite siteData

