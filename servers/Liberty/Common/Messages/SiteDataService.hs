module Liberty.Common.Messages.SiteDataService (
  SiteDataServiceMessageType(..),
  GSDResult(..),
  getSiteDataFromSDS,
  SSDResult(..),
  saveSiteDataToSDS
) where
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Data.Tuple
import Liberty.Common.Messages
import Liberty.Common.ServiceClient

data SiteDataServiceMessageType = GetSiteDataMessage
                                | SaveSiteDataMessage
                                | SiteNotFoundMessage
                                | DataNotAvailableMessage
                                | NonAuthoritativeServerMessage
                                | SiteDataFoundMessage
                                | SiteDataSavedMessage
                                | SiteDataSaveFailedMessage
  deriving (Show, Eq, Ord)

messageIdsAndTypes :: [(Int, SiteDataServiceMessageType)]
messageIdsAndTypes = [
    (1, GetSiteDataMessage),
    (2, SaveSiteDataMessage),
    (3, SiteNotFoundMessage),
    (4, DataNotAvailableMessage),
    (5, NonAuthoritativeServerMessage),
    (6, SiteDataFoundMessage),
    (7, SiteDataSavedMessage),
    (8, SiteDataSaveFailedMessage)
  ]

instance MessageType SiteDataServiceMessageType where
  messageTypeById = Map.fromList messageIdsAndTypes
  messageIdToType = flip Map.lookup messageTypeById
  messageIdByType = Map.fromList $ map swap messageIdsAndTypes
  messageTypeToId = (Map.!) messageIdByType

siteDataServiceConnectionData = ServiceConnectionData "192.168.1.101" 9800

data GSDResult = GSDSuccess (Text, Text, Int, [(Int, Text, Text, Text, Text, Text, Text)], Text)
               | GSDNotFound
               | GSDNotAvailable

getSiteDataFromSDS :: Text -> IO (GSDResult)
getSiteDataFromSDS siteId = do
  serviceRequestResult <- serviceRequest siteDataServiceConnectionData GetSiteDataMessage (siteId, (LT.pack "anivia"))
  case serviceRequestResult of
    Just (responseMessageType, responseMessageEncodedParams) ->
      case responseMessageType of
        SiteDataFoundMessage ->
          case unpackMessage responseMessageEncodedParams of
            Just siteData -> return $ GSDSuccess siteData
            Nothing -> do
              putStrLn $ "ERROR: getSiteDataFromSDS unable to unpack message " ++ show responseMessageType
              return GSDNotAvailable
        SiteNotFoundMessage -> return GSDNotFound
        DataNotAvailableMessage -> return GSDNotAvailable
        _ -> do
          putStrLn $ "ASSERT: getSiteDataFromSDS got unknown message type: " ++ show responseMessageType
          return GSDNotAvailable
    Nothing -> return GSDNotAvailable -- connection/read failure

data SSDResult = SSDSuccess | SSDNotAvailable

saveSiteDataToSDS :: Text -> (Text, Text, Int, [(Int, Text, Text, Text, Text, Text, Text)], Text) -> IO (SSDResult)
saveSiteDataToSDS currentSiteId siteData = do
  serviceRequestResult <- serviceRequest siteDataServiceConnectionData GetSiteDataMessage (currentSiteId, siteData)
  case serviceRequestResult of
    Just (responseMessageType, _) ->
      case responseMessageType of
        SiteDataSavedMessage -> return SSDSuccess
        SiteDataSaveFailedMessage -> return SSDNotAvailable
        _ -> do
          putStrLn $ "ASSERT: saveSiteDataToSDS got unknown message type: " ++ show responseMessageType
          return SSDNotAvailable
    Nothing -> return SSDNotAvailable -- connection/read failure

