module Liberty.Common.Messages.SiteLocatorService (
  SiteLocatorServiceMessageType(..),
  SLResult(..),
  locateSite
) where
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import Data.Tuple
import Liberty.Common.Messages
import Liberty.Common.ServiceClient

data SiteLocatorServiceMessageType = LocateSiteMessage
                                   | SiteLocatedMessage
                                   | SiteLocateFailedMessage
  deriving (Show, Eq, Ord)

messageIdsAndTypes :: [(Int, SiteLocatorServiceMessageType)]
messageIdsAndTypes = [
    (1, LocateSiteMessage),
    (2, SiteLocatedMessage),
    (3, SiteLocateFailedMessage)
  ]

instance MessageType SiteLocatorServiceMessageType where
  messageTypeById = Map.fromList messageIdsAndTypes
  messageIdToType = flip Map.lookup messageTypeById
  messageIdByType = Map.fromList $ map swap messageIdsAndTypes
  messageTypeToId = (Map.!) messageIdByType

siteLocatorServiceConnectionData :: ServiceConnectionData
siteLocatorServiceConnectionData = ServiceConnectionData "192.168.1.100" 9800

data SLResult = SLSuccess Text -- this does not imply that the site exists, rather that if it existed, it would be on the given server
              | SLNotAvailable -- either SiteLocatorService is down or there are no servers available to assign the site to

locateSite :: Text -> IO (SLResult)
locateSite siteId = do
  serviceRequestResult <- serviceRequest siteLocatorServiceConnectionData LocateSiteMessage (siteId)
  case serviceRequestResult of
    Just (responseMessageType, responseMessageEncodedParams) ->
      case responseMessageType of
        SiteLocatedMessage ->
          case unpackMessage responseMessageEncodedParams of
            Just serverId -> return $ SLSuccess serverId
            Nothing -> do
              putStrLn $ "ERROR: locateSite unable to unpack message " ++ show responseMessageType
              return SLNotAvailable
        SiteLocateFailedMessage -> return SLNotAvailable
        _ -> do
          putStrLn $ "ASSERT: locateSite got unknown message type: " ++ show responseMessageType
          return SLNotAvailable
    Nothing -> return SLNotAvailable -- connection/read failure

