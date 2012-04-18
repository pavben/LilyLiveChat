module Liberty.Common.Messages.SiteDataService (
  SiteDataServiceMessageType(..)
) where
import qualified Data.Map as Map
import Data.Tuple
import Liberty.Common.Messages

data SiteDataServiceMessageType = GetSiteDataMessage
                                | SaveSiteDataMessage
  deriving (Show, Eq, Ord)

messageIdsAndTypes :: [(Int, SiteDataServiceMessageType)]
messageIdsAndTypes = [
    (1, GetSiteDataMessage),
    (2, SaveSiteDataMessage)
  ]

instance MessageType SiteDataServiceMessageType where
  messageTypeById = Map.fromList messageIdsAndTypes
  messageIdToType = flip Map.lookup messageTypeById
  messageIdByType = Map.fromList $ map swap messageIdsAndTypes
  messageTypeToId = (Map.!) messageIdByType

