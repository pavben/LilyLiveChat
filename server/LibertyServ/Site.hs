module LibertyServ.Site (
  SiteId,
  SiteData(..)
) where
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT

type SiteId = Integer
data SiteData = SiteData {
  sdSiteId :: SiteId,
  sdName :: Text
} deriving (Show)

