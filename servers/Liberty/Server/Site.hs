module Liberty.Server.Site (
  SiteId,
  SiteData(..)
) where
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Data.Word

type SiteId = Integer
data SiteData = SiteData {
  sdSiteId :: SiteId,
  sdName :: Text
--  sdSiteOps :: [SiteOp]
} deriving (Show)

