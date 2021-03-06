{-# OPTIONS_GHC -fno-warn-orphans #-}

module Liberty.SiteDataService.Types (
  SiteId,
  SiteData(..),
  SiteOperatorData(..),
  ClientSendChanMessage(..),
  ClientSendChan
) where
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)

-- Site
type SiteId = Text
data SiteData = SiteData {
  sdSiteId :: SiteId,
  sdPlanId :: Int,
  sdName :: Text,
  sdNextOperatorId :: Integer,
  sdOperators :: [SiteOperatorData],
  sdAdminUserIds :: [Text]
} deriving (Show)

data SiteOperatorData = SiteOperatorData {
  sodOperatorId :: Integer,
  sodName :: Text,
  sodColor :: Text,
  sodTitle :: Text,
  sodIconUrl :: Text,
  sodUserId :: Maybe Text,
  sodActivationToken :: Maybe Text
} deriving (Show)

-- ClientSendChan
data ClientSendChanMessage = SendMessage ByteString | CloseSocket
type ClientSendChan = TChan ClientSendChanMessage

-- Show instances
instance Show (TVar a) where
  show _ = "TVar"

instance Show (TChan a) where
  show _ = "TChan"

