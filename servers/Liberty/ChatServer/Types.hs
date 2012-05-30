{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Liberty.ChatServer.Types (
  ClientData(..),
  OtherClientData(..),
  ClientCustomerData(..),
  ClientOperatorData(..),
  ClientAdminData(..),
  ClientSuperAdminData(..),
  ChatSession(..),
  ChatSessionTVar,
  ChatOperatorEntry(..),
  ClientDataTVar,
  SiteId,
  SiteData(..),
  SiteOperatorData(..),
  SiteDataTVar,
  ClientSendChanMessage(..),
  ClientSendChan,
  SiteDataSaverChanMessage(..),
  SiteDataSaverChan,
  module Liberty.ChatServer.Constants
) where
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Network.Socket (Socket)
import Liberty.ChatServer.Constants

-- CONSIDER: Get rid of OtherClientData and ClientDataTVar in favor of UnregisteredDataTVar, CustomerDataTVar, OperatorDataTVar
-- Note: Think about how createAndSendMessage will be done in this case

-- Client
data ClientData = ClientData {
  cdSocket :: Socket,
  cdSendChan :: ClientSendChan,
  cdOtherData :: OtherClientData
} deriving (Show)
data OtherClientData = OCDClientUnregistered (Maybe SiteDataTVar) | OCDClientCustomerData ClientCustomerData | OCDClientOperatorData ClientOperatorData | OCDClientAdminData ClientAdminData | OCDClientSuperAdminData ClientSuperAdminData
  deriving (Show)
data ClientCustomerData = ClientCustomerData {
  ccdColor :: Text,
  ccdVisitorId :: Maybe Text,
  ccdCurrentPage :: Maybe Text,
  ccdReferrer :: Maybe Text,
  ccdSiteDataTVar :: SiteDataTVar,
  ccdChatSessionTVar :: ChatSessionTVar
} deriving (Show)
data ClientOperatorData = ClientOperatorData {
  codOperatorId :: Integer,
  codName :: Text,
  codColor :: Text,
  codTitle :: Text,
  codIconUrl :: Text,
  codSiteDataTVar :: SiteDataTVar,
  codChatSessions :: [ChatSessionTVar]
} deriving (Show)
data ClientAdminData = ClientAdminData {
  cadSiteDataTVar :: SiteDataTVar
} deriving (Show)
data ClientSuperAdminData = ClientSuperAdminData {
} deriving (Show)
data ChatSession = ChatSession {
  csId :: Integer,
  csCustomerClientDataTVar :: ClientDataTVar,
  csOperator :: ChatOperatorEntry,
  csMessagesWaiting :: [Text],
  csSiteDataTVar :: SiteDataTVar,
  csLastPositionUpdate :: Maybe Integer
} deriving (Show)
type ChatSessionTVar = TVar ChatSession
data ChatOperatorEntry = ChatOperatorNobody | ChatOperatorClient ClientDataTVar
  deriving (Show)
type ClientDataTVar = TVar ClientData

-- Site
type SiteId = Text
data SiteData = SiteData {
  sdSiteId :: SiteId,
  sdPlanId :: Int,
  sdName :: Text,
  sdAdminEmail :: Text,
  sdNextOperatorId :: Integer,
  sdOperators :: [SiteOperatorData],
  sdSessionsWaiting :: [ChatSessionTVar],
  sdOnlineOperators :: [ClientDataTVar],
  sdAdminPasswordHash :: Text,
  sdOnlineAdmins :: [ClientDataTVar],
  sdNextSessionId :: Integer
} deriving (Show)
data SiteOperatorData = SiteOperatorData {
  sodOperatorId :: Integer,
  sodUsername :: Text,
  sodPasswordHash :: Text,
  sodName :: Text,
  sodColor :: Text,
  sodTitle :: Text,
  sodIconUrl :: Text
} deriving (Show)

type SiteDataTVar = TVar SiteData

-- ClientSendChan
data ClientSendChanMessage = SendMessage ByteString | CloseSocket
type ClientSendChan = TChan ClientSendChanMessage

-- SiteDataSaver
data SiteDataSaverChanMessage = SaveSite SiteData
type SiteDataSaverChan = TChan SiteDataSaverChanMessage

-- Show instances
instance Show (TVar a) where
  show _ = "TVar"

instance Show (TChan a) where
  show _ = "TChan"

