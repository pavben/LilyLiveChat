{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Liberty.Server.Types (
  ClientData(..),
  OtherClientData(..),
  ClientCustomerData(..),
  ClientOperatorData(..),
  ChatSession(..),
  ChatSessionTVar,
  ChatOperatorEntry(..),
  ClientDataTVar,
  SiteId,
  SiteData(..),
  SiteOperatorData(..),
  SiteAdminData(..),
  SiteDataTVar,
  ClientSendChanMessage(..),
  ClientSendChan,
  DatabaseOperationQueueChanMessage(..),
  DatabaseOperationQueueChan,
  module Liberty.Common.Types
) where
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.Text.Lazy (Text)
import Network.Socket (Socket)
import Liberty.Common.Types

-- CONSIDER: Get rid of OtherClientData and ClientDataTVar in favor of UnregisteredDataTVar, CustomerDataTVar, OperatorDataTVar
-- Note: Think about how createAndSendMessage will be done in this case

-- Client
data ClientData = ClientData {
  cdSocket :: Socket,
  cdSendChan :: ClientSendChan,
  cdOtherData :: OtherClientData
} deriving (Show)
data OtherClientData = OCDClientUnregistered (Maybe SiteDataTVar) | OCDClientCustomerData ClientCustomerData | OCDClientOperatorData ClientOperatorData
  deriving (Show)
data ClientCustomerData = ClientCustomerData {
  ccdName :: Text,
  ccdColor :: Text,
  ccdIconUrl :: Text,
  ccdSiteDataTVar :: SiteDataTVar,
  ccdChatSessionTVar :: ChatSessionTVar
} deriving (Show)
data ClientOperatorData = ClientOperatorData {
  codName :: Text,
  codColor :: Text,
  codTitle :: Text,
  codIconUrl :: Text,
  codSiteDataTVar :: SiteDataTVar,
  codChatSessions :: [ChatSessionTVar]
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
  sdName :: Text,
  sdExpiryTimestamp :: Integer,
  sdNextOperatorId :: Integer,
  sdOperators :: [SiteOperatorData],
  sdNextAdminId :: Integer,
  sdAdmins :: [SiteAdminData],
  sdSessionsWaiting :: [ChatSessionTVar],
  sdOnlineOperators :: [ClientDataTVar],
  sdNextSessionId :: Integer
} deriving (Show)
data SiteOperatorData = SiteOperatorData {
  sodOperatorId :: Integer,
  sodUsername :: Text,
  sodPassword :: Text,
  sodName :: Text,
  sodColor :: Text,
  sodTitle :: Text,
  sodIconUrl :: Text
} deriving (Show)
data SiteAdminData = SiteAdminData {
  sadAdminId :: Integer,
  sadUsername :: Text,
  sadPassword :: Text
} deriving (Show)

type SiteDataTVar = TVar SiteData

-- ClientSendChan
data ClientSendChanMessage = SendMessage EncodedMessage | CloseSocket
type ClientSendChan = TChan ClientSendChanMessage

-- DatabaseOperationQueue
data DatabaseOperationQueueChanMessage = SaveSite SiteData
type DatabaseOperationQueueChan = TChan DatabaseOperationQueueChanMessage

-- Show instances
instance Show (TVar a) where
  show _ = "TVar"

instance Show (TChan a) where
  show _ = "TChan"

