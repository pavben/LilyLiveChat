{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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
  Plan(..),
  getPlanById,
  getPlanIdForPlan,
  getMaxOperatorsForPlan,
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
  sdPlan :: Plan,
  sdName :: Text,
  sdNextOperatorId :: Integer,
  sdOperators :: [SiteOperatorData],
  sdOnlineOperators :: [ClientDataTVar],
  sdAdminUserIds :: [Text],
  sdOnlineAdmins :: [ClientDataTVar],
  sdNextSessionId :: Integer,
  sdSessionsWaiting :: [ChatSessionTVar]
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

type SiteDataTVar = TVar SiteData

data Plan = FreePlan | SilverPlan | GoldPlan | PlatinumPlan
  deriving (Show)

getPlanById :: Int -> Maybe Plan
getPlanById 0 = Just $ FreePlan
getPlanById 1 = Just $ SilverPlan
getPlanById 2 = Just $ GoldPlan
getPlanById 3 = Just $ PlatinumPlan
getPlanById _ = Nothing

getPlanIdForPlan :: Plan -> Int
getPlanIdForPlan FreePlan = 0
getPlanIdForPlan SilverPlan = 1
getPlanIdForPlan GoldPlan = 2
getPlanIdForPlan PlatinumPlan = 3

getMaxOperatorsForPlan :: Plan -> Int
getMaxOperatorsForPlan FreePlan = 1
getMaxOperatorsForPlan SilverPlan = 1
getMaxOperatorsForPlan GoldPlan = 4
getMaxOperatorsForPlan PlatinumPlan = 7

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

