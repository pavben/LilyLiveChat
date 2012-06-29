{-# OPTIONS_GHC -fno-warn-orphans #-}

module Liberty.VisitorChatInterface.Types (
  VisitorDataProxyStatus(..),
  VisitorData(..),
  VisitorDataTVar,
  VisitorSessionData(..),
  VisitorSessionDataTVar,
  ChatSessionData(..),
  ChatSessionDataTVar,
  VisitorMapTVar
) where
import Control.Concurrent.STM.TVar
import qualified Data.Aeson as J
import Data.Text.Lazy (Text)
import Data.Map (Map)
import Network.Socket (Socket)

data VisitorDataProxyStatus = VDPSClosed | VDPSConnecting | VDPSConnected Socket
  deriving (Show)

data VisitorData = VisitorData {
  vdSessions :: Map Integer VisitorSessionDataTVar,
  vdNextSessionId :: Integer,
  vdChatSessions :: [ChatSessionDataTVar],
  vdProxyStatus :: VisitorDataProxyStatus,
  vdVisitorExpiryAbortTVar :: TVar Bool
} deriving (Show)

type VisitorDataTVar = TVar VisitorData

data VisitorSessionData = VisitorSessionData {
  vsdInSequence :: Integer,
  vsdOutSequence :: Integer,
  vsdOutgoingMessages :: [(Integer, [J.Value])],
  vsdLongPollAbortTVar :: TVar Bool,
  vsdVisitorSessionExpiryAbortTVar :: TVar Bool
} deriving (Show)

type VisitorSessionDataTVar = TVar VisitorSessionData

data ChatSessionData = ChatSessionData {
  csLog :: [Text] -- TODO: Use a ChatSessionLogEntry type
} deriving (Show)

type ChatSessionDataTVar = TVar ChatSessionData

type VisitorMap = Map Text VisitorDataTVar
type VisitorMapTVar = TVar VisitorMap

-- Show instances
instance Show (TVar a) where
  show _ = "TVar"

