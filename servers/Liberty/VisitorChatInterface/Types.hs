{-# OPTIONS_GHC -fno-warn-orphans #-}

module Liberty.VisitorChatInterface.Types (
  VisitorDataProxyStatus(..),
  VisitorData(..),
  VisitorDataTVar,
  VisitorSessionData(..),
  VisitorSessionDataTVar,
  ChatSessionData(..),
  ChatSessionDataTVar,
  VisitorMapTVar,
  ProxySendChanMessage(..),
  ProxySendChan,
  MessageHandlerFunction
) where
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import qualified Data.Aeson as J
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Map (Map)

data VisitorDataProxyStatus = VDPSClosed | VDPSConnecting | VDPSConnected ProxySendChan
  deriving (Show)

data VisitorData = VisitorData {
  vdSessions :: Map Integer VisitorSessionDataTVar,
  vdNextSessionId :: Integer,
  vdChatSessions :: [ChatSessionDataTVar],
  vdProxyStatus :: VisitorDataProxyStatus,
  vdChatWindowOpen :: Bool, -- true if the visitor has the chat window expanded
  vdConnectionExpiryAbortTVar :: TVar Bool, -- timeout to make the visitor inactive when no sessions for some time
  vdVisitorExpiryAbortTVar :: TVar Bool -- this timeout expires the visitor completely, including the cached chat logs
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

-- ProxySendChan
data ProxySendChanMessage = SendMessage ByteString | CloseSocket
type ProxySendChan = TChan ProxySendChanMessage

type MessageHandlerFunction a b = a -> ByteString -> ProxySendChan -> b -> IO ()

-- Show instances
instance Show (TChan a) where
  show _ = "TChan"

instance Show (TVar a) where
  show _ = "TVar"

