module Liberty.AuthServer.Types (
  UserData(..),
  SessionEntry(..),
  SessionEntryTVar,
  SessionMap,
  SessionMapTVar
) where
import Control.Concurrent.STM.TVar
import Data.Text.Lazy (Text)
import Data.Map (Map)

data UserData = UserData {
  udUserId :: Text,
  udEmail :: Text,
  udGoogleIdentifier :: Text
} deriving (Show)

-- session map
data SessionEntry = SessionEntry {
  seUserId :: Text,
  seEmail :: Text
} deriving (Show)

type SessionEntryTVar = TVar SessionEntry

type SessionMap = Map Text SessionEntry
type SessionMapTVar = TVar SessionMap

