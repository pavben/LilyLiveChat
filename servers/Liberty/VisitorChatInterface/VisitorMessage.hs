module Liberty.VisitorChatInterface.VisitorMessage (
  VisitorMessageType(..),
  sendMessageToVisitor,
  visitorMessageIdToType
) where
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import qualified Data.Aeson as J
import qualified Data.Map as Map
import Data.Tuple
import Liberty.VisitorChatInterface.Types

data VisitorMessageType = VMTJoinSuccess
  deriving (Show, Eq, Ord)

visitorMessageIdsAndTypes :: [(Int, VisitorMessageType)]
visitorMessageIdsAndTypes = [
    (1, VMTJoinSuccess)
  ]

visitorMessageTypeById :: Map.Map Int VisitorMessageType
visitorMessageTypeById = Map.fromList visitorMessageIdsAndTypes

visitorMessageIdToType :: Int -> Maybe VisitorMessageType
visitorMessageIdToType = flip Map.lookup visitorMessageTypeById

visitorMessageIdByType :: Map.Map VisitorMessageType Int
visitorMessageIdByType = Map.fromList $ map swap visitorMessageIdsAndTypes

visitorMessageTypeToId :: VisitorMessageType -> Int
visitorMessageTypeToId = (Map.!) visitorMessageIdByType

sendMessageToVisitor :: VisitorMessageType -> [J.Value] -> VisitorDataTVar -> STM ()
sendMessageToVisitor visitorMessageType messageParams visitorDataTVar =
  let
    messageData = (J.toJSON $ visitorMessageTypeToId visitorMessageType) : messageParams
  in do
    visitorData <- readTVar visitorDataTVar
    -- for each session
    forM_ (Map.elems $ vdSessions visitorData) $ \visitorSessionDataTVar -> do
      visitorSessionData <- readTVar visitorSessionDataTVar
      let newOutSequence = (vsdLastOutSequence visitorSessionData) + 1
      writeTVar visitorSessionDataTVar $ visitorSessionData {
        vsdLastOutSequence = newOutSequence,
        vsdOutgoingMessages = (vsdOutgoingMessages visitorSessionData) ++ [(newOutSequence, messageData)]
      }

    return ()

