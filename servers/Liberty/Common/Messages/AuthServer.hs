module Liberty.Common.Messages.AuthServer (
  AuthServerMessageType(..),
  VSResult(..),
  verifySessionId
) where
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import Data.Tuple
import Liberty.Common.Messages
import Liberty.Common.ServiceClient

data AuthServerMessageType = ASMTVerifySessionId
                           | ASMTVerifySessionIdSuccess
                           | ASMTVerifySessionIdFailure
  deriving (Show, Eq, Ord)

messageIdsAndTypes :: [(Int, AuthServerMessageType)]
messageIdsAndTypes = [
    (1, ASMTVerifySessionId),
    (2, ASMTVerifySessionIdSuccess),
    (3, ASMTVerifySessionIdFailure)
  ]

instance MessageType AuthServerMessageType where
  messageTypeById = Map.fromList messageIdsAndTypes
  messageIdToType = flip Map.lookup messageTypeById
  messageIdByType = Map.fromList $ map swap messageIdsAndTypes
  messageTypeToId = (Map.!) messageIdByType

authServerConnectionData :: ServiceConnectionData
authServerConnectionData = getLocalServiceConnectionData "auth"

data VSResult = VSSuccess Text Text -- userId, email
              | VSFailure
              | VSNotAvailable

verifySessionId :: Text -> IO (VSResult)
verifySessionId sessionId = do
  serviceRequestResult <- serviceRequest authServerConnectionData ASMTVerifySessionId (sessionId)
  case serviceRequestResult of
    Just (responseMessageType, responseMessageEncodedParams) ->
      case responseMessageType of
        ASMTVerifySessionIdSuccess ->
          case unpackMessage responseMessageEncodedParams of
            Just (userId, email) -> return $ VSSuccess userId email
            Nothing -> do
              putStrLn $ "ERROR: verifySessionId unable to unpack message " ++ show responseMessageType
              return VSNotAvailable
        ASMTVerifySessionIdFailure -> return VSFailure
        _ -> do
          putStrLn $ "ASSERT: verifySessionId got unknown message type: " ++ show responseMessageType
          return VSNotAvailable
    Nothing -> return VSNotAvailable -- connection/read failure

