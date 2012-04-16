module Liberty.WebChatInterface.MessageFormatConverter (
  createMessageFromJson
) where
import Control.Arrow (second)
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad.STM
import qualified Data.Aeson as J
import Data.Attoparsec.Number as DAN
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.HashMap.Strict as HMS
import Data.List
import qualified Data.Map as Map
import qualified Data.MessagePack as MP
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Vector as V
import Network.HTTP
import Network.Socket
import Network.URI
import Prelude hiding (catch)
import Safe
import Liberty.WebChatInterface.Sessions
import Liberty.Common.Messages
import Liberty.Common.Messages.ChatServer
import Liberty.Common.Utils

createMessageFromJson :: [J.Value] -> Maybe ByteString
createMessageFromJson jValues =
  case jValues of
    J.Number (DAN.I messageTypeId) : messageParamsAsJson ->
      case messageIdToType (fromInteger messageTypeId) :: Maybe ChatServerMessageType of
        Just messageType -> jsonToMP messageType messageParamsAsJson
        Nothing -> Nothing -- message id not recognized
    _ -> Nothing -- no message type or message type not a number

jsonToMP :: ChatServerMessageType -> [J.Value] -> Maybe ByteString

jsonToMP UnregisteredSelectSiteMessage [J.String siteId] =
  createMessage UnregisteredSelectSiteMessage (siteId)

jsonToMP CustomerJoinMessage [J.String name, J.String color, J.String iconUrl, J.String referrer] =
  createMessage CustomerJoinMessage (name, color, iconUrl, referrer)

jsonToMP CustomerSendChatMessage [J.String text] =
  createMessage CustomerSendChatMessage (text)

jsonToMP CustomerEndingChatMessage [] =
  createMessage CustomerEndingChatMessage ()

jsonToMP OperatorLoginRequestMessage [J.String username, J.String password] =
  createMessage OperatorLoginRequestMessage (username, password)

jsonToMP OperatorAcceptNextChatSessionMessage [] =
  createMessage OperatorAcceptNextChatSessionMessage ()

jsonToMP OperatorSendChatMessage [J.String sessionId, J.String text] =
  createMessage OperatorSendChatMessage (sessionId, text)

jsonToMP OperatorEndingChatMessage [J.String sessionId] =
  createMessage OperatorEndingChatMessage (sessionId)

jsonToMP AdminLoginRequestMessage [J.String password] =
  createMessage AdminLoginRequestMessage (password)

jsonToMP AdminOperatorCreateMessage [J.String username, J.String password, J.String name, J.String color, J.String title, J.String iconUrl] =
  createMessage AdminOperatorCreateMessage (username, password, name, color, title, iconUrl)

jsonToMP AdminOperatorReplaceMessage [J.Number (DAN.I operatorId), J.String username, J.String password, J.String name, J.String color, J.String title, J.String iconUrl] =
  createMessage AdminOperatorReplaceMessage (fromInteger operatorId :: Int, username, password, name, color, title, iconUrl)

jsonToMP AdminOperatorDeleteMessage [J.Number (DAN.I operatorId)] =
  createMessage AdminOperatorDeleteMessage (fromInteger operatorId :: Int)

jsonToMP AdminSetSiteNameMessage [J.String name] =
  createMessage AdminSetSiteNameMessage (name)

jsonToMP AdminSetAdminPasswordMessage [J.String password] =
  createMessage AdminSetAdminPasswordMessage (password)

