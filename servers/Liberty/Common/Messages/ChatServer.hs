module Liberty.Common.Messages.ChatServer (
  ChatServerMessageType(..),
  getServiceConnectionDataForChatServer
) where
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Data.Tuple
import Liberty.Common.Messages
import Liberty.Common.ServiceClient

data ChatServerMessageType = CustomerJoinMessage
                           | CustomerInLinePositionMessage
                           | CustomerNowTalkingToMessage
                           | CustomerSendChatMessage
                           | CustomerReceiveChatMessage
                           | CustomerEndingChatMessage
                           | SomethingWentWrongMessage
                           | OperatorLoginRequestMessage
                           | OperatorLoginSuccessMessage
                           | OperatorLoginFailedMessage
                           | OperatorLineStatusDetailsMessage
                           | OperatorLineStatusEmptyMessage
                           | OperatorAcceptNextChatSessionMessage
                           | OperatorNowTalkingToMessage
                           | OperatorReceiveChatMessage
                           | OperatorSendChatMessage
                           | OperatorEndingChatMessage
                           | OperatorChatEndedMessage
                           | CustomerChatEndedMessage
                           | UnregisteredSelectSiteMessage
                           | UnregisteredSiteSelectedMessage
                           | UnregisteredSiteInvalidMessage
                           | CustomerNoOperatorsAvailableMessage
                           | AdminLoginRequestMessage
                           | AdminOperatorCreateMessage
                           | AdminOperatorReplaceMessage
                           | AdminOperatorDeleteMessage
                           | AdminLoginSuccessMessage
                           | AdminLoginFailedMessage
                           | AdminSiteInfoMessage
                           | AdminOperatorDetailsStartMessage
                           | AdminOperatorDetailsMessage
                           | AdminOperatorDetailsEndMessage
                           | AdminOperatorCreateSuccessMessage
                           | CSMTAdminSendOperatorWelcomeEmailSuccess
                           | AdminOperatorReplaceSuccessMessage
                           | CSMTUnregisteredActivateOperator
                           | AdminOperatorReplaceInvalidIdMessage
                           | CSMTAdminSetSiteInfoMessage
                           | CSMTAdminSetSiteInfoSuccessMessage
                           | CSSALoginRequestMessage
                           | CSSASiteCreateMessage
                           | CSSASiteDeleteMessage
                           | CSMTSAGetSiteInfo
                           | CSMTUnregisteredActivateOperatorSuccess
                           | CSSALoginSuccessMessage
                           | CSSALoginFailedMessage
                           | CSSASiteCreateSuccessMessage
                           | CSSASiteCreateDuplicateIdMessage
                           | CSSASiteDeleteSuccessMessage
                           | CSSASiteDeleteFailedMessage
                           | CSMTUnregisteredActivateOperatorFailure
                           | CSMTUnregisteredActivateAdminSuccess
                           | CSSASiteCreateUnavailableMessage
                           | CSMTAdminSendOperatorWelcomeEmail
                           | CSMTUnregisteredActivateAdminFailure
                           | CSMTVisitorJoinSuccess
                           | CSUnavailableMessage
                           | CSMTVisitorOnPage
                           | CSMTOperatorCustomerOnPage
                           | CSMTWrongChatServer
                           | AdminOperatorDeleteSuccessMessage
                           | AdminOperatorDeleteFailedMessage
                           | CSMTSASiteInfo
                           | CSMTInvalidSiteId
                           | CSMTSASetSitePlan
                           | CSMTSuccess
                           | CSMTFailure
                           | CSMTUnregisteredIsOperatorActivated
                           | CSMTUnregisteredIsOperatorActivatedResponse
                           | CSMTUnregisteredActivateAdmin
                           | CSMTUnregisteredClientIp
                           | CSMTOperatorCustomerLocation
                           | CSMTVisitorJoin
  deriving (Show, Eq, Ord)

messageIdsAndTypes :: [(Int, ChatServerMessageType)]
messageIdsAndTypes = [
    (1, CustomerJoinMessage),
    (2, CustomerInLinePositionMessage),
    (3, CustomerNowTalkingToMessage),
    (4, CustomerSendChatMessage),
    (5, CustomerReceiveChatMessage),
    (6, CustomerEndingChatMessage),
    (7, SomethingWentWrongMessage),
    (8, OperatorLoginRequestMessage),
    (9, OperatorLoginSuccessMessage),
    (10, OperatorLoginFailedMessage),
    (11, OperatorLineStatusDetailsMessage),
    (12, OperatorLineStatusEmptyMessage),
    (13, OperatorAcceptNextChatSessionMessage),
    (14, OperatorNowTalkingToMessage),
    (15, OperatorReceiveChatMessage),
    (16, OperatorSendChatMessage),
    (17, OperatorEndingChatMessage),
    (18, OperatorChatEndedMessage),
    (19, CustomerChatEndedMessage),
    (20, UnregisteredSelectSiteMessage),
    (21, UnregisteredSiteSelectedMessage),
    (22, UnregisteredSiteInvalidMessage),
    (23, CustomerNoOperatorsAvailableMessage),
    (24, AdminLoginRequestMessage),
    (25, AdminOperatorCreateMessage),
    (26, AdminOperatorReplaceMessage),
    (27, AdminOperatorDeleteMessage),
    (28, AdminLoginSuccessMessage),
    (29, AdminLoginFailedMessage),
    (30, AdminSiteInfoMessage),
    (31, AdminOperatorDetailsStartMessage),
    (32, AdminOperatorDetailsMessage),
    (33, AdminOperatorDetailsEndMessage),
    (34, AdminOperatorCreateSuccessMessage),
    (35, CSMTAdminSendOperatorWelcomeEmailSuccess),
    (36, AdminOperatorReplaceSuccessMessage),
    (37, CSMTUnregisteredActivateOperator),
    (38, CSMTAdminSetSiteInfoMessage),
    (39, CSMTAdminSetSiteInfoSuccessMessage),
    (40, AdminOperatorReplaceInvalidIdMessage),
    (41, CSSALoginRequestMessage),
    (42, CSSASiteCreateMessage),
    (43, CSSASiteDeleteMessage),
    (44, CSMTSAGetSiteInfo),
    (45, CSMTUnregisteredActivateOperatorSuccess),
    (46, CSSALoginSuccessMessage),
    (47, CSSALoginFailedMessage),
    (48, CSSASiteCreateSuccessMessage),
    (49, CSSASiteCreateDuplicateIdMessage),
    (50, CSSASiteDeleteSuccessMessage),
    (51, CSSASiteDeleteFailedMessage),
    (52, CSMTUnregisteredActivateOperatorFailure),
    (53, CSMTUnregisteredActivateAdminSuccess),
    (54, CSSASiteCreateUnavailableMessage),
    (55, CSMTAdminSendOperatorWelcomeEmail),
    (56, CSMTUnregisteredActivateAdminFailure),
    (57, CSMTVisitorJoinSuccess),
    (58, CSUnavailableMessage),
    (59, CSMTVisitorOnPage),
    (60, CSMTOperatorCustomerOnPage),
    (61, CSMTWrongChatServer),
    (62, AdminOperatorDeleteSuccessMessage),
    (63, AdminOperatorDeleteFailedMessage),
    (64, CSMTSASiteInfo),
    (65, CSMTInvalidSiteId),
    (66, CSMTSASetSitePlan),
    (67, CSMTSuccess),
    (68, CSMTFailure),
    (69, CSMTUnregisteredIsOperatorActivated),
    (70, CSMTUnregisteredIsOperatorActivatedResponse),
    (71, CSMTUnregisteredActivateAdmin),
    (72, CSMTUnregisteredClientIp),
    (73, CSMTOperatorCustomerLocation),
    (74, CSMTVisitorJoin)
  ]

instance MessageType ChatServerMessageType where
  messageTypeById = Map.fromList messageIdsAndTypes
  messageIdToType = flip Map.lookup messageTypeById
  messageIdByType = Map.fromList $ map swap messageIdsAndTypes
  messageTypeToId = (Map.!) messageIdByType

getServiceConnectionDataForChatServer :: Text -> ServiceConnectionData
getServiceConnectionDataForChatServer serverId = getLocalServiceConnectionData (LT.unpack serverId)

