module Liberty.Common.Messages.ChatServer (
  ChatServerMessageType(..)
) where
import qualified Data.Map as Map
import Data.Tuple
import Liberty.Common.Messages

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
                           | AdminOperatorCreateDuplicateUsernameMessage
                           | AdminOperatorReplaceSuccessMessage
                           | AdminOperatorReplaceDuplicateUsernameMessage
                           | AdminOperatorReplaceInvalidIdMessage
                           | AdminSetSiteNameMessage
                           | AdminSetSiteNameSuccessMessage
                           | SALoginRequestMessage
                           | SASiteCreateMessage
                           | SASiteDeleteMessage
                           | SAGetSiteInfoMessage
                           | SASetExpiryTimestampMessage
                           | SALoginSuccessMessage
                           | SALoginFailedMessage
                           | SASiteCreateSuccessMessage
                           | SASiteCreateFailedMessage
                           | SASiteDeleteSuccessMessage
                           | SASiteDeleteFailedMessage
                           | SASiteInfoMessage
                           | SASiteInfoFailedMessage
                           | SASetExpiryTimestampSuccessMessage
                           | AdminSetAdminPasswordMessage
                           | AdminSetAdminPasswordSuccessMessage
                           | AdminOperatorDeleteSuccessMessage
                           | AdminOperatorDeleteFailedMessage
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
    (35, AdminOperatorCreateDuplicateUsernameMessage),
    (36, AdminOperatorReplaceSuccessMessage),
    (37, AdminOperatorReplaceDuplicateUsernameMessage),
    (38, AdminSetSiteNameMessage),
    (39, AdminSetSiteNameSuccessMessage),
    (40, AdminOperatorReplaceInvalidIdMessage),
    (41, SALoginRequestMessage),
    (42, SASiteCreateMessage),
    (43, SASiteDeleteMessage),
    (44, SAGetSiteInfoMessage),
    (45, SASetExpiryTimestampMessage),
    (46, SALoginSuccessMessage),
    (47, SALoginFailedMessage),
    (48, SASiteCreateSuccessMessage),
    (49, SASiteCreateFailedMessage),
    (50, SASiteDeleteSuccessMessage),
    (51, SASiteDeleteFailedMessage),
    (52, SASiteInfoMessage),
    (53, SASiteInfoFailedMessage),
    (54, SASetExpiryTimestampSuccessMessage),
    (55, AdminSetAdminPasswordMessage),
    (56, AdminSetAdminPasswordSuccessMessage),
    (62, AdminOperatorDeleteSuccessMessage),
    (63, AdminOperatorDeleteFailedMessage)
  ]

instance MessageType ChatServerMessageType where
  messageTypeById = Map.fromList messageIdsAndTypes
  messageIdToType = flip Map.lookup messageTypeById
  messageIdByType = Map.fromList $ map swap messageIdsAndTypes
  messageTypeToId = (Map.!) messageIdByType

