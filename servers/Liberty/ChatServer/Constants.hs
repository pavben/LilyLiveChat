module Liberty.ChatServer.Constants where
import Data.Int

-- Text value limits
maxAdminPasswordLength :: Int64
maxAdminPasswordLength = 100

maxSiteIdLength :: Int64
maxSiteIdLength = 20

maxSiteNameLength :: Int64
maxSiteNameLength = 20

maxEmailLength :: Int64
maxEmailLength = 256

maxOperatorUsernameLength :: Int64
maxOperatorUsernameLength = 30

maxOperatorPasswordLength :: Int64
maxOperatorPasswordLength = 100

maxPersonNameLength :: Int64
maxPersonNameLength = 20

maxColorLength :: Int64
maxColorLength = 7 -- #abcdef

maxOperatorTitleLength :: Int64
maxOperatorTitleLength = 25

maxIconUrlLength :: Int64
maxIconUrlLength = 250

-- TODO PL: Enforce this limit on the client side
-- CONSIDER: Instead of limiting individual messages, limit all traffic from the client
maxChatMessageLength :: Int64
maxChatMessageLength = 20000

maxReceiveBufferLength :: Int64
maxReceiveBufferLength = maxChatMessageLength + 1000

