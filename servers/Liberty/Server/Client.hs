module Liberty.Server.Client (
  initializeClient
) where
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTI
import qualified Data.Text.Lazy.Read as LTR
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import Liberty.Common.NetworkMessage
import Liberty.Common.Utils
import Liberty.Server.DatabaseManager
import Liberty.Server.Messages.ClientChan
import Liberty.Server.Site
import Liberty.Server.SiteMap

data ClientGuestData' = ClientGuestData' {
  cgdSiteId :: SiteId,
  cgdName :: Text,
  cgdColor :: Text,
  cgdIconUrl :: Text
} deriving (Show)

data OtherClientData = ClientUnregistered | ClientGuestData ClientGuestData'
  deriving (Show)

data ClientData = ClientData {
  cdSocket :: Maybe Socket,
  cdOtherData :: OtherClientData
} deriving (Show)

initializeClient :: Socket -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
initializeClient clientSocket databaseHandleTVar siteMapTVar =
  finally
    (clientSocketReadLoop (ClientData (Just clientSocket) ClientUnregistered) LBS.empty databaseHandleTVar siteMapTVar)
    (sClose clientSocket)

-- TODO: DoS vulnerability: Filling the buffer until out of memory
clientSocketReadLoop :: ClientData -> ByteString -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
clientSocketReadLoop clientData buffer databaseHandleTVar siteMapTVar = do
  case parseMessage buffer of
    Just (maybeMessage, newBuffer) ->
      case maybeMessage of
        Just message -> do
          putStrLn $ "Msg: " ++ show message
          newClientData <- handleMessage message clientData databaseHandleTVar siteMapTVar
          clientSocketReadLoop newClientData newBuffer databaseHandleTVar siteMapTVar
        Nothing -> do
          putStrLn "No valid message in current buffer yet"
          case cdSocket clientData of
            Just clientSocket -> do
              maybeReceivedData <- catch (do
                recvResult <- recv clientSocket 2048
                if not $ LBS.null recvResult then do
                  return $ Just recvResult
                else do
                  return Nothing
                )
                (\(SomeException ex) -> do
                  putStrLn $ "Client disconnecting due to exception: " ++ show ex
                  return Nothing
                )

              case maybeReceivedData of
                Just receivedData ->
                  -- now that we've received some data, loop around and try parsing it
                  clientSocketReadLoop clientData (LBS.append newBuffer receivedData) databaseHandleTVar siteMapTVar
                Nothing ->
                  putStrLn $ "Client disconnecting -- recv returned nothing"
            Nothing -> do
              putStrLn "Client disconnecting -- we've already closed the socket"
    Nothing ->
      putStrLn "Client disconnecting due to a protocol violation"

handleMessage :: Message -> ClientData -> DatabaseHandleTVar -> SiteMapTVar -> IO ClientData
handleMessage (messageType, params) clientData databaseHandleTVar siteMapTVar =
  case (messageType,params) of
    (GuestJoinMessage,[siteIdT,name,color,icon]) -> do
      case parseIntegral siteIdT of
        Just siteId -> handleGuestJoin siteId name color icon clientData databaseHandleTVar siteMapTVar
        Nothing -> do
          putStrLn "Numeric conversion failed!"
          closeClientSocket clientData
    _ -> do
      putStrLn "No match"
      closeClientSocket clientData

handleGuestJoin :: SiteId -> Text -> Text -> Text -> ClientData -> DatabaseHandleTVar -> SiteMapTVar -> IO ClientData
handleGuestJoin siteId name color icon clientData databaseHandleTVar siteMapTVar = do
  lookupResult <- lookupSite databaseHandleTVar siteMapTVar siteId
  case lookupResult of
    Right siteData -> do
      putStrLn $ "Obtained site data from db: " ++ show siteData
      -- start debug
      _ <- createAndSendMessage (NowTalkingToMessage, [LT.pack "Joe"]) clientData
      _ <- createAndSendMessage (NowTalkingToMessage, [LT.pack "Joe"]) clientData
      createAndSendMessage (NowTalkingToMessage, [LT.pack "Joe"]) clientData
      -- end debug
      -- TODO: make sure all fields are HTML-safe
    Left lookupFailureReason ->
      case lookupFailureReason of
        LookupFailureNotExist -> do
          putStrLn "Lookup failed: Site does not exist"
          -- TODO: respond with a more specific error
          createAndSendMessage (SomethingWentWrongMessage, []) clientData >>= closeClientSocket
        LookupFailureTechnicalError -> do
          putStrLn "Lookup failed: Technical error"
          createAndSendMessage (SomethingWentWrongMessage, []) clientData >>= closeClientSocket

createAndSendMessage :: Message -> ClientData -> IO ClientData
createAndSendMessage messageTypeAndParams clientData =
  case createMessage messageTypeAndParams of
    Just encodedMessage -> sendMessage clientData encodedMessage
    Nothing -> return clientData

sendMessage :: ClientData -> ByteString -> IO ClientData
sendMessage clientData byteString =
  case cdSocket clientData of
    Just clientSocket ->
      catch
        (do
          sendAll clientSocket byteString
          return clientData
        )
        (\(SomeException e) -> do
          putStrLn "Exception on sendMessage. Closing socket."
          closeClientSocket clientData
        )
    Nothing -> return clientData -- if the socket is already closed, simply return

closeClientSocket :: ClientData -> IO ClientData
closeClientSocket clientData = case cdSocket clientData of
  Just clientSocket -> do
    sClose clientSocket
    return clientData { cdSocket = Nothing }
  Nothing -> return clientData -- else we return the unchanged client data

