module Liberty.Server.Client (
  initializeClient
) where
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
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
import Liberty.Server.SiteMap
import Liberty.Server.Types

initializeClient :: Socket -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
initializeClient clientSocket databaseHandleTVar siteMapTVar = do
  clientSendChan <- atomically $ newTChan
  finally
    (do
      clientDataTVar <- atomically $ newTVar (ClientData clientSocket clientSendChan OCDClientUnregistered)
      _ <- forkIO $ clientSocketSendLoop clientSendChan clientDataTVar
      clientSocketReadLoop clientDataTVar LBS.empty databaseHandleTVar siteMapTVar
    )
    (do
      atomically $ writeTChan clientSendChan $ CloseSocket
      sClose clientSocket
    )

clientSocketSendLoop :: ClientSendChan -> ClientDataTVar -> IO ()
clientSocketSendLoop clientSendChan clientDataTVar = do
  clientSendChanMessage <- atomically $ readTChan clientSendChan
  clientData <- atomically $ readTVar clientDataTVar
  let clientSocket = cdSocket clientData
  case clientSendChanMessage of
    SendMessage encodedMessage -> do
      sendSuccess <- catch
        (do
          sendAll clientSocket encodedMessage
          return True
        )
        (\(SomeException ex) -> do
          putStrLn $ "Exception on sendAll: " ++ show ex ++ " -- closing socket."
          return False
        )
      if sendSuccess then
        clientSocketSendLoop clientSendChan clientDataTVar
      else
        sClose clientSocket
    CloseSocket -> do
      putStrLn "Got CloseSocket message. Closing client socket."
      sClose clientSocket

-- TODO: DoS vulnerability: Filling the buffer until out of memory
clientSocketReadLoop :: ClientDataTVar -> ByteString -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
clientSocketReadLoop clientDataTVar buffer databaseHandleTVar siteMapTVar = do
  case parseMessage buffer of
    Just (maybeMessage, newBuffer) ->
      case maybeMessage of
        Just message -> do
          putStrLn $ "Msg: " ++ show message
          handleMessage message clientDataTVar databaseHandleTVar siteMapTVar
          clientSocketReadLoop clientDataTVar newBuffer databaseHandleTVar siteMapTVar
        Nothing -> do
          putStrLn "No valid message in current buffer yet"
          clientData <- atomically $ readTVar clientDataTVar
          maybeReceivedData <- catch (do
            recvResult <- recv (cdSocket clientData) 2048
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
              clientSocketReadLoop clientDataTVar (LBS.append newBuffer receivedData) databaseHandleTVar siteMapTVar
            Nothing ->
              putStrLn $ "Client disconnecting -- recv returned nothing"
    Nothing ->
      putStrLn "Client disconnecting due to a protocol violation"

handleMessage :: Message -> ClientDataTVar -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleMessage (messageType, params) clientDataTVar databaseHandleTVar siteMapTVar = do
  clientData <- atomically $ readTVar clientDataTVar
  case cdOtherData clientData of
    OCDClientUnregistered ->
      case (messageType,params) of
        (GuestJoinMessage,[siteId,name,color,icon]) -> handleGuestJoin siteId name color icon clientDataTVar databaseHandleTVar siteMapTVar
        (OperatorLoginRequestMessage,[siteId,username,password]) -> handleOperatorLoginRequest siteId username password clientDataTVar databaseHandleTVar siteMapTVar
        _ -> do
          putStrLn "Client (Unregistered) sent an unknown command"
          closeClientSocket clientDataTVar
    OCDClientGuestData clientGuestData ->
      case (messageType,params) of
        (ChatMessage,[text]) -> handleGuestChatMessage text clientGuestData clientDataTVar
        _ -> do
          putStrLn "Client (Guest) sent an unknown command"
          closeClientSocket clientDataTVar
    OCDClientOperatorData clientOperatorData ->
      case (messageType,params) of
        (ChatMessage,[text]) -> handleOperatorChatMessage text clientOperatorData clientDataTVar
        _ -> do
          putStrLn "Client (Operator) sent an unknown command"
          closeClientSocket clientDataTVar

handleGuestJoin :: SiteId -> Text -> Text -> Text -> ClientDataTVar -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleGuestJoin siteId name color icon clientDataTVar databaseHandleTVar siteMapTVar = do
  lookupResult <- lookupSite databaseHandleTVar siteMapTVar siteId
  case lookupResult of
    Right siteDataTVar -> do
      positionInLine <- atomically $ do
        clientData <- readTVar clientDataTVar
        -- create a new chat session with this client as the guest and no operator
        chatSessionTVar <- newTVar $ ChatSession clientDataTVar ChatOperatorNobody [CLEJoin name color]
        writeTVar clientDataTVar $ clientData { cdOtherData = OCDClientGuestData $ ClientGuestData name color icon siteDataTVar chatSessionTVar }
        -- add the newly-created chat session to the site data's waiting list
        siteData <- readTVar siteDataTVar
        let newSessionsWaiting = sdSessionsWaiting siteData ++ [chatSessionTVar]
        writeTVar siteDataTVar $ siteData { sdSessionsWaiting = newSessionsWaiting }
        return $ length newSessionsWaiting
      createAndSendMessage (InLinePositionMessage, [LT.pack $ show $ positionInLine]) clientDataTVar
      -- DEBUG START
      newCD <- atomically $ readTVar clientDataTVar
      print newCD
      -- DEBUG END
      -- TODO: make sure all fields are HTML-safe
    Left lookupFailureReason ->
      case lookupFailureReason of
        LookupFailureNotExist -> do
          putStrLn "Lookup failed: Site does not exist"
          -- TODO: respond with a more specific error
          createAndSendMessage (SomethingWentWrongMessage, []) clientDataTVar
          closeClientSocket clientDataTVar
        LookupFailureTechnicalError -> do
          putStrLn "Lookup failed: Technical error"
          createAndSendMessage (SomethingWentWrongMessage, []) clientDataTVar
          closeClientSocket clientDataTVar

data OperatorLoginResult = OperatorLoginResultSuccess | OperatorLoginResultFailedMatch | OperatorLoginResultFailedDuplicate
handleOperatorLoginRequest :: SiteId -> Text -> Text -> ClientDataTVar -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleOperatorLoginRequest siteId username password clientDataTVar databaseHandleTVar siteMapTVar =
  let
    matchSiteOperatorCredentials siteOperatorInfo = (sodUsername siteOperatorInfo == username) && (sodPassword siteOperatorInfo == password)
  in
    withSiteDataTVar siteId clientDataTVar databaseHandleTVar siteMapTVar (\siteDataTVar -> do
      operatorLoginResult <- atomically $ do
        -- read the site data
        siteData <- readTVar siteDataTVar
        -- see if any operators match the given credentials
        maybeSiteOperatorInfo <- case filter matchSiteOperatorCredentials $ sdOperators siteData of
          [siteOperatorInfo] -> do
            -- Successful match
            return $ Just siteOperatorInfo
          [] -> do
            -- No match
            return Nothing
          _ -> do
            -- Multiple match -- data integrity error
            return Nothing

        case maybeSiteOperatorInfo of
          Just (SiteOperatorInfo _ _ name color title iconUrl) -> do
            -- update the site, adding the operator to it
            let newOnlineOperators = clientDataTVar : sdOnlineOperators siteData
            writeTVar siteDataTVar $ siteData { sdOnlineOperators = newOnlineOperators }

            -- update the client, associating it with the site
            clientData <- readTVar clientDataTVar
            writeTVar clientDataTVar $ clientData { cdOtherData = OCDClientOperatorData $ ClientOperatorData name color title iconUrl siteDataTVar [] }
            return $ Just (name, color, title, iconUrl)
          Nothing -> return Nothing -- could not authenticate the user

      case operatorLoginResult of
        -- successful login
        Just (name, color, title, iconUrl) -> do
          putStrLn "Operator login successful"
          createAndSendMessage (OperatorLoginSuccessMessage, [name, color, title, iconUrl]) clientDataTVar
        -- failed login
        Nothing -> do
          putStrLn "Operator login failed: Invalid credentials"
          createAndSendMessage (OperatorLoginFailedMessage, []) clientDataTVar
          closeClientSocket clientDataTVar
    )

withSiteDataTVar :: SiteId -> ClientDataTVar -> DatabaseHandleTVar -> SiteMapTVar -> (SiteDataTVar -> IO ()) -> IO ()
withSiteDataTVar siteId clientDataTVar databaseHandleTVar siteMapTVar f = do
  lookupResult <- lookupSite databaseHandleTVar siteMapTVar siteId
  case lookupResult of
    Right siteDataTVar -> f siteDataTVar
    Left lookupFailureReason ->
      case lookupFailureReason of
        LookupFailureNotExist -> do
          putStrLn "Lookup failed: Site does not exist"
          -- TODO: respond with a more specific error
          createAndSendMessage (SomethingWentWrongMessage, []) clientDataTVar
          closeClientSocket clientDataTVar
        LookupFailureTechnicalError -> do
          putStrLn "Lookup failed: Technical error"
          createAndSendMessage (SomethingWentWrongMessage, []) clientDataTVar
          closeClientSocket clientDataTVar

handleGuestChatMessage :: Text -> ClientGuestData -> ClientDataTVar -> IO ()
handleGuestChatMessage messageText clientGuestData clientDataTVar = do
  -- first, append the message to the log and retrieve the chat session operator value
  chatSessionOperator <- atomically $ do
    let chatSessionTVar = cgdChatSession clientGuestData
    chatSession <- readTVar $ chatSessionTVar
    -- now that the message was sent to all appropriate parties, add it to the log
    let updatedChatLog = (CLEMessage (cgdName clientGuestData) (cgdColor clientGuestData) messageText) : csLog chatSession
    let updatedChatSession = chatSession { csLog = updatedChatLog }
    writeTVar chatSessionTVar $ updatedChatSession
    return $ csOperator updatedChatSession

  -- DEBUG
  log <- atomically $ do
    cSession <- readTVar $ cgdChatSession clientGuestData
    return $ csLog cSession
  print log
  -- END DEBUG

  case chatSessionOperator of
    ChatOperatorClient operatorClientDataTVar -> do
      putStrLn "TODO: Support operators receiving messages"
    ChatOperatorNobody -> return () -- nothing to do

handleOperatorChatMessage :: Text -> ClientOperatorData -> ClientDataTVar -> IO ()
handleOperatorChatMessage messageText clientOperatorData clientDataTVar = do
  putStrLn "TODO"

createAndSendMessage :: Message -> ClientDataTVar -> IO ()
createAndSendMessage messageTypeAndParams clientDataTVar = do
  case createMessage messageTypeAndParams of
    Just encodedMessage -> do
      clientData <- atomically $ readTVar clientDataTVar
      atomically $ writeTChan (cdSendChan clientData) $ SendMessage encodedMessage
    Nothing -> return ()

closeClientSocket :: ClientDataTVar -> IO ()
closeClientSocket clientDataTVar = do
  clientData <- atomically (readTVar clientDataTVar)
  sClose $ cdSocket clientData

