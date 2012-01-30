module Liberty.Server.Client (
  initializeClient
) where
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTI
import qualified Data.Text.Lazy.Read as LTR
import Debug.Trace
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Liberty.Common.NetworkMessage
import Liberty.Common.Utils
import Liberty.Server.DatabaseManager
import Liberty.Server.SiteMap
import Liberty.Server.Types
import Prelude hiding (catch)
import Safe

initializeClient :: Socket -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
initializeClient clientSocket databaseHandleTVar siteMapTVar = do
  clientSendChan <- atomically $ newTChan
  clientDataTVar <- atomically $ newTVar (ClientData clientSocket clientSendChan OCDClientUnregistered)
  finally
    (do
      _ <- forkIO $ clientSocketSendLoop clientSendChan clientDataTVar
      clientSocketReadLoop clientDataTVar LBS.empty databaseHandleTVar siteMapTVar
    )
    (do
      atomically $ writeTChan clientSendChan $ CloseSocket
      sClose clientSocket
      handleClientExitEvent clientDataTVar
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
        (CustomerJoinMessage,[siteId,name,color,icon]) -> handleCustomerJoin siteId name color icon clientDataTVar databaseHandleTVar siteMapTVar
        (OperatorLoginRequestMessage,[siteId,username,password]) -> handleOperatorLoginRequest siteId username password clientDataTVar databaseHandleTVar siteMapTVar
        _ -> do
          putStrLn "Client (Unregistered) sent an unknown command"
          atomically $ closeClientSocket clientDataTVar
    OCDClientCustomerData clientCustomerData ->
      case (messageType,params) of
        (ChatMessage,[text]) -> handleCustomerChatMessage text clientCustomerData clientDataTVar
        _ -> do
          putStrLn "Client (Customer) sent an unknown command"
          atomically $ closeClientSocket clientDataTVar
    OCDClientOperatorData clientOperatorData ->
      case (messageType,params) of
        (AcceptNextChatSessionMessage,[]) -> handleAcceptNextChatSessionMessage clientDataTVar (codSiteDataTVar clientOperatorData)
        _ -> do
          putStrLn "Client (Operator) sent an unknown command"
          atomically $ closeClientSocket clientDataTVar

handleCustomerJoin :: SiteId -> Text -> Text -> Text -> ClientDataTVar -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleCustomerJoin siteId name color icon clientDataTVar databaseHandleTVar siteMapTVar =
  -- TODO: make sure all fields are HTML-safe
  withSiteDataTVar siteId clientDataTVar databaseHandleTVar siteMapTVar (\siteDataTVar -> atomically $ do
    siteData <- readTVar siteDataTVar
    let thisChatSessionId = sdNextSessionId siteData
    clientData <- readTVar clientDataTVar

    -- create a new chat session with this client as the customer and no operator
    chatSessionTVar <- newTVar $ ChatSession thisChatSessionId clientDataTVar ChatOperatorNobody [CLEJoin name color]
    writeTVar clientDataTVar $ clientData { cdOtherData = OCDClientCustomerData $ ClientCustomerData name color icon siteDataTVar chatSessionTVar }

    -- add the newly-created chat session to the site data's waiting list
    let newSessionsWaiting = sdSessionsWaiting siteData ++ [chatSessionTVar]
    writeTVar siteDataTVar $ siteData { sdSessionsWaiting = newSessionsWaiting, sdNextSessionId = thisChatSessionId + 1 }

    -- and notify all waiting clients and operators about the update (including sending the position to this customer)
    onWaitingListUpdated siteDataTVar
  )

onWaitingListUpdated :: SiteDataTVar -> STM ()
onWaitingListUpdated siteDataTVar = do
  -- first, update the operators
  siteData <- readTVar siteDataTVar
  let onlineOperators = sdOnlineOperators siteData
  let sessionsWaiting = sdSessionsWaiting siteData
  maybeNextCustomerInfo <- case headMay sessionsWaiting of
    Just nextChatSessionTVar -> do
      nextChatSession <- readTVar $ nextChatSessionTVar
      nextChatSessionClientData <- readTVar $ csCustomerClientDataTVar nextChatSession
      case cdOtherData nextChatSessionClientData of
        OCDClientCustomerData clientCustomerData -> return $ Just (ccdName clientCustomerData, ccdColor clientCustomerData)
        _ -> return Nothing
    _ -> return Nothing

  case maybeNextCustomerInfo of
    Just (nextCustomerName, nextCustomerColor) -> forM_ onlineOperators $ createAndSendMessage (LineStatusUpdateMessage, [nextCustomerName, nextCustomerColor, LT.pack $ show $ length $ sessionsWaiting])
    Nothing -> forM_ onlineOperators $ createAndSendMessage (LineIsEmptyMessage, [])

  -- update the waiting customers with their new positions
  forM_ (zip [1..] sessionsWaiting) (\(positionInLine, chatSessionTVar) -> do
    chatSession <- readTVar chatSessionTVar
    let clientDataTVar = csCustomerClientDataTVar chatSession
    createAndSendMessage (InLinePositionMessage, [LT.pack $ show $ positionInLine]) clientDataTVar)

data OperatorLoginResult = OperatorLoginResultSuccess | OperatorLoginResultFailedMatch | OperatorLoginResultFailedDuplicate
handleOperatorLoginRequest :: SiteId -> Text -> Text -> ClientDataTVar -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleOperatorLoginRequest siteId username password clientDataTVar databaseHandleTVar siteMapTVar =
  let
    matchSiteOperatorCredentials siteOperatorInfo = (sodUsername siteOperatorInfo == username) && (sodPassword siteOperatorInfo == password)
  in
    withSiteDataTVar siteId clientDataTVar databaseHandleTVar siteMapTVar (\siteDataTVar -> atomically $ do
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
          -- Operator login successful
          -- update the site, adding the operator to it
          let newOnlineOperators = clientDataTVar : sdOnlineOperators siteData
          writeTVar siteDataTVar $ siteData { sdOnlineOperators = newOnlineOperators }

          -- update the client, associating it with the site
          clientData <- readTVar clientDataTVar
          writeTVar clientDataTVar $ clientData { cdOtherData = OCDClientOperatorData $ ClientOperatorData name color title iconUrl siteDataTVar [] }
          createAndSendMessage (OperatorLoginSuccessMessage, [name, color, title, iconUrl]) clientDataTVar
        Nothing -> do
          -- Operator login failed: Invalid credentials
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
          atomically $ do
            createAndSendMessage (SomethingWentWrongMessage, []) clientDataTVar
            closeClientSocket clientDataTVar
        LookupFailureTechnicalError -> do
          putStrLn "Lookup failed: Technical error"
          atomically $ do
            createAndSendMessage (SomethingWentWrongMessage, []) clientDataTVar
            closeClientSocket clientDataTVar

handleCustomerChatMessage :: Text -> ClientCustomerData -> ClientDataTVar -> IO ()
handleCustomerChatMessage messageText clientCustomerData clientDataTVar = do
  -- first, append the message to the log and retrieve the chat session operator value
  chatSessionOperator <- atomically $ do
    let chatSessionTVar = ccdChatSessionTVar clientCustomerData
    chatSession <- readTVar $ chatSessionTVar
    -- now that the message was sent to all appropriate parties, add it to the log
    let updatedChatLog = (CLEMessage (ccdName clientCustomerData) (ccdColor clientCustomerData) messageText) : csLog chatSession
    let updatedChatSession = chatSession { csLog = updatedChatLog }
    writeTVar chatSessionTVar $ updatedChatSession
    return $ csOperator updatedChatSession

  -- DEBUG
  log <- atomically $ do
    cSession <- readTVar $ ccdChatSessionTVar clientCustomerData
    return $ csLog cSession
  print log
  -- END DEBUG

  case chatSessionOperator of
    ChatOperatorClient operatorClientDataTVar -> do
      putStrLn "TODO: Support operators receiving messages"
    ChatOperatorNobody -> return () -- nothing to do

handleAcceptNextChatSessionMessage :: ClientDataTVar -> SiteDataTVar -> IO ()
handleAcceptNextChatSessionMessage clientDataTVar siteDataTVar = atomically $ do
  siteData <- readTVar siteDataTVar
  case sdSessionsWaiting siteData of
    chatSessionTVar:remainingChatSessionTVars -> do
      clientData <- readTVar clientDataTVar
      case cdOtherData clientData of
        OCDClientOperatorData clientOperatorData -> do
          -- add the chat session to the operator
          writeTVar clientDataTVar $ clientData {
            cdOtherData = OCDClientOperatorData $ clientOperatorData {
              codChatSessions = (codChatSessions clientOperatorData) ++ [chatSessionTVar]
            }
          }
          -- remove the chat session from sdSessionsWaiting
          writeTVar siteDataTVar $ siteData {
            sdSessionsWaiting = remainingChatSessionTVars
          }
          -- set the operator as the operator for this chat session
          chatSession <- readTVar chatSessionTVar
          writeTVar chatSessionTVar $ chatSession {
            csOperator = ChatOperatorClient clientDataTVar
          }
          -- update the operators with 'next in line' and waiting customers with their new position
          onWaitingListUpdated siteDataTVar
        
          -- send the NowTalkingToMessage to the customer
          createAndSendMessage (NowTalkingToMessage, [codName clientOperatorData, codColor clientOperatorData, codTitle clientOperatorData, codIconUrl clientOperatorData]) (csCustomerClientDataTVar chatSession)
          
          -- TODO: send the session added packet to the operator

        _ -> return () -- ASSERT: Already pattern matched by caller

    _ -> return () -- no waiting sessions, so do nothing

handleClientExitEvent :: ClientDataTVar -> IO ()
handleClientExitEvent clientDataTVar = do
  clientData <- atomically $ readTVar clientDataTVar
  case cdOtherData clientData of
    OCDClientCustomerData clientCustomerData -> do
      putStrLn "Client (Customer) exited"
      -- TODO: if there is an operator in the session, notify them that the customer has exited
      let chatSessionTVar = ccdChatSessionTVar clientCustomerData
      chatSession <- atomically $ readTVar $ chatSessionTVar
      case csOperator chatSession of
        ChatOperatorNobody -> do
          -- client exiting while on the waiting list
          let siteDataTVar = ccdSiteDataTVar clientCustomerData
          atomically $ do
            siteData <- readTVar siteDataTVar
            let newSessionsWaiting = filter (/= chatSessionTVar) $ sdSessionsWaiting siteData
            writeTVar siteDataTVar $ siteData { sdSessionsWaiting = newSessionsWaiting }

            onWaitingListUpdated siteDataTVar
        ChatOperatorClient operatorClientDataTVar -> do
          putStrLn "TODO: Client disconnected while talking to an operator"
    OCDClientOperatorData clientOperatorData -> do
      putStrLn "Client (Operator) exited"
      -- TODO: for each active chat session
      --   end it (for now?)
      --   consider re-queueing the customer (though perhaps we can't be reporting position in line then)
    OCDClientUnregistered -> do
      putStrLn "Client (Unregistered) exited -- nothing to cleanup"

createAndSendMessageIO :: Message -> ClientDataTVar -> IO ()
createAndSendMessageIO messageTypeAndParams clientDataTVar = atomically $ createAndSendMessage messageTypeAndParams clientDataTVar

createAndSendMessage :: Message -> ClientDataTVar -> STM ()
createAndSendMessage messageTypeAndParams clientDataTVar = do
  case createMessage messageTypeAndParams of
    Just encodedMessage -> do
      clientData <- readTVar clientDataTVar
      writeTChan (cdSendChan clientData) $ SendMessage encodedMessage
    Nothing -> return ()

closeClientSocket :: ClientDataTVar -> STM ()
closeClientSocket clientDataTVar = do
  clientData <- readTVar clientDataTVar
  writeTChan (cdSendChan clientData) CloseSocket

