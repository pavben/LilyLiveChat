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
--import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
--import qualified Data.Text.Lazy.IO as LTI
--import qualified Data.Text.Lazy.Read as LTR
import Debug.Trace
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Liberty.Common.NetworkMessage
import Liberty.Common.Utils
import Liberty.Server.DatabaseManager
import Liberty.Server.SiteMap
import Liberty.Server.Types
import Liberty.Server.Utils
import Prelude hiding (catch)
import Safe

initializeClient :: Socket -> SiteMapTVar -> DatabaseOperationQueueChan -> IO ()
initializeClient clientSocket siteMapTVar databaseOperationQueueChan = do
  clientSendChan <- atomically $ newTChan
  clientDataTVar <- atomically $ newTVar (ClientData clientSocket clientSendChan (OCDClientUnregistered Nothing))
  finally
    (do
      _ <- forkIO $ clientSocketSendLoop clientSendChan clientDataTVar
      clientSocketReadLoop clientDataTVar LBS.empty siteMapTVar databaseOperationQueueChan
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
clientSocketReadLoop :: ClientDataTVar -> ByteString -> SiteMapTVar -> DatabaseOperationQueueChan -> IO ()
clientSocketReadLoop clientDataTVar buffer siteMapTVar databaseOperationQueueChan = do
  case parseMessage buffer of
    Just (maybeMessage, newBuffer) ->
      case maybeMessage of
        Just message -> do
          putStrLn $ "Msg: " ++ show message
          handleMessage message clientDataTVar siteMapTVar databaseOperationQueueChan
          clientSocketReadLoop clientDataTVar newBuffer siteMapTVar databaseOperationQueueChan
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
              clientSocketReadLoop clientDataTVar (LBS.append newBuffer receivedData) siteMapTVar databaseOperationQueueChan
            Nothing ->
              putStrLn $ "Client disconnecting -- recv returned nothing"
    Nothing ->
      putStrLn "Client disconnecting due to a protocol violation"

handleMessage :: Message -> ClientDataTVar -> SiteMapTVar -> DatabaseOperationQueueChan -> IO ()
handleMessage (messageType, params) clientDataTVar siteMapTVar databaseOperationQueueChan = do
  clientData <- atomically $ readTVar clientDataTVar
  case cdOtherData clientData of
    OCDClientUnregistered maybeSiteDataTVar ->
      case maybeSiteDataTVar of
        Nothing ->
          case (messageType,params) of
            (UnregisteredSelectSiteMessage,[siteId]) -> handleUnregisteredSelectSiteMessage siteId clientDataTVar siteMapTVar
            _ -> do
              putStrLn "Client (Unregistered) sent an unknown command"
              atomically $ closeClientSocket clientDataTVar
        Just siteDataTVar ->
          case (messageType,params) of
            (CustomerJoinMessage,[name,color,icon]) -> handleCustomerJoinMessage name color icon clientDataTVar siteDataTVar
            (OperatorLoginRequestMessage,[username,password]) -> handleOperatorLoginRequestMessage username password clientDataTVar siteDataTVar
            (AdminLoginRequestMessage,[password]) -> handleAdminLoginRequestMessage password clientDataTVar siteDataTVar
            _ -> do
              putStrLn "Client (Unregistered, with site selected) sent an unknown command"
              atomically $ closeClientSocket clientDataTVar
    OCDClientCustomerData clientCustomerData ->
      case (messageType,params) of
        (CustomerSendChatMessage,[text]) -> handleCustomerSendChatMessage text clientCustomerData
        (CustomerEndingChatMessage,[]) -> handleCustomerEndingChatMessage (ccdChatSessionTVar clientCustomerData)
        _ -> do
          putStrLn "Client (Customer) sent an unknown command"
          atomically $ closeClientSocket clientDataTVar
    OCDClientOperatorData clientOperatorData ->
      case (messageType,params) of
        (OperatorAcceptNextChatSessionMessage,[]) -> handleOperatorAcceptNextChatSessionMessage clientDataTVar (codSiteDataTVar clientOperatorData)
        (OperatorSendChatMessage,[chatSessionIdT,text]) -> case parseIntegral chatSessionIdT of
          Just chatSessionId -> handleOperatorSendChatMessage chatSessionId text (codChatSessions clientOperatorData)
          Nothing -> atomically $ closeClientSocket clientDataTVar
        (OperatorEndingChatMessage, [chatSessionIdT]) -> case parseIntegral chatSessionIdT of
          Just chatSessionId -> handleOperatorEndingChatMessage chatSessionId (codChatSessions clientOperatorData)
          Nothing -> atomically $ closeClientSocket clientDataTVar
        _ -> do
          putStrLn "Client (Operator) sent an unknown command"
          atomically $ closeClientSocket clientDataTVar
    OCDClientAdminData _ ->
      case (messageType,params) of
        (AdminOperatorCreateMessage,[username,password,name,color,title,iconUrl]) -> handleAdminOperatorCreateMessage username password name color title iconUrl clientDataTVar databaseOperationQueueChan
        (AdminOperatorReplaceMessage,[operatorIdT,username,password,name,color,title,iconUrl]) -> case parseIntegral operatorIdT of
          Just operatorId -> handleAdminOperatorReplaceMessage operatorId username password name color title iconUrl clientDataTVar databaseOperationQueueChan
          Nothing -> atomically $ closeClientSocket clientDataTVar
        (AdminOperatorDeleteMessage,[operatorIdT]) -> case parseIntegral operatorIdT of
          Just operatorId -> handleAdminOperatorDeleteMessage operatorId clientDataTVar databaseOperationQueueChan
          Nothing -> atomically $ closeClientSocket clientDataTVar
        (AdminSetSiteNameMessage,[name]) -> handleAdminSetSiteNameMessage name clientDataTVar databaseOperationQueueChan
        (AdminSetAdminPasswordMessage,[password]) -> handleAdminSetAdminPasswordMessage password clientDataTVar databaseOperationQueueChan
        _ -> do
          putStrLn "Client (Admin) sent an unknown command"
          atomically $ closeClientSocket clientDataTVar

handleUnregisteredSelectSiteMessage :: SiteId -> ClientDataTVar -> SiteMapTVar -> IO ()
handleUnregisteredSelectSiteMessage siteId clientDataTVar siteMapTVar = atomically $ do
  siteMap <- readTVar siteMapTVar
  case Map.lookup siteId siteMap of
    Just siteDataTVar -> do
      -- update the client with the new OCDClientUnregistered entry
      clientData <- readTVar clientDataTVar
      writeTVar clientDataTVar clientData { cdOtherData = OCDClientUnregistered (Just siteDataTVar) }

      -- send the appropriate message depending on whether or not there are any operators online
      siteData <- readTVar siteDataTVar
      let isActive = if null $ sdOnlineOperators siteData then "0" else "1"
      createAndSendMessage (UnregisteredSiteSelectedMessage,[sdName siteData, LT.pack $ isActive]) clientDataTVar
    Nothing -> createAndSendMessage (UnregisteredSiteInvalidMessage,[]) clientDataTVar

handleCustomerJoinMessage :: Text -> Text -> Text -> ClientDataTVar -> SiteDataTVar -> IO ()
handleCustomerJoinMessage name color icon clientDataTVar siteDataTVar = atomically $ do
  clientData <- readTVar clientDataTVar
  siteData <- readTVar siteDataTVar
  if not $ null $ sdOnlineOperators siteData then do
    let thisChatSessionId = sdNextSessionId siteData
    -- create a new chat session with this client as the customer and no operator
    chatSessionTVar <- newTVar $ ChatSession thisChatSessionId clientDataTVar ChatOperatorNobody [] siteDataTVar Nothing
    writeTVar clientDataTVar $ clientData { cdOtherData = OCDClientCustomerData $ ClientCustomerData name color icon siteDataTVar chatSessionTVar }

    -- add the newly-created chat session to the site data's waiting list
    let newSessionsWaiting = sdSessionsWaiting siteData ++ [chatSessionTVar]
    writeTVar siteDataTVar $ siteData { sdSessionsWaiting = newSessionsWaiting, sdNextSessionId = thisChatSessionId + 1 }

    -- and notify all waiting clients and operators about the update (including sending the position to this customer)
    waitingListUpdated siteDataTVar
  else do
    -- otherwise, there are no operators online, so we will not queue this customer
    createAndSendMessage (CustomerNoOperatorsAvailableMessage,[]) clientDataTVar
    closeClientSocket clientDataTVar

handleOperatorLoginRequestMessage :: Text -> Text -> ClientDataTVar -> SiteDataTVar -> IO ()
handleOperatorLoginRequestMessage username password clientDataTVar siteDataTVar =
  let
    matchSiteOperatorCredentials siteOperatorData = (sodUsername siteOperatorData == username) && (sodPasswordHash siteOperatorData == hashTextWithSalt password)
  in
    atomically $ do
      -- read the site data
      siteData <- readTVar siteDataTVar
      -- see if any operators match the given credentials
      maybeSiteOperatorData <- case filter matchSiteOperatorCredentials $ sdOperators siteData of
        [siteOperatorData] -> do
          -- Successful match
          return $ Just siteOperatorData
        [] -> do
          -- No match
          return Nothing
        _ -> do
          -- Multiple match -- data integrity error
          return Nothing

      case maybeSiteOperatorData of
        Just (SiteOperatorData operatorId _ _ name color title iconUrl) -> do
          -- Operator login successful
          -- update the site, adding the operator to it
          let newOnlineOperators = clientDataTVar : sdOnlineOperators siteData
          writeTVar siteDataTVar $ siteData { sdOnlineOperators = newOnlineOperators }

          -- update the client, associating it with the site
          clientData <- readTVar clientDataTVar
          writeTVar clientDataTVar $ clientData { cdOtherData = OCDClientOperatorData $ ClientOperatorData operatorId name color title iconUrl siteDataTVar [] }
          createAndSendMessage (OperatorLoginSuccessMessage, [name, color, title, iconUrl]) clientDataTVar

          -- send the line status
          lineStatusInfo <- getLineStatusInfo siteDataTVar
          sendLineStatusInfoToOperator lineStatusInfo clientDataTVar
        Nothing -> do
          -- Operator login failed: Invalid credentials
          createAndSendMessage (OperatorLoginFailedMessage, []) clientDataTVar
          closeClientSocket clientDataTVar

handleAdminLoginRequestMessage :: Text -> ClientDataTVar -> SiteDataTVar -> IO ()
handleAdminLoginRequestMessage password clientDataTVar siteDataTVar =
  atomically $ do
    -- read the site data
    siteData <- readTVar siteDataTVar
    if hashTextWithSalt password == sdAdminPasswordHash siteData then do
      -- Admin login successful
      -- update the site, adding the admin to it
      let newOnlineAdmins = clientDataTVar : sdOnlineAdmins siteData
      writeTVar siteDataTVar $ siteData { sdOnlineAdmins = newOnlineAdmins }

      -- update the client, associating it with the site
      clientData <- readTVar clientDataTVar
      writeTVar clientDataTVar $ clientData { cdOtherData = OCDClientAdminData $ ClientAdminData siteDataTVar }
      createAndSendMessage (AdminLoginSuccessMessage, []) clientDataTVar

      -- send the site info (siteId, name, expiryTimestamp)
      sendSiteInfoToAdmin siteData clientDataTVar

      -- send the operators list
      sendOperatorsListToAdmin siteData clientDataTVar
    else do
      -- Admin login failed: Invalid credentials
      createAndSendMessage (AdminLoginFailedMessage, []) clientDataTVar
      closeClientSocket clientDataTVar

handleCustomerSendChatMessage :: Text -> ClientCustomerData -> IO ()
handleCustomerSendChatMessage messageText clientCustomerData = do
  atomically $ do
    let chatSessionTVar = ccdChatSessionTVar clientCustomerData
    chatSession <- readTVar $ chatSessionTVar
    writeTVar chatSessionTVar $ chatSession {
      csMessagesWaiting = case csOperator chatSession of
        -- if there is no operator, buffer the message
        ChatOperatorNobody -> messageText : csMessagesWaiting chatSession
        -- otherwise, don't buffer the message since it'll be immediately sent to the operator
        _ -> csMessagesWaiting chatSession
    }
    case csOperator chatSession of
      ChatOperatorClient operatorClientDataTVar -> createAndSendMessage (OperatorReceiveChatMessage, [LT.pack $ show $ csId chatSession,messageText]) operatorClientDataTVar
      ChatOperatorNobody -> return () -- we have buffered the message above

handleCustomerEndingChatMessage :: ChatSessionTVar -> IO ()
handleCustomerEndingChatMessage chatSessionTVar = atomically $ endChatSession chatSessionTVar

handleOperatorAcceptNextChatSessionMessage :: ClientDataTVar -> SiteDataTVar -> IO ()
handleOperatorAcceptNextChatSessionMessage clientDataTVar siteDataTVar = atomically $ do
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
            csOperator = ChatOperatorClient clientDataTVar,
            csMessagesWaiting = []
          }
          -- read additional data that will be needed below
          updatedChatSession <- readTVar chatSessionTVar
          customerClientData <- readTVar (csCustomerClientDataTVar updatedChatSession)
          (customerName, customerColor, customerIconUrl) <- case cdOtherData customerClientData of
            OCDClientCustomerData clientCustomerData -> return (ccdName clientCustomerData, ccdColor clientCustomerData, ccdIconUrl clientCustomerData)
            _ -> return $ trace "ASSERT: csCustomerClientDataTVar contains a non-customer" (LT.empty, LT.empty, LT.empty)
          -- update the operators with 'next in line' and waiting customers with their new position
          waitingListUpdated siteDataTVar
        
          -- send the CustomerNowTalkingToMessage to the customer
          createAndSendMessage (CustomerNowTalkingToMessage, [codName clientOperatorData, codColor clientOperatorData, codTitle clientOperatorData, codIconUrl clientOperatorData]) (csCustomerClientDataTVar updatedChatSession)
          
          -- send the OperatorNowTalkingToMessage to the operator
          createAndSendMessage (OperatorNowTalkingToMessage, [LT.pack $ show $ csId updatedChatSession, customerName, customerColor, customerIconUrl]) clientDataTVar

          -- send all csMessagesWaiting to the operator
          -- note: chatSession is a snapshot from before we emptied csMessagesWaiting
          forM_ (reverse $ csMessagesWaiting chatSession) (\messageText -> createAndSendMessage (OperatorReceiveChatMessage, [LT.pack $ show $ csId chatSession, messageText]) clientDataTVar)

        _ -> return $ trace "ASSERT: clientDataTVar contains a non-operator, but should have been pattern-matched by the caller" ()

    _ -> return () -- no waiting sessions, so do nothing

handleOperatorSendChatMessage :: Integer -> Text -> [ChatSessionTVar] -> IO ()
handleOperatorSendChatMessage chatSessionId text chatSessionTVars =
  atomically $ do
    matchedSessions <- filterM (\chatSessionTVar -> do
      chatSession <- readTVar chatSessionTVar
      return $ chatSessionId == csId chatSession
      ) chatSessionTVars

    case matchedSessions of
      [chatSessionTVar] -> do
        chatSession <- readTVar chatSessionTVar
        createAndSendMessage (CustomerReceiveChatMessage, [text]) (csCustomerClientDataTVar chatSession)
      _ -> return () -- if no match or too many matches, do nothing (most likely, the session ended)

handleOperatorEndingChatMessage :: Integer -> [ChatSessionTVar] -> IO ()
handleOperatorEndingChatMessage chatSessionId chatSessionTVars = do
  atomically $ do
    matchedSessions <- filterM (\chatSessionTVar -> do
      chatSession <- readTVar chatSessionTVar
      return $ chatSessionId == csId chatSession
      ) chatSessionTVars

    case matchedSessions of
      [chatSessionTVar] -> endChatSession chatSessionTVar
      _ -> return () -- if no match or too many matches, do nothing (most likely, the session ended)

handleAdminOperatorCreateMessage :: Text -> Text -> Text -> Text -> Text -> Text -> ClientDataTVar -> DatabaseOperationQueueChan -> IO ()
handleAdminOperatorCreateMessage username password name color title iconUrl clientDataTVar databaseOperationQueueChan =
  atomically $ do
    clientData <- readTVar clientDataTVar
    case cdOtherData clientData of
      OCDClientAdminData clientAdminData -> do
        let siteDataTVar = cadSiteDataTVar clientAdminData
        siteData <- readTVar siteDataTVar
        if null $ filter (\siteOperatorData -> sodUsername siteOperatorData == username) (sdOperators siteData) then do
          let newSiteOperatorData = SiteOperatorData (sdNextOperatorId siteData) username (hashTextWithSalt password) name color title iconUrl
          -- save siteDataTVar with the new operator and sdNextOperatorId
          writeTVar siteDataTVar $ siteData {
            sdOperators = newSiteOperatorData : sdOperators siteData,
            sdNextOperatorId = sdNextOperatorId siteData + 1
          }
          -- save to the database
          queueSaveSiteData siteDataTVar databaseOperationQueueChan
          -- respond to the admin who issued the create command
          createAndSendMessage (AdminOperatorCreateSuccessMessage,[]) clientDataTVar
          -- notify the admins with the new list
          sendOperatorsListToAdmins siteDataTVar
        else
          createAndSendMessage (AdminOperatorCreateDuplicateUsernameMessage,[]) clientDataTVar
      _ -> return $ trace "ASSERT: Expecting OCDClientAdminData in handleAdminOperatorCreateMessage" ()

handleAdminOperatorReplaceMessage :: Integer -> Text -> Text -> Text -> Text -> Text -> Text -> ClientDataTVar -> DatabaseOperationQueueChan -> IO ()
handleAdminOperatorReplaceMessage operatorId username password name color title iconUrl clientDataTVar databaseOperationQueueChan =
  -- TODO: validate the length of all inputs
  atomically $ do
    clientData <- readTVar clientDataTVar
    case cdOtherData clientData of
      OCDClientAdminData clientAdminData -> do
        let siteDataTVar = cadSiteDataTVar clientAdminData
        siteData <- readTVar siteDataTVar
        case partition (\siteOperatorData -> sodOperatorId siteOperatorData == operatorId) (sdOperators siteData) of
          ([oldSiteOperatorData], remainingOperators) ->
            -- exactly 1 operator matched the search
            if null $ filter (\siteOperatorData -> sodUsername siteOperatorData == username) remainingOperators then do
              -- the new password hash is either the same as the old (if no new password given) or a hash of the new password
              let newPasswordHash = if LT.null password then sodPasswordHash oldSiteOperatorData else hashTextWithSalt password
              -- the new username does not collide with any other operators
              let newSiteOperatorData = SiteOperatorData operatorId username newPasswordHash name color title iconUrl
              -- save siteDataTVar with the new operator
              writeTVar siteDataTVar $ siteData {
                sdOperators = newSiteOperatorData : remainingOperators
              }
              -- save to the database
              queueSaveSiteData siteDataTVar databaseOperationQueueChan
              -- respond to the admin who issued the replace command
              createAndSendMessage (AdminOperatorReplaceSuccessMessage,[]) clientDataTVar
              -- notify the admins with the new list
              sendOperatorsListToAdmins siteDataTVar
            else
              -- an operator with that username already exists
              createAndSendMessage (AdminOperatorReplaceDuplicateUsernameMessage,[]) clientDataTVar
          _ ->
            -- operator with the given operatorId does not exist (or duplicate? shouldn't be possible)
            createAndSendMessage (AdminOperatorReplaceInvalidIdMessage,[]) clientDataTVar
      _ -> return $ trace "ASSERT: Expecting OCDClientAdminData in handleAdminOperatorReplaceMessage" ()

handleAdminOperatorDeleteMessage :: Integer -> ClientDataTVar -> DatabaseOperationQueueChan -> IO ()
handleAdminOperatorDeleteMessage operatorId clientDataTVar databaseOperationQueueChan =
  atomically $ do
    clientData <- readTVar clientDataTVar
    case cdOtherData clientData of
      OCDClientAdminData clientAdminData -> do
        let siteDataTVar = cadSiteDataTVar clientAdminData
        siteData <- readTVar siteDataTVar
        let (operatorsRemoved, remainingOperators) = partition (\siteOperatorData -> sodOperatorId siteOperatorData == operatorId) (sdOperators siteData)
        if length operatorsRemoved == 1 then do
          -- exactly 1 operator matched the search
          -- see if the deleted operator is currently online and kick them out if so
          forM_ (sdOnlineOperators siteData) (\operatorClientDataTVar -> do
            currentOperatorClientData <- readTVar operatorClientDataTVar
            case cdOtherData currentOperatorClientData of
              OCDClientOperatorData clientOperatorData ->
                if codOperatorId clientOperatorData == operatorId then
                  -- disconnect the matching operator
                  closeClientSocket operatorClientDataTVar
                else
                  -- do nothing
                  return ()
              _ -> return $ trace "ASSERT: Expecting OCDClientOperatorData in handleAdminOperatorDeleteMessage" ()
            )
          -- save siteDataTVar with remainingOperators
          writeTVar siteDataTVar $ siteData {
            sdOperators = remainingOperators
          }
          -- save to the database
          queueSaveSiteData siteDataTVar databaseOperationQueueChan
          -- respond to the admin who issued the delete command
          createAndSendMessage (AdminOperatorDeleteSuccessMessage,[]) clientDataTVar
          -- notify the admins with the new list
          sendOperatorsListToAdmins siteDataTVar
        else
          -- operator with the given operatorId does not exist (or duplicate? shouldn't be possible)
          createAndSendMessage (AdminOperatorDeleteFailedMessage,[]) clientDataTVar
      _ -> return $ trace "ASSERT: Expecting OCDClientAdminData in handleAdminOperatorDeleteMessage" ()

handleAdminSetSiteNameMessage :: Text -> ClientDataTVar -> DatabaseOperationQueueChan -> IO ()
handleAdminSetSiteNameMessage name clientDataTVar databaseOperationQueueChan =
  atomically $ do
    if LT.length name <= 20 then do
      clientData <- readTVar clientDataTVar
      case cdOtherData clientData of
        OCDClientAdminData clientAdminData -> do
          let siteDataTVar = cadSiteDataTVar clientAdminData
          siteData <- readTVar siteDataTVar

          -- save siteDataTVar with the new name
          writeTVar siteDataTVar $ siteData {
            sdName = name
          }

          -- save to the database
          queueSaveSiteData siteDataTVar databaseOperationQueueChan

          -- respond to the admin who issued the delete command
          createAndSendMessage (AdminSetSiteNameSuccessMessage,[]) clientDataTVar

          -- notify the admins of the new site name
          sendSiteInfoToAdmins siteDataTVar
        _ -> return $ trace "ASSERT: Expecting OCDClientAdminData in handleAdminSetSiteNameMessage" ()
    else
      -- if the browser allowed them to set an extra long site name, something is wrong
      createAndSendMessage (SomethingWentWrongMessage,[]) clientDataTVar

handleAdminSetAdminPasswordMessage :: Text -> ClientDataTVar -> DatabaseOperationQueueChan -> IO ()
handleAdminSetAdminPasswordMessage password clientDataTVar databaseOperationQueueChan =
  atomically $ do
    if LT.length password <= 100 then do
      clientData <- readTVar clientDataTVar
      case cdOtherData clientData of
        OCDClientAdminData clientAdminData -> do
          let siteDataTVar = cadSiteDataTVar clientAdminData
          siteData <- readTVar siteDataTVar

          -- save siteDataTVar with the new name
          writeTVar siteDataTVar $ siteData {
            sdAdminPasswordHash = hashTextWithSalt password
          }

          -- save to the database
          queueSaveSiteData siteDataTVar databaseOperationQueueChan

          -- respond to the admin who issued the set password command
          createAndSendMessage (AdminSetAdminPasswordSuccessMessage,[]) clientDataTVar

          -- TODO: disconnect all other admins
        _ -> return $ trace "ASSERT: Expecting OCDClientAdminData in handleAdminSetAdminPasswordMessage" ()
    else
      -- if the browser allowed them to set an extra long password, something is wrong
      createAndSendMessage (SomethingWentWrongMessage,[]) clientDataTVar

sendOperatorsListToAdmins :: SiteDataTVar -> STM ()
sendOperatorsListToAdmins siteDataTVar = do
  -- read the site data
  siteData <- readTVar siteDataTVar

  -- call sendOperatorsListToAdmin on all admins
  forM_ (sdOnlineAdmins siteData) $ sendOperatorsListToAdmin siteData

sendOperatorsListToAdmin :: SiteData -> ClientDataTVar -> STM ()
sendOperatorsListToAdmin siteData adminClientDataTVar = do
  -- send the operator details start message
  createAndSendMessage (AdminOperatorDetailsStartMessage,[]) adminClientDataTVar

  -- send the operator details, one message per operator
  forM_ (sdOperators siteData) $ (\siteOperatorData ->
    createAndSendMessage (AdminOperatorDetailsMessage,
      [
        LT.pack $ show $ sodOperatorId siteOperatorData,
        sodUsername siteOperatorData,
        sodName siteOperatorData,
        sodColor siteOperatorData,
        sodTitle siteOperatorData,
        sodIconUrl siteOperatorData
      ]
      ) adminClientDataTVar
    )

  -- send the operator details end message
  createAndSendMessage (AdminOperatorDetailsEndMessage,[]) adminClientDataTVar

sendSiteInfoToAdmins :: SiteDataTVar -> STM ()
sendSiteInfoToAdmins siteDataTVar = do
  -- read the site data
  siteData <- readTVar siteDataTVar
  -- send the operator details start message to all admins
  forM_ (sdOnlineAdmins siteData) $ sendSiteInfoToAdmin siteData

sendSiteInfoToAdmin :: SiteData -> ClientDataTVar -> STM ()
sendSiteInfoToAdmin siteData adminClientDataTVar = createAndSendMessage (AdminSiteInfoMessage,[sdSiteId siteData, sdName siteData, LT.pack $ show $ sdExpiryTimestamp siteData]) adminClientDataTVar

handleClientExitEvent :: ClientDataTVar -> IO ()
handleClientExitEvent clientDataTVar = do
  atomically $ do
    clientData <- readTVar clientDataTVar
    case cdOtherData clientData of
      OCDClientCustomerData clientCustomerData -> do
        let chatSessionTVar = ccdChatSessionTVar clientCustomerData
        endChatSession chatSessionTVar
      OCDClientOperatorData clientOperatorData -> do
        -- end all chat sessions
        mapM_ endChatSession $ codChatSessions clientOperatorData
        
        -- remove the operator from sdOnlineOperators
        let siteDataTVar = codSiteDataTVar clientOperatorData
        siteData <- readTVar siteDataTVar
        writeTVar siteDataTVar $ siteData {
          sdOnlineOperators = filter (/= clientDataTVar) $ sdOnlineOperators siteData
        }
        onlineOperatorsListUpdated siteDataTVar
      OCDClientAdminData clientAdminData -> do
        -- remove the admin from sdOnlineAdmins
        let siteDataTVar = cadSiteDataTVar clientAdminData
        siteData <- readTVar siteDataTVar
        writeTVar siteDataTVar $ siteData {
          sdOnlineAdmins = filter (/= clientDataTVar) $ sdOnlineAdmins siteData
        }
      OCDClientUnregistered _ -> return () -- nothing to cleanup

waitingListUpdated :: SiteDataTVar -> STM ()
waitingListUpdated siteDataTVar = do
  -- first, send the line status info to all operators
  siteData <- readTVar siteDataTVar
  lineStatusInfo <- getLineStatusInfo siteDataTVar

  forM_ (sdOnlineOperators siteData) $ sendLineStatusInfoToOperator lineStatusInfo

  -- update the waiting customers with their new positions
  forM_ (zip ([1..] :: [Integer]) (sdSessionsWaiting siteData)) (\(positionInLine, chatSessionTVar) -> do
    chatSession <- readTVar chatSessionTVar
    case csLastPositionUpdate chatSession of
      Just lastPositionUpdate ->
        -- if the last update sent doesn't match the current position, send the update
        if lastPositionUpdate /= positionInLine then
          sendPositionUpdate chatSessionTVar chatSession positionInLine
        else
          return ()
      Nothing ->
        -- if this is the first update, send it
        sendPositionUpdate chatSessionTVar chatSession positionInLine
    )
  where
    sendPositionUpdate chatSessionTVar chatSession positionInLine = do
      createAndSendMessage (CustomerInLinePositionMessage, [LT.pack $ show $ positionInLine]) (csCustomerClientDataTVar chatSession)
      writeTVar chatSessionTVar $ chatSession { csLastPositionUpdate = Just $ positionInLine }

-- CONSIDER: When the last operator becomes unavailable, allow 2 minutes or so before kicking everyone off
onlineOperatorsListUpdated :: SiteDataTVar -> STM ()
onlineOperatorsListUpdated siteDataTVar = do
  siteData <- readTVar siteDataTVar
  if null $ sdOnlineOperators siteData then do
    -- list everyone who is waiting in line
    customerClientDataTVars <- mapM (\chatSessionTVar -> do
      chatSession <- readTVar chatSessionTVar
      return $ csCustomerClientDataTVar chatSession
      ) (sdSessionsWaiting siteData)
    -- send the messages
    forM_ customerClientDataTVars $ createAndSendMessage (CustomerNoOperatorsAvailableMessage,[])
    -- remove all clients from the line
    writeTVar siteDataTVar $ siteData { sdSessionsWaiting = [] }
    -- disconnect them
    mapM_ closeClientSocket customerClientDataTVars
    -- since we updated the waiting list
    waitingListUpdated siteDataTVar
  else
    return ()

data LineStatusInfo = LineStatusInfo (Maybe (Text, Text)) Int

getLineStatusInfo :: SiteDataTVar -> STM LineStatusInfo
getLineStatusInfo siteDataTVar = do
  siteData <- readTVar siteDataTVar
  let sessionsWaiting = sdSessionsWaiting siteData
  maybeNextCustomerInfo <- case headMay sessionsWaiting of
    Just nextChatSessionTVar -> do
      nextChatSession <- readTVar $ nextChatSessionTVar
      nextChatSessionClientData <- readTVar $ csCustomerClientDataTVar nextChatSession
      case cdOtherData nextChatSessionClientData of
        OCDClientCustomerData clientCustomerData -> return $ Just (ccdName clientCustomerData, ccdColor clientCustomerData)
        _ -> return Nothing
    _ -> return Nothing
  return $ LineStatusInfo maybeNextCustomerInfo (length sessionsWaiting)

sendLineStatusInfoToOperator :: LineStatusInfo -> ClientDataTVar -> STM ()
sendLineStatusInfoToOperator (LineStatusInfo maybeNextCustomerInfo numCustomersInLine) clientDataTVar =
  let
    message = case maybeNextCustomerInfo of
      Just (nextCustomerName, nextCustomerColor) -> (OperatorLineStatusDetailsMessage, [nextCustomerName, nextCustomerColor, LT.pack $ show $ numCustomersInLine])
      Nothing -> (OperatorLineStatusEmptyMessage, [])
  in
    createAndSendMessage message clientDataTVar

endChatSession :: ChatSessionTVar -> STM ()
endChatSession chatSessionTVar = do
  chatSession <- readTVar chatSessionTVar
  -- notify the customer that the chat session has ended
  createAndSendMessage (CustomerChatEndedMessage, []) (csCustomerClientDataTVar chatSession)
  
  case csOperator chatSession of
    ChatOperatorNobody -> do
      -- session ended while on the waiting list
      let siteDataTVar = csSiteDataTVar chatSession
      siteData <- readTVar siteDataTVar
      writeTVar siteDataTVar $ siteData {
        sdSessionsWaiting = filter (/= chatSessionTVar) $ sdSessionsWaiting siteData
      }
      waitingListUpdated siteDataTVar
    ChatOperatorClient operatorClientDataTVar -> do
      -- remove the operator from the session
      writeTVar chatSessionTVar $ chatSession { csOperator = ChatOperatorNobody }

      -- remove the session from the operator
      operatorClientData <- readTVar operatorClientDataTVar
      case cdOtherData operatorClientData of
        OCDClientOperatorData clientOperatorData -> do
          writeTVar operatorClientDataTVar $ operatorClientData {
            cdOtherData = OCDClientOperatorData clientOperatorData {
              codChatSessions = filter (/= chatSessionTVar) $ codChatSessions clientOperatorData
            }
          }
        _ -> trace "ASSERT: operatorClientData contains a non-operator" $ return ()
      -- notify the operator that the chat session has ended
      createAndSendMessage (OperatorChatEndedMessage, [LT.pack $ show $ csId chatSession]) operatorClientDataTVar

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

