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
        (CustomerSendChatMessage,[text]) -> handleCustomerSendChatMessage text clientCustomerData clientDataTVar
        _ -> do
          putStrLn "Client (Customer) sent an unknown command"
          atomically $ closeClientSocket clientDataTVar
    OCDClientOperatorData clientOperatorData ->
      case (messageType,params) of
        (OperatorAcceptNextChatSessionMessage,[]) -> handleOperatorAcceptNextChatSessionMessage clientDataTVar (codSiteDataTVar clientOperatorData)
        (OperatorSendChatMessage,[chatSessionIdT,text]) -> case parseIntegral chatSessionIdT of
          Just chatSessionId -> handleOperatorSendChatMessage chatSessionId text clientDataTVar (codChatSessions clientOperatorData)
          Nothing -> atomically $ closeClientSocket clientDataTVar
        _ -> do
          putStrLn "Client (Operator) sent an unknown command"
          atomically $ closeClientSocket clientDataTVar

handleCustomerJoin :: SiteId -> Text -> Text -> Text -> ClientDataTVar -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleCustomerJoin siteId name color icon clientDataTVar databaseHandleTVar siteMapTVar =
  withSiteDataTVar siteId clientDataTVar databaseHandleTVar siteMapTVar (\siteDataTVar -> atomically $ do
    siteData <- readTVar siteDataTVar
    let thisChatSessionId = sdNextSessionId siteData
    clientData <- readTVar clientDataTVar

    -- create a new chat session with this client as the customer and no operator
    chatSessionTVar <- newTVar $ ChatSession thisChatSessionId clientDataTVar ChatOperatorNobody [] [CLEJoin name color]
    writeTVar clientDataTVar $ clientData { cdOtherData = OCDClientCustomerData $ ClientCustomerData name color icon siteDataTVar chatSessionTVar }

    -- add the newly-created chat session to the site data's waiting list
    let newSessionsWaiting = sdSessionsWaiting siteData ++ [chatSessionTVar]
    writeTVar siteDataTVar $ siteData { sdSessionsWaiting = newSessionsWaiting, sdNextSessionId = thisChatSessionId + 1 }

    -- and notify all waiting clients and operators about the update (including sending the position to this customer)
    onWaitingListUpdated siteDataTVar
  )

onWaitingListUpdated :: SiteDataTVar -> STM ()
onWaitingListUpdated siteDataTVar = do
  -- first, send the line status info to all operators
  siteData <- readTVar siteDataTVar
  lineStatusInfo <- getLineStatusInfo siteDataTVar

  forM_ (sdOnlineOperators siteData) $ sendLineStatusInfoToOperator lineStatusInfo

  -- update the waiting customers with their new positions
  forM_ (zip ([1..] :: [Integer]) (sdSessionsWaiting siteData)) (\(positionInLine, chatSessionTVar) -> do
    chatSession <- readTVar chatSessionTVar
    let clientDataTVar = csCustomerClientDataTVar chatSession
    createAndSendMessage (CustomerInLinePositionMessage, [LT.pack $ show $ positionInLine]) clientDataTVar)

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

          -- send the line status
          lineStatusInfo <- getLineStatusInfo siteDataTVar
          sendLineStatusInfoToOperator lineStatusInfo clientDataTVar
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

handleCustomerSendChatMessage :: Text -> ClientCustomerData -> ClientDataTVar -> IO ()
handleCustomerSendChatMessage messageText clientCustomerData clientDataTVar = do
  atomically $ do
    let chatSessionTVar = ccdChatSessionTVar clientCustomerData
    chatSession <- readTVar $ chatSessionTVar
    writeTVar chatSessionTVar $ chatSession {
      csLog = (CLEMessage (ccdName clientCustomerData) (ccdColor clientCustomerData) messageText) : csLog chatSession,
      csMessagesWaiting = case csOperator chatSession of
        -- if there is no operator, buffer the message
        ChatOperatorNobody -> messageText : csMessagesWaiting chatSession
        -- otherwise, don't buffer the message since it'll be immediately sent to the operator
        _ -> csMessagesWaiting chatSession
    }
    case csOperator chatSession of
      ChatOperatorClient operatorClientDataTVar -> createAndSendMessage (OperatorReceiveChatMessage, [LT.pack $ show $ csId chatSession,messageText]) operatorClientDataTVar
      ChatOperatorNobody -> return () -- we have buffered the message above

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
          onWaitingListUpdated siteDataTVar
        
          -- send the CustomerNowTalkingToMessage to the customer
          createAndSendMessage (CustomerNowTalkingToMessage, [codName clientOperatorData, codColor clientOperatorData, codTitle clientOperatorData, codIconUrl clientOperatorData]) (csCustomerClientDataTVar updatedChatSession)
          
          -- send the OperatorNowTalkingToMessage to the operator
          createAndSendMessage (OperatorNowTalkingToMessage, [LT.pack $ show $ csId updatedChatSession, customerName, customerColor, customerIconUrl]) clientDataTVar

          -- send all csMessagesWaiting to the operator
          -- note: chatSession is a snapshot from before we emptied csMessagesWaiting
          forM_ (reverse $ csMessagesWaiting chatSession) (\messageText -> createAndSendMessage (OperatorReceiveChatMessage, [LT.pack $ show $ csId chatSession, messageText]) clientDataTVar)

        _ -> return $ trace "ASSERT: clientDataTVar contains a non-operator, but should have been pattern-matched by the caller" ()

    _ -> return () -- no waiting sessions, so do nothing

handleOperatorSendChatMessage :: Integer -> Text -> ClientDataTVar -> [ChatSessionTVar] -> IO ()
handleOperatorSendChatMessage chatSessionId text clientDataTVar chatSessionTVars =
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

handleClientExitEvent :: ClientDataTVar -> IO ()
handleClientExitEvent clientDataTVar = do
  atomically $ do
    clientData <- readTVar clientDataTVar
    case cdOtherData clientData of
      OCDClientCustomerData clientCustomerData -> do
        let chatSessionTVar = ccdChatSessionTVar clientCustomerData
        chatSession <- readTVar $ chatSessionTVar
        case csOperator chatSession of
          ChatOperatorNobody -> do
            -- customer exiting while on the waiting list
            let siteDataTVar = ccdSiteDataTVar clientCustomerData
            siteData <- readTVar siteDataTVar
            writeTVar siteDataTVar $ siteData {
              sdSessionsWaiting = filter (/= chatSessionTVar) $ sdSessionsWaiting siteData
            }
            onWaitingListUpdated siteDataTVar
          ChatOperatorClient operatorClientDataTVar ->
            -- customer exiting while talking to a client operator
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
      OCDClientUnregistered -> return () -- nothing to cleanup

endChatSession :: ChatSessionTVar -> STM ()
endChatSession chatSessionTVar = do
  chatSession <- readTVar chatSessionTVar
  -- notify the customer that the chat session has ended
  createAndSendMessage (CustomerChatEndedMessage, []) (csCustomerClientDataTVar chatSession)
  
  case csOperator chatSession of
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

