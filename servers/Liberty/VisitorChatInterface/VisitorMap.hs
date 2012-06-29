module Liberty.VisitorChatInterface.VisitorMap (
  createVisitorMapTVar,
  createVisitor,
  createVisitorSession,
  resetVisitorSessionExpiry,
  deleteVisitorSession
) where
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (recv)
import Liberty.Common.Messages
import Liberty.Common.Messages.ChatServer
import Liberty.Common.RandomString
import Liberty.Common.ServiceClient
import Liberty.Common.Timeouts
import Liberty.VisitorChatInterface.ProxyConnection
import Liberty.VisitorChatInterface.Types

createVisitorMapTVar :: IO VisitorMapTVar
createVisitorMapTVar = atomically $ newTVar Map.empty

createVisitor :: VisitorMapTVar -> IO (Text, VisitorDataTVar)
createVisitor visitorMapTVar = do
  newVisitorId <- getRandomAlphanumericText 10
  maybeVisitorDataTVar <- atomically $ do
    visitorMap <- readTVar visitorMapTVar
    case Map.lookup newVisitorId visitorMap of
      Just _ -> return Nothing
      Nothing -> do
        visitorExpiryAbortTVar <- newTVar True
        newVisitorDataTVar <- newTVar $ VisitorData Map.empty 0 [] VDPSClosed visitorExpiryAbortTVar
        writeTVar visitorMapTVar $ Map.insert newVisitorId newVisitorDataTVar visitorMap
        return $ Just newVisitorDataTVar

  case maybeVisitorDataTVar of
    Just visitorDataTVar -> do
      resetVisitorExpiry visitorDataTVar newVisitorId visitorMapTVar
      return (newVisitorId, visitorDataTVar)
    Nothing -> createVisitor visitorMapTVar -- retry

resetVisitorExpiry :: VisitorDataTVar -> Text -> VisitorMapTVar -> IO ()
resetVisitorExpiry visitorDataTVar visitorId visitorMapTVar = do
  (oldVisitorExpiryAbortTVar, newVisitorExpiryAbortTVar) <- atomically $ do
    visitorData <- readTVar visitorDataTVar
    let oldVisitorExpiryAbortTVar' = vdVisitorExpiryAbortTVar visitorData
    newVisitorExpiryAbortTVar' <- newTVar False
    writeTVar visitorDataTVar $ visitorData { vdVisitorExpiryAbortTVar = newVisitorExpiryAbortTVar' }
    return (oldVisitorExpiryAbortTVar', newVisitorExpiryAbortTVar')

  -- abort the old expiry timeout
  void $ abortTimeout oldVisitorExpiryAbortTVar

  -- create the new timeout
  setTimeout (24 * 60 * 60) newVisitorExpiryAbortTVar $ deleteVisitor visitorDataTVar visitorId visitorMapTVar

deleteVisitor :: VisitorDataTVar -> Text -> VisitorMapTVar -> IO ()
deleteVisitor visitorDataTVar visitorId visitorMapTVar = do
  putStrLn "Visitor cleanup triggered"
  maybeProxySocket <- atomically $ do
    visitorData <- readTVar visitorDataTVar

    -- remove the visitor from the visitor map
    visitorMap <- readTVar visitorMapTVar
    writeTVar visitorMapTVar $ Map.delete visitorId visitorMap
    
    -- return the proxy socket so that it can be closed
    return $ case vdProxyStatus visitorData of
      VDPSConnected proxySocket -> Just proxySocket
      _ -> Nothing

  -- close the proxy socket, if any
  case maybeProxySocket of
    Just proxySocket -> sClose proxySocket
    Nothing -> return ()

  return ()

createVisitorSession :: VisitorDataTVar -> Text -> VisitorMapTVar -> IO (Integer, VisitorSessionDataTVar)
createVisitorSession visitorDataTVar visitorId visitorMapTVar = do
  (visitorSessionId, visitorSessionDataTVar, mustInitiateConnection) <- atomically $ do
    longPollAbortTVar <- newTVar False
    visitorSessionExpiryAbortTVar <- newTVar False
    visitorSessionDataTVar' <- newTVar $ VisitorSessionData 0 0 [] longPollAbortTVar visitorSessionExpiryAbortTVar
    visitorData <- readTVar visitorDataTVar
    let
      (newProxyStatus, mustInitiateConnection') =
        case vdProxyStatus visitorData of
          VDPSClosed -> (VDPSConnecting, True)
          currentProxyStatus -> (currentProxyStatus, False)
    let visitorSessionId' = vdNextSessionId visitorData
    writeTVar visitorDataTVar $ visitorData {
      vdSessions = Map.insert visitorSessionId' visitorSessionDataTVar' (vdSessions visitorData),
      vdNextSessionId = visitorSessionId' + 1,
      vdProxyStatus = newProxyStatus
    }
    return $ (visitorSessionId', visitorSessionDataTVar', mustInitiateConnection')

  when mustInitiateConnection $ void $ forkIO $ do
    -- TODO PL: server "anivia" is hardcoded
    maybeProxySocket <- establishServiceConnection (getServiceConnectionDataForChatServer (LT.pack "anivia"))
    case maybeProxySocket of
      Just proxySocket ->
        let
          onCloseCallback =
            atomically $ do
              visitorData <- readTVar visitorDataTVar
              writeTVar visitorDataTVar $ visitorData { vdProxyStatus = VDPSClosed }
        in do
          proxySendChan <- initializeProxyConnection proxySocket handleMessageFromProxy visitorDataTVar onCloseCallback
          return ()
          -- TODO: Send the client's IP
          {-
            -- new session request
            case lookupHeader (HdrCustom "X-Real-IP") $ rqHeaders request of
              Just (LT.pack -> clientIp) ->
                handleNewSession sessionMapTVar handleStream clientIp
              Nothing ->
                return () -- X-Real-IP header missing
          -}
      Nothing -> return ()

    -- update vdProxyStatus with the appropriate value based on maybeProxySocket
    atomically $ do
      visitorData <- readTVar visitorDataTVar
      writeTVar visitorDataTVar $ visitorData {
        vdProxyStatus = case maybeProxySocket of
          Just proxySocket -> VDPSConnected proxySocket
          Nothing -> VDPSClosed
      }

  resetVisitorSessionExpiry visitorSessionDataTVar visitorSessionId visitorDataTVar visitorId visitorMapTVar

  return (visitorSessionId, visitorSessionDataTVar)

resetVisitorSessionExpiry :: VisitorSessionDataTVar -> Integer -> VisitorDataTVar -> Text -> VisitorMapTVar -> IO ()
resetVisitorSessionExpiry visitorSessionDataTVar visitorSessionId visitorDataTVar visitorId visitorMapTVar = do
  (oldVisitorSessionExpiryAbortTVar, newVisitorSessionExpiryAbortTVar) <- atomically $ do
    visitorSessionData <- readTVar visitorSessionDataTVar
    let oldVisitorSessionExpiryAbortTVar' = vsdVisitorSessionExpiryAbortTVar visitorSessionData
    newVisitorSessionExpiryAbortTVar' <- newTVar False
    writeTVar visitorSessionDataTVar $ visitorSessionData { vsdVisitorSessionExpiryAbortTVar = newVisitorSessionExpiryAbortTVar' }
    return (oldVisitorSessionExpiryAbortTVar', newVisitorSessionExpiryAbortTVar')

  -- abort the old expiry timeout
  void $ abortTimeout oldVisitorSessionExpiryAbortTVar

  -- create the new timeout
  setTimeout 20 newVisitorSessionExpiryAbortTVar $ deleteVisitorSession visitorSessionDataTVar visitorSessionId visitorDataTVar

  -- also reset the visitor expiry
  resetVisitorExpiry visitorDataTVar visitorId visitorMapTVar

deleteVisitorSession :: VisitorSessionDataTVar -> Integer -> VisitorDataTVar -> IO ()
deleteVisitorSession visitorSessionDataTVar visitorSessionId visitorDataTVar = do
  putStrLn "Visitor session cleanup triggered"
  xxx <- atomically $ do
    visitorData <- readTVar visitorDataTVar
    visitorSessionData <- readTVar visitorSessionDataTVar
    return (visitorData, visitorSessionData)
  print xxx
  -- TODO remove

  maybeProxySocketToClose <- atomically $ do
    -- remove the visitor session from the visitor sessions map
    visitorData <- readTVar visitorDataTVar
    let newVisitorSessionsMap = Map.delete visitorSessionId (vdSessions visitorData)
    writeTVar visitorDataTVar $ visitorData {
      vdSessions = newVisitorSessionsMap
    }
    
    return $ case (Map.null newVisitorSessionsMap, vdProxyStatus visitorData) of
      (True, VDPSConnected proxySocket) -> Just proxySocket
      _ -> Nothing

  -- close the proxy socket, if any
  case maybeProxySocketToClose of
    Just proxySocket -> sClose proxySocket
    Nothing -> return ()

handleMessageFromProxy :: ChatServerMessageType -> ByteString -> ProxySendChan -> VisitorDataTVar -> IO ()
handleMessageFromProxy messageType encodedParams proxySendChan visitorDataTVar = do
  case messageType of
    --LocateSiteMessage -> unpackAndHandle $ \(siteId) -> handleLocateSiteMessage siteId proxySendChan siteMapTVar
    _ -> do
      putStrLn "Proxy sent an unknown command"
      atomically $ closeProxySocket proxySendChan
  where
    unpackAndHandle handleFunction =
      case unpackMessage encodedParams of
        Just params -> handleFunction params
        Nothing -> do
          putStrLn "Proxy connection dropped due to message unpack failure."
          atomically $ closeProxySocket proxySendChan
