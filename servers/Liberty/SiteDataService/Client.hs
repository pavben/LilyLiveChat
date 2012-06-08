{-# LANGUAGE ScopedTypeVariables #-}

module Liberty.SiteDataService.Client (
  initializeClient
) where
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import Data.List
import qualified Data.MessagePack as MP
import Data.Text.Lazy (Text)
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import Liberty.Common.Messages
import Liberty.Common.Messages.SiteDataService
import Liberty.Common.Messages.SiteLocatorService
import Liberty.SiteDataService.DatabaseManager
import Liberty.SiteDataService.Types

-- TODO: Move SDS to ServiceServer

initializeClient :: Socket -> DatabaseHandleTVar -> IO ()
initializeClient clientSocket databaseHandleTVar = do
  clientSendChan <- atomically $ newTChan
  finally
    (do
      _ <- forkIO $ clientSocketSendLoop clientSocket clientSendChan
      clientSocketReadLoop clientSocket LBS.empty clientSendChan databaseHandleTVar
    )
    (do
      atomically $ writeTChan clientSendChan $ CloseSocket
      sClose clientSocket
    )

clientSocketSendLoop :: Socket -> ClientSendChan -> IO ()
clientSocketSendLoop clientSocket clientSendChan = do
  clientSendChanMessage <- atomically $ readTChan clientSendChan
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
        clientSocketSendLoop clientSocket clientSendChan
      else
        sClose clientSocket
    CloseSocket -> do
      putStrLn "Got CloseSocket message. Closing client socket."
      sClose clientSocket

clientSocketReadLoop :: Socket -> ByteString -> ClientSendChan -> DatabaseHandleTVar -> IO ()
clientSocketReadLoop clientSocket buffer clientSendChan databaseHandleTVar =
  case parseMessage buffer of
    Just (maybeMessage, newBuffer) ->
      case maybeMessage of
        Just (messageType, encodedParams) -> do
          putStrLn $ "Msg: " ++ show messageType ++ ", Encoded Params: " ++ show encodedParams
          handleMessage messageType encodedParams clientSendChan databaseHandleTVar
          clientSocketReadLoop clientSocket newBuffer clientSendChan databaseHandleTVar
        Nothing -> do
          putStrLn "No valid message in current buffer yet"
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
              clientSocketReadLoop clientSocket (LBS.append newBuffer receivedData) clientSendChan databaseHandleTVar
            Nothing ->
              putStrLn $ "Client disconnecting -- recv returned nothing"
    Nothing ->
      putStrLn "Client disconnecting due to a protocol violation"

handleMessage :: SiteDataServiceMessageType -> ByteString -> ClientSendChan -> DatabaseHandleTVar -> IO ()
handleMessage messageType encodedParams clientSendChan databaseHandleTVar = do
  case messageType of
    GetSiteDataMessage -> unpackAndHandle $ \(siteId, requesterServerId) -> handleGetSiteDataMessage siteId requesterServerId clientSendChan databaseHandleTVar
    SaveSiteDataMessage -> unpackAndHandle $ \(currentSiteId, siteDataParam, requesterServerId) -> handleSaveSiteDataMessage currentSiteId siteDataParam requesterServerId clientSendChan databaseHandleTVar
    _ -> do
      putStrLn "Client sent an unknown command"
      atomically $ closeClientSocket clientSendChan
  where
    unpackAndHandle handleFunction =
      case unpackMessage encodedParams of
        Just params -> handleFunction params
        Nothing -> do
          putStrLn "Client dropped due to message unpack failure."
          atomically $ closeClientSocket clientSendChan

handleGetSiteDataMessage :: SiteId -> Text -> ClientSendChan -> DatabaseHandleTVar -> IO ()
handleGetSiteDataMessage siteId requesterServerId clientSendChan databaseHandleTVar = do
  -- Make sure requesterServerId is authoritative for siteId
  siteLocateResult <- locateSite siteId
  case siteLocateResult of
    SLSuccess authoritativeServerId | requesterServerId == authoritativeServerId -> do
      getResult <- getSiteDataFromDb siteId databaseHandleTVar
      case getResult of
        GSDRSuccess siteData ->
          atomically $ createAndSendMessage SiteDataFoundMessage (siteDataToMessage siteData) clientSendChan
        GSDRNotFound ->
          atomically $ createAndSendMessage SiteNotFoundMessage () clientSendChan
        GSDRNotAvailable ->
          atomically $ createAndSendMessage DataNotAvailableMessage () clientSendChan
      where
        siteDataToMessage siteData = (
          sdSiteId siteData,
          sdPlanId siteData,
          sdName siteData,
          (fromInteger $ sdNextOperatorId siteData :: Int),
          map operatorToMessage (sdOperators siteData),
          sdAdminUserIds siteData)
        operatorToMessage siteOperatorData = (
          (fromInteger $ sodOperatorId siteOperatorData :: Int),
          sodName siteOperatorData,
          sodColor siteOperatorData,
          sodTitle siteOperatorData,
          sodIconUrl siteOperatorData,
          sodUserId siteOperatorData,
          sodActivationToken siteOperatorData)
    SLSuccess _ -> atomically $ createAndSendMessage NonAuthoritativeServerMessage () clientSendChan
    SLNotAvailable -> atomically $ createAndSendMessage DataNotAvailableMessage () clientSendChan

handleSaveSiteDataMessage :: Text -> SiteDataForMessage -> Text -> ClientSendChan -> DatabaseHandleTVar -> IO ()
handleSaveSiteDataMessage currentSiteId (siteId, planId, name, nextOperatorId, operators, adminUserIds) requesterServerId clientSendChan databaseHandleTVar = do
  -- Make sure requesterServerId is authoritative for siteId
  siteLocateResult <- locateSite siteId
  case siteLocateResult of
    SLSuccess authoritativeServerId | requesterServerId == authoritativeServerId -> do
      saveResult <- saveSiteDataToDb currentSiteId siteData databaseHandleTVar
      case saveResult of
        True -> atomically $ createAndSendMessage SiteDataSavedMessage () clientSendChan
        False -> atomically $ createAndSendMessage SiteDataSaveFailedMessage () clientSendChan

      where
        siteData = SiteData siteId planId name (toInteger nextOperatorId) operatorsToSiteData adminUserIds
        operatorsToSiteData = flip map operators $ \(
          operatorId,
          operatorName,
          operatorColor,
          operatorTitle,
          operatorIconUrl,
          operatorUserId,
          operatorActivationToken
          ) ->
          SiteOperatorData (toInteger operatorId) operatorName operatorColor operatorTitle operatorIconUrl operatorUserId operatorActivationToken
    SLSuccess _ -> atomically $ createAndSendMessage NonAuthoritativeServerMessage () clientSendChan
    SLNotAvailable -> atomically $ createAndSendMessage DataNotAvailableMessage () clientSendChan

createAndSendMessage :: (MessageType a, MP.Packable b) => a -> b -> ClientSendChan -> STM ()
createAndSendMessage messageType params clientSendChan =
  case createMessage messageType params of
    Just encodedMessage -> do
      writeTChan clientSendChan $ SendMessage encodedMessage
    Nothing -> return ()

closeClientSocket :: ClientSendChan -> STM ()
closeClientSocket clientSendChan =
  writeTChan clientSendChan CloseSocket

