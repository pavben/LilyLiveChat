{-# LANGUAGE ScopedTypeVariables #-}

module Liberty.SiteLocatorService.ServiceHandlers (
  initializeClient
) where
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List
import qualified Data.MessagePack as MP
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)
import Liberty.Common.Messages
import Liberty.Common.Messages.SiteLocatorService
import Liberty.SiteLocatorService.SiteMap
import Liberty.SiteLocatorService.Types

initializeClient :: Socket -> SiteMapTVar -> IO ()
initializeClient clientSocket siteMapTVar = do
  clientSendChan <- atomically $ newTChan
  finally
    (do
      _ <- forkIO $ clientSocketSendLoop clientSocket clientSendChan
      clientSocketReadLoop clientSocket LBS.empty clientSendChan siteMapTVar
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

clientSocketReadLoop :: Socket -> ByteString -> ClientSendChan -> SiteMapTVar -> IO ()
clientSocketReadLoop clientSocket buffer clientSendChan siteMapTVar =
  case parseMessage buffer of
    Just (maybeMessage, newBuffer) ->
      case maybeMessage of
        Just (messageType, encodedParams) -> do
          putStrLn $ "Msg: " ++ show messageType ++ ", Encoded Params: " ++ show encodedParams
          handleMessage messageType encodedParams clientSendChan siteMapTVar
          clientSocketReadLoop clientSocket newBuffer clientSendChan siteMapTVar
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
              clientSocketReadLoop clientSocket (LBS.append newBuffer receivedData) clientSendChan siteMapTVar
            Nothing ->
              putStrLn $ "Client disconnecting -- recv returned nothing"
    Nothing ->
      putStrLn "Client disconnecting due to a protocol violation"

handleMessage :: SiteLocatorServiceMessageType -> ByteString -> ClientSendChan -> SiteMapTVar -> IO ()
handleMessage messageType encodedParams clientSendChan siteMapTVar = do
  case messageType of
    LocateSiteMessage -> unpackAndHandle $ \(siteId) -> handleLocateSiteMessage siteId clientSendChan siteMapTVar
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

handleLocateSiteMessage :: SiteId -> ClientSendChan -> SiteMapTVar -> IO ()
handleLocateSiteMessage siteId clientSendChan siteMapTVar = do
  maybeServerId <- lookupServerForSite siteId siteMapTVar
  case maybeServerId of
    Just serverId -> atomically $ createAndSendMessage SiteLocatedMessage (serverId) clientSendChan
    Nothing -> atomically $ createAndSendMessage SiteLocateFailedMessage () clientSendChan -- no servers available

createAndSendMessage :: (MessageType a, MP.Packable b) => a -> b -> ClientSendChan -> STM ()
createAndSendMessage messageType params clientSendChan =
  case createMessage messageType params of
    Just encodedMessage -> do
      writeTChan clientSendChan $ SendMessage encodedMessage
    Nothing -> return ()

closeClientSocket :: ClientSendChan -> STM ()
closeClientSocket clientSendChan =
  writeTChan clientSendChan CloseSocket

