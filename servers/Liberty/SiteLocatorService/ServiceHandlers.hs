{-# LANGUAGE ScopedTypeVariables #-}

module Liberty.SiteLocatorService.ServiceHandlers (
  handleMessage
) where
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Data.ByteString.Lazy (ByteString)
import qualified Data.MessagePack as MP
import Prelude hiding (catch)
import Liberty.Common.Messages
import Liberty.Common.Messages.SiteLocatorService
import Liberty.Common.ServiceServer
import Liberty.SiteLocatorService.SiteMap
import Liberty.SiteLocatorService.Types

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

