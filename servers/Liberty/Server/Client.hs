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

-- local data
data ClientRef = ClientRef {
  crClientChan :: ClientChan,
  crClientSocket :: Socket
}

initializeClient :: Socket -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
initializeClient clientSocket databaseHandleTVar siteMapTVar = do
  clientChan <- atomically $ newTChan
  let clientRef = ClientRef clientChan clientSocket
  _ <- forkIO $ clientChanLoop clientRef
  clientSocketReadLoop clientRef LBS.empty databaseHandleTVar siteMapTVar

-- TODO: DoS vulnerability: Filling the buffer until out of memory
clientSocketReadLoop :: ClientRef -> ByteString -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
clientSocketReadLoop clientRef buffer databaseHandleTVar siteMapTVar =
  finally
    (do
      case parseMessage buffer of
        Just (maybeMessage, newBuffer) ->
          case maybeMessage of
            Just message -> do
              putStrLn $ "Msg: " ++ show message
              handleMessage message clientRef databaseHandleTVar siteMapTVar
              clientSocketReadLoop clientRef newBuffer databaseHandleTVar siteMapTVar
            Nothing -> do
              putStrLn "No valid message in current buffer yet"
              maybeReceivedData <- catch (do
                recvResult <- recv (crClientSocket clientRef) 2048
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
                  clientSocketReadLoop clientRef (LBS.append newBuffer receivedData) databaseHandleTVar siteMapTVar
                Nothing ->
                  putStrLn $ "Client disconnecting -- recv returned nothing"
    )
    (atomically $ writeTChan (crClientChan clientRef) $ Disconnect)

clientChanLoop :: ClientRef -> IO ()
clientChanLoop clientRef = do
  msg <- atomically $ readTChan (crClientChan clientRef)
  shouldLoop <- case msg of
    SendMessageToClient encodedMessage -> do
      catch
        (sendAll (crClientSocket clientRef) encodedMessage)
        (\(SomeException ex) -> do
          -- disconnect on exception
          putStrLn "Error on send"
          atomically $ writeTChan (crClientChan clientRef) $ Disconnect
        )
      return True
    Disconnect -> do
      sClose (crClientSocket clientRef)
      putStrLn "Client socket closed"
      return False

  case shouldLoop of
    True -> clientChanLoop clientRef
    False -> return ()

handleMessage :: Message -> ClientRef -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleMessage (messageType, params) clientRef databaseHandleTVar siteMapTVar =
  case (messageType,params) of
    (GuestJoinMessage,[siteIdT,name,color,icon]) -> do
      case parseIntegralCheckBounds siteIdT of
        Just siteId -> handleGuestJoin siteId name color icon clientRef databaseHandleTVar siteMapTVar
        Nothing -> putStrLn "Numeric conversion failed!"
    _ -> putStrLn "No match"

handleGuestJoin :: SiteId -> Text -> Text -> Text -> ClientRef -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleGuestJoin siteId name color icon (ClientRef clientChan clientSocket) databaseHandleTVar siteMapTVar = do
  lookupResult <- lookupSite databaseHandleTVar siteMapTVar siteId
  case lookupResult of
    Right siteData -> do
      putStrLn $ "got site data: " ++ show siteData
      case createMessage (NowTalkingToMessage, [LT.pack "Joe"]) of
        Just encodedMessage -> do
          sendAll clientSocket $ encodedMessage
          sendAll clientSocket $ encodedMessage
          sendAll clientSocket $ encodedMessage
          return ()
        Nothing -> return ()
    Left _ -> putStrLn "Error in lookup"
  return ()

