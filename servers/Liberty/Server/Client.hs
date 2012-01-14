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
      clientDataTVar <- atomically $ newTVar (ClientData clientSocket clientSendChan ClientUnregistered)
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
        (\(SomeException e) -> do
          putStrLn "Exception on sendAll. Closing socket."
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
handleMessage (messageType, params) clientDataTVar databaseHandleTVar siteMapTVar =
  case (messageType,params) of
    (GuestJoinMessage,[siteIdT,name,color,icon]) -> do
      case parseIntegral siteIdT of
        Just siteId -> handleGuestJoin siteId name color icon clientDataTVar databaseHandleTVar siteMapTVar
        Nothing -> do
          putStrLn "Numeric conversion failed!"
          closeClientSocket clientDataTVar
    _ -> do
      putStrLn "No match"
      closeClientSocket clientDataTVar

handleGuestJoin :: SiteId -> Text -> Text -> Text -> ClientDataTVar -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleGuestJoin siteId name color icon clientDataTVar databaseHandleTVar siteMapTVar = do
  lookupResult <- lookupSite databaseHandleTVar siteMapTVar siteId
  case lookupResult of
    Right siteDataTVar -> do
      siteData <- atomically $ readTVar siteDataTVar
      putStrLn $ "Obtained site data from db: " ++ show siteData
      -- start debug
      createAndSendMessage (NowTalkingToMessage, [LT.pack "Joe"]) clientDataTVar
      createAndSendMessage (NowTalkingToMessage, [LT.pack "Joe"]) clientDataTVar
      createAndSendMessage (NowTalkingToMessage, [LT.pack "Joe"]) clientDataTVar
      -- end debug
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

