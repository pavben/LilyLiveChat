module LibertyServ.Client (
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
import LibertyServ.DatabaseManager
import LibertyServ.Messages.ClientChan
import LibertyServ.NetworkMessage
import LibertyServ.Site
import LibertyServ.SiteMap
import LibertyServ.Utils

initializeClient :: Socket -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
initializeClient clientSocket databaseHandleTVar siteMapTVar = do
  clientChan <- atomically $ newTChan
  _ <- forkIO $ clientChanLoop clientChan clientSocket
  clientSocketReadLoop clientSocket LBS.empty clientChan databaseHandleTVar siteMapTVar

-- TODO: DoS vulnerability: Filling the buffer until out of memory
clientSocketReadLoop :: Socket -> ByteString -> ClientChan -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
clientSocketReadLoop clientSocket buffer clientChan databaseHandleTVar siteMapTVar =
  finally
    (catch
      (do
        recvResult <- recv clientSocket 2048
        if not $ LBS.null recvResult then do
          putStrLn $ "Len: " ++ show (LBS.length recvResult)
          let parseResult = parseMessage $ LBS.append buffer recvResult
          case parseResult of
            Just (maybeMessage, newBuffer) -> do
              case maybeMessage of
                Just message -> do
                  putStrLn $ "Msg: " ++ show message
                  handleMessage message databaseHandleTVar siteMapTVar
                Nothing -> putStrLn $ "No valid message in current buffer yet"
              clientSocketReadLoop clientSocket newBuffer clientChan databaseHandleTVar siteMapTVar
            Nothing -> do
              putStrLn $ "Message parse failed due to protocol error"
        else do
          putStrLn $ "Client disconnecting -- recv returned nothing"
      )
      (\(SomeException ex) -> do
        putStrLn $ "Client disconnecting due to exception: " ++ show ex
      )
    )
    (atomically $ writeTChan clientChan $ Disconnect)

clientChanLoop :: ClientChan -> Socket -> IO ()
clientChanLoop clientChan clientSocket = do
  msg <- atomically $ readTChan clientChan
  shouldLoop <- case msg of
    SendMessageToClient encodedMessage -> do
      catch
        (sendAll clientSocket encodedMessage)
        (\(SomeException ex) -> do
          -- disconnect on exception
          putStrLn "Error on send"
          atomically $ writeTChan clientChan $ Disconnect
        )
      return True
    Disconnect -> do
      sClose clientSocket
      putStrLn "Client socket closed"
      return False

  case shouldLoop of
    True -> clientChanLoop clientChan clientSocket
    False -> return ()

handleMessage :: Message -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleMessage (messageType, params) databaseHandleTVar siteMapTVar =
  case (messageType,params) of
    (GuestJoinMessage,[siteIdT,name,color]) -> do
      case parseIntegralCheckBounds siteIdT of
        Just siteId -> handleGuestJoin siteId name color databaseHandleTVar siteMapTVar
        Nothing -> putStrLn "Numeric conversion failed!"
    _ -> putStrLn "No match"

handleGuestJoin :: SiteId -> Text -> Text -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleGuestJoin siteId name color databaseHandleTVar siteMapTVar = do
  lookupResult <- lookupSite databaseHandleTVar siteMapTVar siteId
  case lookupResult of
    Right siteData -> do
      putStrLn $ "got site data: " ++ show siteData
    Left _ -> putStrLn "Error in lookup"
  return ()

