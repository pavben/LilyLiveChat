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

initializeClient :: Socket -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
initializeClient clientSocket databaseHandleTVar siteMapTVar =
  finally
    (clientSocketReadLoop clientSocket LBS.empty databaseHandleTVar siteMapTVar)
    (sClose clientSocket)

-- TODO: DoS vulnerability: Filling the buffer until out of memory
clientSocketReadLoop :: Socket -> ByteString -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
clientSocketReadLoop clientSocket buffer databaseHandleTVar siteMapTVar = do
  case parseMessage buffer of
    Just (maybeMessage, newBuffer) ->
      case maybeMessage of
        Just message -> do
          putStrLn $ "Msg: " ++ show message
          handleMessage message clientSocket databaseHandleTVar siteMapTVar
          clientSocketReadLoop clientSocket newBuffer databaseHandleTVar siteMapTVar
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
              clientSocketReadLoop clientSocket (LBS.append newBuffer receivedData) databaseHandleTVar siteMapTVar
            Nothing ->
              putStrLn $ "Client disconnecting -- recv returned nothing"
    Nothing ->
      putStrLn "Client disconnecting due to a protocol violation"

handleMessage :: Message -> Socket -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleMessage (messageType, params) clientSocket databaseHandleTVar siteMapTVar =
  case (messageType,params) of
    (GuestJoinMessage,[siteIdT,name,color,icon]) -> do
      case parseIntegral siteIdT of
        Just siteId -> handleGuestJoin siteId name color icon clientSocket databaseHandleTVar siteMapTVar
        Nothing -> putStrLn "Numeric conversion failed!"
    _ -> putStrLn "No match"

handleGuestJoin :: SiteId -> Text -> Text -> Text -> Socket -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleGuestJoin siteId name color icon clientSocket databaseHandleTVar siteMapTVar = do
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

