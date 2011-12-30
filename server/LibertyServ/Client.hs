module LibertyServ.Client (
  initializeClient
) where
import Control.Exception
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
import LibertyServ.NetworkMessage
import LibertyServ.Site
import LibertyServ.SiteMap
import LibertyServ.Utils

initializeClient :: Socket -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
initializeClient clientSocket databaseHandleTVar siteMapTVar = do
  clientSocketLoop clientSocket LBS.empty databaseHandleTVar siteMapTVar

-- TODO: DoS vulnerability: Filling the buffer until out of memory
clientSocketLoop :: Socket -> ByteString -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
clientSocketLoop clientSocket buffer databaseHandleTVar siteMapTVar = catch
  (do
    recvResult <- recv clientSocket 2048
    if not $ LBS.null recvResult then do
      putStrLn $ "Len: " ++ show (LBS.length recvResult)
      let (maybeMessage, newBuffer) = parseMessage $ LBS.append buffer recvResult
      case maybeMessage of
        Just (messageTypeId, texts) -> do
          putStrLn $ "MsgType: " ++ show messageTypeId ++ " - Texts: " ++ show texts
          case messageIdToType messageTypeId of
            Just messageType -> handleMessage messageType texts databaseHandleTVar siteMapTVar
            Nothing -> putStrLn $ "Received a message with an invalid type!"
        Nothing -> putStrLn $ "No valid message in current buffer"
      clientSocketLoop clientSocket newBuffer databaseHandleTVar siteMapTVar
    else do
      putStrLn $ "Client disconnecting -- recv returned nothing"
      sClose clientSocket
  )
  (\ex -> do
    let _ = ex :: SomeException
    putStrLn $ "Client disconnecting due to exception: " ++ show ex
    sClose clientSocket
  )

handleMessage :: MessageType -> [Text] -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleMessage messageType params databaseHandleTVar siteMapTVar =
  case (messageType,params) of
    (GuestJoinMessage,[siteIdT,name,color]) -> do
      case parseIntegralCheckBounds siteIdT of
        Just siteId -> handleGuestJoin siteId name color databaseHandleTVar siteMapTVar
        Nothing -> putStrLn "Numeric conversion failed!"
    _ -> putStrLn "No match"

handleGuestJoin :: SiteId -> Text -> Text -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleGuestJoin siteId name color databaseHandleTVar siteMapTVar = do
  putStrLn $ "About to perform the lookup for site id: " ++ show siteId
  lookupResult <- lookupSite databaseHandleTVar siteMapTVar siteId
  case lookupResult of
    Right siteData -> putStrLn $ "got site data: " ++ show siteData
    Left _ -> putStrLn "Error in lookup"
  return ()

