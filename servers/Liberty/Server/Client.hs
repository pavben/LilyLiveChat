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

data ClientGuestData' = ClientGuestData' {
  cgdSiteId :: SiteId,
  cgdName :: Text,
  cgdColor :: Text,
  cgdIconUrl :: Text
} deriving (Show)

data OtherClientData = ClientUnregistered | ClientGuestData ClientGuestData'
  deriving (Show)

data ClientData = ClientData {
  cdSocket :: Socket,
  cdOtherData :: OtherClientData
} deriving (Show)

type MaybeClientData = Maybe ClientData

initializeClient :: Socket -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
initializeClient clientSocket databaseHandleTVar siteMapTVar =
  finally
    (clientSocketReadLoop (ClientData clientSocket ClientUnregistered) LBS.empty databaseHandleTVar siteMapTVar)
    (sClose clientSocket)

-- TODO: DoS vulnerability: Filling the buffer until out of memory
clientSocketReadLoop :: ClientData -> ByteString -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
clientSocketReadLoop clientData buffer databaseHandleTVar siteMapTVar = do
  case parseMessage buffer of
    Just (maybeMessage, newBuffer) ->
      case maybeMessage of
        Just message -> do
          putStrLn $ "Msg: " ++ show message
          handleMessage message clientData databaseHandleTVar siteMapTVar
          clientSocketReadLoop clientData newBuffer databaseHandleTVar siteMapTVar
        Nothing -> do
          putStrLn "No valid message in current buffer yet"
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
              clientSocketReadLoop clientData (LBS.append newBuffer receivedData) databaseHandleTVar siteMapTVar
            Nothing ->
              putStrLn $ "Client disconnecting -- recv returned nothing"
    Nothing ->
      putStrLn "Client disconnecting due to a protocol violation"

handleMessage :: Message -> ClientData -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleMessage (messageType, params) clientData databaseHandleTVar siteMapTVar =
  case (messageType,params) of
    (GuestJoinMessage,[siteIdT,name,color,icon]) -> do
      case parseIntegral siteIdT of
        Just siteId -> handleGuestJoin siteId name color icon clientData databaseHandleTVar siteMapTVar
        Nothing -> putStrLn "Numeric conversion failed!"
    _ -> putStrLn "No match"

handleGuestJoin :: SiteId -> Text -> Text -> Text -> ClientData -> DatabaseHandleTVar -> SiteMapTVar -> IO ()
handleGuestJoin siteId name color icon clientData databaseHandleTVar siteMapTVar = do
  lookupResult <- lookupSite databaseHandleTVar siteMapTVar siteId
  case lookupResult of
    Right siteData -> do
      putStrLn $ "got site data: " ++ show siteData
      case createMessage (NowTalkingToMessage, [LT.pack "Joe"]) of
        Just encodedMessage -> do
          sendAll (cdSocket clientData) $ encodedMessage
          sendAll (cdSocket clientData) $ encodedMessage
          sendAll (cdSocket clientData) $ encodedMessage
          return ()
        Nothing -> return ()
    Left _ -> putStrLn "Error in lookup"
  return ()

-- TODO: Make this return an error on exception?
sendMessage :: ClientData -> ByteString -> IO ()
sendMessage clientData byteString =
  catch
    (sendAll (cdSocket clientData) byteString)
    (\(SomeException e) -> do
      putStrLn "Exception on sendMessage. Closing socket."
      sClose (cdSocket clientData)
    )

