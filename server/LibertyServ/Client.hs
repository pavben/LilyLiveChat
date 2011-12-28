module LibertyServ.Client (
  initializeClient
) where
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTI
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)

import LibertyServ.NetworkMessage

initializeClient :: Socket -> IO ()
initializeClient clientSocket = do
  clientSocketLoop clientSocket LBS.empty

-- TODO: DoS vulnerability: Filling the buffer until out of memory
clientSocketLoop :: Socket -> ByteString -> IO ()
clientSocketLoop clientSocket buffer = catch
  (do
    recvResult <- recv clientSocket 2048
    if not $ LBS.null recvResult then do
      putStrLn $ "Len: " ++ show (LBS.length recvResult)
      let (maybeMessage, newBuffer) = parseMessage $ LBS.append buffer recvResult
      case maybeMessage of
        Just (messageTypeId, texts) -> do
          putStrLn $ "MsgType: " ++ show messageTypeId ++ " - Texts: " ++ show texts
          case messageIdToType messageTypeId of
            Just messageType -> handleMessage messageType texts
            Nothing -> putStrLn $ "Received a message with an invalid type!"
        Nothing -> putStrLn $ "No valid message in current buffer"
      clientSocketLoop clientSocket newBuffer
    else do
      putStrLn $ "Client disconnecting -- recv returned nothing"
      sClose clientSocket
  )
  (\ex -> do
    let _ = ex :: SomeException
    putStrLn $ "Client disconnecting due to exception: " ++ show ex
    sClose clientSocket
  )

handleMessage :: MessageType -> [Text] -> IO ()
handleMessage messageType params =
  case (messageType,params) of
    (GuestJoinMessage,[x,y]) -> LTI.putStrLn $ LT.append x y
    _ -> putStrLn "No match"

