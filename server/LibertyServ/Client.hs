module LibertyServ.Client (
  initializeClient
) where
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
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
    recvResult <- recv clientSocket 2
    if not $ LBS.null recvResult then do
      putStrLn $ "Len: " ++ show (LBS.length recvResult)
      let (maybeMessage, newBuffer) = parseMessage $ LBS.append buffer recvResult
      case maybeMessage of
        Just (messageType, texts) -> putStrLn $ "MsgType: " ++ show messageType ++ " - Texts: " ++ show texts
        Nothing -> putStrLn $ "No valid message in current buffer"
      clientSocketLoop clientSocket newBuffer
    else do
      putStrLn $ "Client disconnecting -- recv returned nothing"
      sClose clientSocket
  )
  (\ex -> do
    let _ = ex :: IOException
    putStrLn $ "Client disconnecting due to IO exception: " ++ show ex
    sClose clientSocket
  )

