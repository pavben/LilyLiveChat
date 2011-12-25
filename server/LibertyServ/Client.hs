module LibertyServ.Client (
  initializeClient
) where
import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Prelude hiding (catch)

import LibertyServ.NetworkMessage

initializeClient :: Socket -> IO ()
initializeClient clientSocket = do
  putStrLn "client init"
  clientSocketLoop clientSocket

clientSocketLoop :: Socket -> IO ()
clientSocketLoop clientSocket = catch
  (do
    recvResult <- recv clientSocket 2048
    if not $ LBS.null recvResult then do
      putStrLn $ "Len: " ++ show (LBS.length recvResult)
      let maybeMessage = parseMessage recvResult
      case maybeMessage of
        Just (messageType, texts) -> putStrLn $ "MsgType: " ++ show messageType ++ " - Texts: " ++ show texts
        Nothing -> do
          putStrLn $ "Client disconnecting -- sent an invalid message"
          sClose clientSocket
      clientSocketLoop clientSocket
    else do
      putStrLn $ "Client disconnecting -- recv returned nothing"
      sClose clientSocket
  )
  (\ex -> do
    let _ = ex :: IOException
    putStrLn $ "Client disconnecting due to IO exception: " ++ show ex
    sClose clientSocket
  )

