module LibertyServ.Client (
  initializeClient
) where
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
clientSocketLoop clientSocket = do
  -- TODO: Exception
  recvResult <- recv clientSocket 2048
  putStrLn $ "Len: " ++ show (LBS.length recvResult)
  putStrLn $ show $ parseMessage recvResult
  clientSocketLoop clientSocket

