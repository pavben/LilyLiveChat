module LibertyServ.Client (
  initializeClient
) where
import Network.Socket

initializeClient :: Socket -> IO ()
initializeClient clientSocket = do
  putStrLn "client init"

