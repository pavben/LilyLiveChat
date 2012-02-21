module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.WebGateway.ConnectionDispatcher
import Liberty.WebGateway.Sessions

main :: IO ()
main = do
  putStrLn "Web Gateway starting..."
  sessionMapTVar <- createSessionMapTVar
  _ <- forkIO $ runConnectionDispatcher sessionMapTVar
  -- wait for a kill signal
  forever $ threadDelay (10000 * 1000)

