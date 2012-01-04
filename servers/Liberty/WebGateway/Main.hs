module Liberty.WebGateway.Main (
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.WebGateway.ConnectionDispatcher

main :: IO ()
main = do
  putStrLn "Web Gateway starting..."
  _ <- forkIO runConnectionDispatcher
  -- wait for a kill signal
  forever $ threadDelay (10000 * 1000)

