module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import LibertyServ.ClientDispatcher

main :: IO ()
main = do
  putStrLn "LibertyServ starting..."
  _ <- forkIO runClientDispatcher
  -- go into a permanent loop until an exception occurs due to CTRL+C -- this is ugly, but haven't found a better way yet
  forever $ threadDelay (10000 * 1000)

