module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.ChatStatusService.WebDispatcher

main :: IO ()
main = do
  putStrLn "ChatStatusService starting..."

  -- run the web dispatcher
  _ <- forkIO $ runWebDispatcher

  -- go into a permanent loop until an exception occurs due to CTRL+C -- this is ugly, but haven't found a better way yet
  forever $ threadDelay (10000 * 1000)

