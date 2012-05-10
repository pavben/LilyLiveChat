module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.MainWebsite.WebDispatcher

main :: IO ()
main = do
  putStrLn "MainWebsite starting..."

  -- run the web dispatcher
  _ <- forkIO $ runWebDispatcher

  -- go into a permanent loop until an exception occurs due to CTRL+C -- this is ugly, but haven't found a better way yet
  forever $ threadDelay (10000 * 1000)

