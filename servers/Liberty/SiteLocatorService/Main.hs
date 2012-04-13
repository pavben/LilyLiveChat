module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.SiteLocatorService.WebDispatcher

main :: IO ()
main = do
  putStrLn "SiteLocatorService starting..."

  -- run the web dispatcher
  forkIO $ runWebDispatcher

  -- TODO: go into a permanent loop until an exception occurs due to CTRL+C -- this is ugly, but haven't found a better way yet
  forever $ threadDelay (10000 * 1000)

