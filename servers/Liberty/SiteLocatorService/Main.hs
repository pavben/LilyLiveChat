module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.SiteLocatorService.SiteMap
import Liberty.SiteLocatorService.ServiceHandlers
import Liberty.SiteLocatorService.WebDispatcher
import Liberty.Common.ServiceServer
import Liberty.Common.Utils

main :: IO ()
main = do
  putStrLn "SiteLocatorService starting..."

  -- initialize the SiteMap
  siteMapTVar <- initializeSiteMap

  -- run the service dispatcher
  _ <- forkIO $ runServiceDispatcher (getLocalServiceHost "sl") handleMessage siteMapTVar

  -- run the web dispatcher
  _ <- forkIO $ runWebDispatcher siteMapTVar

  -- go into a permanent loop until an exception occurs due to CTRL+C -- this is ugly, but haven't found a better way yet
  forever $ threadDelay (10000 * 1000)

