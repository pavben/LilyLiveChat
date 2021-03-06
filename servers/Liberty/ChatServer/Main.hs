module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.ChatServer.ClientDispatcher
import Liberty.ChatServer.SiteDataSaver
import Liberty.ChatServer.SiteMap

main :: IO ()
main = do
  putStrLn "ChatServer starting..."
  -- site map
  siteMapTVar <- initializeSiteMap
  -- site data saver
  siteDataSaverChan <- initializeSiteDataSaver
  -- client dispatcher
  _ <- forkIO $ runClientDispatcher siteMapTVar siteDataSaverChan
  -- go into a permanent loop until an exception occurs due to CTRL+C -- this is ugly, but haven't found a better way yet
  forever $ threadDelay (10000 * 1000)

