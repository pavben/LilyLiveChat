module Liberty.Server.Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.Server.ClientDispatcher
import Liberty.Server.DatabaseManager
import Liberty.Server.SiteMap

main :: IO ()
main = do
  putStrLn "Server starting..."
  -- database
  databaseHandleTVar <- initializeDatabaseManager
  _ <- forkIO $ runDatabaseManager databaseHandleTVar
  -- site map
  siteMap <- initializeSiteMap
  -- client dispatcher
  _ <- forkIO $ runClientDispatcher databaseHandleTVar siteMap
  -- go into a permanent loop until an exception occurs due to CTRL+C -- this is ugly, but haven't found a better way yet
  threadDelay $ 5000 * 1000
  -- TODO
  forever $ threadDelay (10000 * 1000)

