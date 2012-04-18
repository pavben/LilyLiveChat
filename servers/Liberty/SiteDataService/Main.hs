module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.SiteDataService.ClientDispatcher
import Liberty.SiteDataService.DatabaseManager

main :: IO ()
main = do
  putStrLn "SiteDataService starting..."

  -- database
  databaseHandleTVar <- initializeDatabaseManager
  _ <- forkIO $ runDatabaseManager databaseHandleTVar

  -- run the client dispatcher
  _ <- forkIO $ runClientDispatcher databaseHandleTVar

  -- go into a permanent loop until an exception occurs due to CTRL+C -- this is ugly, but haven't found a better way yet
  forever $ threadDelay (10000 * 1000)

