module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.ChatServer.ClientDispatcher
import Liberty.ChatServer.DatabaseManager
import Liberty.ChatServer.SiteMap

main :: IO ()
main = do
  putStrLn "Server starting..."
  -- database
  (databaseHandleTVar, databaseOperationQueueChan) <- initializeDatabaseManager
  _ <- forkIO $ runDatabaseManager databaseHandleTVar databaseOperationQueueChan
  -- site map
  siteMap <- doUntilSuccess (initializeSiteMap databaseHandleTVar) (1000 * 1000)
  putStrLn "SiteMap loaded"
  -- client dispatcher
  _ <- forkIO $ runClientDispatcher databaseOperationQueueChan siteMap
  -- go into a permanent loop until an exception occurs due to CTRL+C -- this is ugly, but haven't found a better way yet
  forever $ threadDelay (10000 * 1000)

doUntilSuccess :: IO (Maybe a) -> Int -> IO a
doUntilSuccess f interval = do
  maybeResult <- f
  case maybeResult of
    Just result -> return result
    Nothing -> threadDelay interval >> doUntilSuccess f interval

