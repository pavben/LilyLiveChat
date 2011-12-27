module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import LibertyServ.ClientDispatcher
import LibertyServ.DatabaseManager

main :: IO ()
main = do
  putStrLn "LibertyServ starting..."
  -- database
  databaseHandleTVar <- initializeDatabaseManager
  _ <- forkIO $ runDatabaseManager databaseHandleTVar
  -- client dispatcher
  _ <- forkIO runClientDispatcher
  -- go into a permanent loop until an exception occurs due to CTRL+C -- this is ugly, but haven't found a better way yet
  threadDelay $ 5000 * 1000
  notifyDatabaseFailure databaseHandleTVar
  forever $ threadDelay (10000 * 1000)

