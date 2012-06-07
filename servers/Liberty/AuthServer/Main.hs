module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.AuthServer.DatabaseManager
import Liberty.AuthServer.ServiceHandlers
import Liberty.AuthServer.SessionMap
import Liberty.AuthServer.WebDispatcher
import Liberty.Common.ServiceServer
import Liberty.Common.Utils

main :: IO ()
main = do
  putStrLn "AuthServer starting..."

  -- sessionMap
  sessionMapTVar <- initializeSessionMap

  -- database
  databaseHandleTVar <- initializeDatabaseManager
  _ <- forkIO $ runDatabaseManager databaseHandleTVar

  -- run the service dispatcher
  _ <- forkIO $ runServiceDispatcher (getLocalServiceHost "auth") handleMessage sessionMapTVar

  -- run the web dispatcher
  _ <- forkIO $ runWebDispatcher sessionMapTVar databaseHandleTVar

  -- go into a permanent loop until an exception occurs due to CTRL+C -- this is ugly, but haven't found a better way yet
  forever $ threadDelay (10000 * 1000)

