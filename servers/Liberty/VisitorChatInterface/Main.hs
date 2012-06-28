module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.VisitorChatInterface.Types
import Liberty.VisitorChatInterface.VisitorMap
import Liberty.VisitorChatInterface.WebDispatcher

main :: IO ()
main = do
  putStrLn "VisitorChatInterface starting..."

  visitorMapTVar <- createVisitorMapTVar
  _ <- forkIO $ runWebDispatcher visitorMapTVar

  -- wait for a kill signal
  forever $ threadDelay (10000 * 1000)

