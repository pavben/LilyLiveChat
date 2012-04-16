module Main (
  main
) where
import Control.Concurrent
import Control.Monad (forever)
import Liberty.WebChatInterface.Sessions
import Liberty.WebChatInterface.WebDispatcher

main :: IO ()
main = do
  putStrLn "WebChatInterface starting..."
  sessionMapTVar <- createSessionMapTVar
  _ <- forkIO $ runWebDispatcher sessionMapTVar
  -- wait for a kill signal
  forever $ threadDelay (10000 * 1000)

