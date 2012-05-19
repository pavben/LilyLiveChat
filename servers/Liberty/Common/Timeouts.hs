module Liberty.Common.Timeouts (
  setTimeout,
  abortTimeout
) where
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM

setTimeout :: Int -> TVar Bool -> IO () -> IO ()
setTimeout seconds abortTVar timeoutAction = do
  timeoutTVar <- atomically $ newTVar False
  -- timeout notification thread
  -- TODO PL: possibly add killThread to terminate the wait if abortTriggered becomes true before timeoutTriggered
  _ <- forkIO $ do
    -- wait the requested number of seconds
    threadDelay $ seconds * 1000 * 1000
    -- then set the timeout tvar to true, indicating a timeout
    atomically $ writeTVar timeoutTVar True

  -- this is the thread that waits for abort or timeout and conditionally runs timeoutAction
  _ <- forkIO $ do
    runActionFlag <- atomically $ do
      abortTriggered <- readTVar abortTVar
      timeoutTriggered <- readTVar timeoutTVar
      case (abortTriggered, timeoutTriggered) of
        -- if nothing triggered, retry
        (False, False) -> retry
        -- if abort triggered, don't run timeoutAction regardless of timeoutTriggered
        (True, _)      -> return False
        -- otherwise, timeout was triggered, so run the timeoutAction
        (_, True)      -> return True

    when runActionFlag timeoutAction
  return ()

-- ensure the abort flag is set to False and return False if the abort flag was already set
abortTimeout :: TVar Bool -> IO Bool
abortTimeout abortTVar = do
  abortedFlag <- atomically $ do
    alreadyAborted <- readTVar abortTVar
    if alreadyAborted then
      return False
    else do
      writeTVar abortTVar True
      return True

  return abortedFlag

