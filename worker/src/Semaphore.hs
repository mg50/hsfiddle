module Semaphore where
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TBChan

data Semaphore = Semaphore (TBChan ()) Int

newSemaphore :: Int -> IO Semaphore
newSemaphore n = do ch <- newTBChanIO n
                    return $ Semaphore ch n

acquire :: Semaphore -> IO ()
acquire (Semaphore ch _) = atomically $ writeTBChan ch ()

release :: Semaphore -> IO ()
release (Semaphore ch _) = atomically $ readTBChan ch

awaitDrain :: Semaphore -> IO ()
awaitDrain sem@(Semaphore ch n) = atomically $ do
  free <- freeSlotsTBChan ch
  when (free /= n) retry
