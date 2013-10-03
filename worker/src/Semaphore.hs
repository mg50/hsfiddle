module Semaphore where
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TBChan

data Semaphore = Semaphore (TBChan ()) (TVar Int) (TMVar ())

newSemaphore :: Int -> IO Semaphore
newSemaphore n = do ch <- newTBChanIO n
                    count <- newTVarIO 0
                    empty <- newTMVarIO ()
                    return $ Semaphore ch count empty

acquire :: Semaphore -> IO ()
acquire (Semaphore ch ct em) = atomically $ do
  writeTBChan ch ()
  n <- readTVar ct
  writeTVar ct (n + 1)
  tryTakeTMVar em
  return ()

release :: Semaphore -> IO ()
release (Semaphore ch ct em) = atomically $ do
  readTBChan ch
  n <- readTVar ct
  writeTVar ct (n - 1)
  when (n - 1 == 0) $ void $ tryPutTMVar em ()
  return ()

awaitDrain :: Semaphore -> IO ()
awaitDrain sem@(Semaphore _ _ em) = atomically $ takeTMVar em
