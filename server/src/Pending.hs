module Pending where
import Control.Monad
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.IORef

type Pending a b = IORef (M.Map a (MVar b))

newPending :: (Ord a) => IO (Pending a b)
newPending = newIORef M.empty

deliverPending :: (Ord a) => Pending a b -> a -> b -> IO ()
deliverPending pending id val = do
  pmap <- readIORef pending
  case M.lookup id pmap of
    Just mv -> putMVar mv val
    Nothing -> return ()

awaitPending pending id = do
  mvar <- newEmptyMVar
  atomicModifyIORef pending $ \pmap -> (M.insert id mvar pmap, ())
  result <- takeMVar mvar
  atomicModifyIORef pending $ \pmap -> (M.delete id pmap, ())
  return result
