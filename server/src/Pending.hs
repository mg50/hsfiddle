module Pending where
import Control.Monad
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.IORef
import System.Timeout
import Types

newPending :: (Ord a) => IO (Pending a b)
newPending = newIORef M.empty

deliverPending :: (Ord a) => Pending a b -> a -> b -> IO ()
deliverPending pending id val = do
  pmap <- readIORef pending
  case M.lookup id pmap of
    Just mv -> tryPutMVar mv val >> return ()
    Nothing -> return ()

awaitPending :: (Ord a) => Pending a b -> a -> Maybe Int -> IO (Maybe b)
awaitPending pending id timer = do
  mvar <- newEmptyMVar
  atomicModifyIORef pending $ \pmap -> (M.insert id mvar pmap, ())
  result <- maybeTimeout timer (takeMVar mvar)
  atomicModifyIORef pending $ \pmap -> (M.delete id pmap, ())
  return result

maybeTimeout Nothing action  = liftM Just action
maybeTimeout (Just t) action = timeout t action
