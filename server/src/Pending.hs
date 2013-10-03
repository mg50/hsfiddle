module Pending where
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.SafeMVar
import qualified Data.Map as M
import Data.Text.Lazy as T

type Pending a b = SafeMVar (M.Map a (MVar b))

newPending :: (Ord a) => IO (Pending a b)
newPending = liftM SafeMVar $ newMVar M.empty

deliverPending :: (Ord a) => Pending a b -> a -> b -> IO ()
deliverPending pending id val = modifySafeMVar pending $ \pmap -> do
  case M.lookup id pmap of
    Just mv -> putMVar mv val
    Nothing -> return ()
  return pmap

awaitPending :: (Ord a) => Pending a b -> a -> IO b
awaitPending pending id = do
  mvar <- newEmptyMVar
  modifySafeMVar pending $ return . M.insert id mvar
  result <- takeMVar mvar
  modifySafeMVar pending $ return . M.delete id
  return result
