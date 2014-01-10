module Database.Redis.Mock where

import Data.Maybe
import Data.Text
import qualified Data.Map as M
import Data.IORef

type MockRedis = IORef (M.Map Text Text)

newMockRedis = newIORef M.empty
clearMockRedis r = atomicModifyIORef' r $ \_ -> (M.empty, ())
cache r k v = atomicModifyIORef' r $ \m -> (M.insert k v m, ())

retrieve :: (Ord k) => IORef (M.Map k v) -> k -> IO (Maybe v)
retrieve r k = atomicModifyIORef' r $ \m -> (m, M.lookup k m)
contains r k = fmap (isJust . M.lookup k) (readIORef r)
