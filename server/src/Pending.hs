{-# LANGUAGE OverloadedStrings #-}

module Pending where
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.SafeMVar
import qualified Data.Map as M
import Data.Text.Lazy as T

type Pending a b = M.Map a (MVar b)

awaitPending :: (Ord a) => SafeMVar (Pending a b) -> a -> IO b
awaitPending pending id = do
  mvar <- newEmptyMVar
  modifySafeMVar pending $ return . M.insert id mvar
  result <- takeMVar mvar
  modifySafeMVar pending $ return . M.delete id
  return result
