module Service where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Pending
import Types

class Service a where
  saveNewFiddle      :: a -> T.Text -> T.Text -> T.Text -> IO ()
  updateFiddle       :: a -> T.Text -> T.Text -> T.Text -> T.Text -> IO ()
  retrieveFiddle     :: a -> T.Text -> Int -> IO (Maybe Fiddle)
  retrieveCompiled   :: a -> T.Text -> IO (Maybe T.Text)
  jsIsCached         :: a -> T.Text -> IO Bool
  enqueueCompilation :: a -> T.Text -> IO T.Text

awaitCompilation config code timer pending = do
  msgId <- enqueueCompilation config code
  let timer' = fmap (\x -> (x, CompileTimeout)) timer
  awaitPending pending msgId timer'

saveFiddle :: (Service a) => a -> Maybe T.Text -> T.Text -> T.Text -> T.Text -> IO ()
saveFiddle service Nothing     = saveNewFiddle service
saveFiddle service (Just slug) = updateFiddle service slug
