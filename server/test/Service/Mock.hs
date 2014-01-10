{-# LANGUAGE FlexibleInstances #-}

module Service.Mock where
import Service
import Test.Types
import qualified PG as PG
import qualified Database.Redis.Mock as MR
import Types (service, Config)

instance Service (Config TestEnv) where
  saveNewFiddle conf    = PG.saveNewFiddle (postgres' $ service conf)
  updateFiddle conf     = PG.updateFiddle (postgres' $ service conf)
  retrieveFiddle conf   = PG.retrieveFiddle (postgres' $ service conf)

  retrieveCompiled conf = MR.retrieve (mockRedis $ service conf)
  jsIsCached conf       = MR.contains (mockRedis $ service conf)

  enqueueCompilation    = enqueuer . service
