{-# LANGUAGE FlexibleInstances #-}

module Service.HSFiddle () where

import Data.Maybe
import Data.Default

import Types
import Service
import qualified PG as PG
import qualified Redis as Redis
import qualified AMQP as AMQP

instance Service (Config ProdEnv) where
  saveNewFiddle conf    = PG.saveNewFiddle (postgres conf)
  updateFiddle conf     = PG.updateFiddle (postgres conf)
  retrieveFiddle conf   = PG.retrieveFiddle (postgres conf)

  retrieveCompiled conf = Redis.retrieveCachedJS (redis conf)
  jsIsCached conf       = Redis.md5Exists (redis conf)

  awaitCompilation conf = AMQP.awaitCompilation (amqpChan conf) (compileTimeout conf)
