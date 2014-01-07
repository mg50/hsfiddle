{-# LANGUAGE FunctionalDependencies #-}

module Types where
import qualified Data.Text.Lazy as T
import qualified Data.Text as TStrict
import Data.IORef
import qualified Data.Map as M
import Control.Concurrent.MVar
import qualified Network.AMQP as AMQP
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Redis as Redis

type Pending a b = IORef (M.Map a (MVar b))

data CompileResult = CompileSuccess | CompileFailure T.Text | CompileTimeout
type PendingCompilations = Pending TStrict.Text CompileResult

data Fiddle = Fiddle { hs :: String
                     , css :: String
                     , html' :: String
                     }

data Credentials = Credentials { amqpHost  :: String
                               , amqpUser  :: String
                               , amqpPass  :: String
                               , redisHost :: String
                               , redisPass :: String
                               , pgHost    :: String
                               , pgUser    :: String
                               , pgPass    :: String
                               , pgDb      :: String
                               }

-- data Config = Config { amqpConn :: AMQP.Connection
--                      , amqpChan :: AMQP.Channel
--                      , redis :: Redis.Connection
--                      , postgres :: PG.Connection
--                      , compileTimeout :: Maybe Int
--                      , port :: Int
--                      }

data Config a = Config { service        :: a
                       , compileTimeout :: Maybe Int
                       , port           :: Int
                       }

data ProdEnv = ProdEnv { amqpConn' :: AMQP.Connection
                       , amqpChan' :: AMQP.Channel
                       , redis'    :: Redis.Connection
                       , postgres' :: PG.Connection
                       }

amqpConn = amqpConn' . service
amqpChan = amqpChan' . service
redis    = redis' . service
postgres = postgres' . service

class Connectable a b | a -> b where
  connect :: Credentials -> b -> IO a
  disconnect :: a -> IO ()

--readCredentials :: String -> Credentials

--readConfig :: AMQP -> Redis -> Postgres -> String -> Config
