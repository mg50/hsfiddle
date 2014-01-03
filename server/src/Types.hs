{-# LANGUAGE FunctionalDependencies #-}

module Types where
import qualified Data.Text.Lazy as T
import qualified Data.Text as TStrict
import Data.IORef
import qualified Data.Map as M
import Control.Concurrent.MVar
import qualified Network.AMQP as AMQP
import qualified Database.PostgreSQL.Simple as PG

type Pending a b = IORef (M.Map a (MVar b))

data CompileResult = CompileSuccess | CompileFailure T.Text
type PendingCompilations = Pending TStrict.Text CompileResult

data Fiddle = Fiddle { hs :: String
                     , css :: String
                     , html' :: String
                     }

data Credentials = Credentials { amqpAddr  :: String
                               , amqpUser  :: String
                               , amqpPass  :: String
                               , redisAddr :: String
                               , redisUser :: String
                               , redisPass :: String
                               , pgHost    :: String
                               , pgPort    :: String
                               , pgUser    :: String
                               , pgPass    :: String
                               , pgDb      :: String
                               }

data Config = Config { amqpConn :: AMQP.Connection
                     , amqpChan :: AMQP.Channel
                     , redis :: String
                     , postgres :: PG.Connection
                     , compileTimeout :: Maybe Int
                     , port :: Int
                     }

class Connectable a b c | a -> b c where
  connect :: Credentials -> c -> IO (a, b)
  disconnect :: a -> IO ()
--readCredentials :: String -> Credentials

--readConfig :: AMQP -> Redis -> Postgres -> String -> Config
