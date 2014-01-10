module Test.Types where
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Redis.Mock as MR
import qualified Data.Text as T

data TestEnv = TestEnv { postgres' :: PG.Connection
                       , mockRedis :: MR.MockRedis
                       , enqueuer  :: T.Text -> IO T.Text
                       }
