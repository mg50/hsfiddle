{-# LANGUAGE MultiParamTypeClasses #-}

module Redis where
import Database.Redis hiding (connect)
import qualified Database.Redis as Redis
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Enc
import qualified Data.Text as T
import Types
import Control.Monad

instance Connectable Connection () () where
  connect cred () = do putStrLn "Connecting to Redis."
                       redis <- Redis.connect (toConnectInfo cred)
                       return (redis, ())
  disconnect conn = void $ runRedis conn quit

md5Exists :: Connection -> T.Text -> IO Bool
md5Exists redis md5 = runRedis redis $ do
  result <- exists (Enc.encodeUtf8 md5)
  return $ case result of
    Left _  -> False
    Right b -> b

retrieveMd5 :: Connection -> T.Text -> IO (Maybe T.Text)
retrieveMd5 redis md5 = do result <- runRedis redis $ get (Enc.encodeUtf8 md5)
                           return $ case result of
                             Left _  -> Nothing
                             Right m -> fmap Enc.decodeUtf8 m

toConnectInfo cred =
  defaultConnectInfo{ connectHost = undefined --redisHost config
                    , connectPort = undefined --redisPort config
                    , connectAuth = undefined --Just (BS.pack $ redisPass cred)
                    }
