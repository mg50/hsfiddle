{-# LANGUAGE MultiParamTypeClasses #-}

module Redis (md5Exists, retrieveCachedJS) where
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
  disconnect conn = void $ do putStrLn "Disconnecting from Redis."
                              runRedis conn quit

md5Exists :: Connection -> T.Text -> IO Bool
md5Exists redis md5 = runRedis redis $ do
  result <- exists (Enc.encodeUtf8 md5)
  return $ case result of
    Left _  -> False
    Right b -> b

retrieveCachedJS :: Connection -> T.Text -> IO (Maybe T.Text)
retrieveCachedJS redis md5 = do
  result <- runRedis redis $ get (Enc.encodeUtf8 md5)
  return $ case result of
    Left _  -> Nothing
    Right m -> fmap Enc.decodeUtf8 m

toConnectInfo cred =
  defaultConnectInfo{ connectHost = redisHost cred
                    , connectAuth = Just . toBS $ redisPass cred
                    }
  where toBS = Enc.encodeUtf8 . T.pack
