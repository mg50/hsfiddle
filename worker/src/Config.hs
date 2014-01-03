{-# LANGUAGE OverloadedStrings #-}

module Config (Config(..), getConfig) where
import Control.Applicative
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

data Config = Config { amqpServer :: String
                     , amqpLogin :: String
                     , amqpPass :: String
                     , redisHost :: String
                     , redisTimeout :: Int
                     , maxCompilations :: Int
                     }

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> (v .: "amqpHost")
           <*> (v .: "amqpUser")
           <*> (v .: "amqpPass")
           <*> (v .: "redisHost")
           <*> (v .: "redisTimeout")
           <*> (v .: "maxCompilations")

getConfig :: String -> IO Config
getConfig loc = do str <- BS.readFile loc
                   case eitherDecode str of
                     Right conf -> return conf
                     Left err -> error err
