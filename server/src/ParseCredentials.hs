{-# LANGUAGE OverloadedStrings #-}

module ParseCredentials (readCredentials) where

import Types
import Control.Applicative
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

instance FromJSON Credentials where
  parseJSON (Object v) =
    Credentials <$> (v .: "amqpHost")
                <*> (v .: "amqpUser")
                <*> (v .: "amqpPass")
                <*> (v .: "redisHost")
                <*> (v .: "redisPass")
                <*> (v .: "pgHost")
                <*> (v .: "pgUser")
                <*> (v .: "pgPass")
                <*> (v .: "pgDb")

readCredentials :: String -> IO Credentials
readCredentials loc = do str <- BS.readFile loc
                         case eitherDecode str of
                           Right conf -> return conf
                           Left err -> error err
