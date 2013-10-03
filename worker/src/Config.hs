{-# LANGUAGE OverloadedStrings #-}

module Config (Config(..), getConfig) where
import Control.Applicative
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

data Config = Config { amqpServer :: String
                     , amqpLogin :: T.Text
                     , amqpPass :: T.Text
                     , maxCompilations :: Int }

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> (v .: "amqpServer")
           <*> (v .: "amqpLogin")
           <*> (v .: "amqpPass")
           <*> (v .: "maxCompilations")

getConfig :: String -> IO Config
getConfig loc = do str <- BS.readFile loc
                   case eitherDecode str of
                     Right conf -> return conf
                     Left err -> error err
