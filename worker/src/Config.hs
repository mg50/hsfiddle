{-# LANGUAGE OverloadedStrings #-}

module Config (Config(..), getConfig) where
import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

data Config = Config { ampqServer :: String
                     , ampqLogin :: String
                     , ampqPass :: String }

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> (v .: "ampqServer")
           <*> (v .: "ampqLogin")
           <*> (v .: "ampqPass")

getConfig :: String -> IO Config
getConfig loc = do str <- BS.readFile loc
                   case decode str of
                     Just conf -> return conf
                     Nothing -> error $ "could not parse json at " ++ loc
