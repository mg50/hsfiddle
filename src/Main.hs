{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty
import Control.Monad
import Control.Monad.Trans
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T

main = scotty 8001 $ do
  middleware $ staticPolicy (noDots >-> addBase "./public")

  get "/" $ do
    file <- liftIO $ readFile "./public/html/index.html"
    html (T.pack file)
