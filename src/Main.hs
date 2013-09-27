{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty
import Control.Monad
import Control.Monad.Trans
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T
import Compile
import Data.Aeson hiding (json)

main = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "./public")

  get "/" $ do
    file <- liftIO $ readFile "./public/html/index.html"
    html (T.pack file)

  post "/compile" $ do
    code <- param "code"
    result <- liftIO $ compile (T.unpack code)
    json (jsonify result)

  get "/ajax/echo/:word" $ do
    word <- param "word"
    html word

jsonify :: CompileResult -> Value
jsonify (CompileSuccess js) = object ["error" .= Null, "js" .= js]
jsonify (CompileError err)  = object ["error" .= err, "js" .= Null]
