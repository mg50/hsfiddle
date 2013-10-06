{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty
import Control.Monad.Trans
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Data.Aeson       (object, (.=))
import Data.Aeson.Types (emptyArray, Pair)
import qualified Data.Text      as TStrict
import qualified Data.Text.Lazy as T
import qualified Data.UUID      as UUID
import qualified Data.UUID.V4   as UUID.V4

import CompilationCache (Cache (), newCompilationCache, requestCompilation, requestResult)

main :: IO ()
main = do
  cache <- newCompilationCache
  runServer cache

runServer :: Cache -> IO ()
runServer cache = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "./public")
  middleware logStdoutDev
  middleware $ gzip def

  get "/" $ do
    fileContents <- liftIO $ readFile "./public/html/index.html"
    html (T.pack fileContents)

  post "/compile" $ do
    code <- param "code"
    uuid <- liftIO $ genMessageId
    liftIO $ requestCompilation cache uuid code
    jsonResponse ["errors" .= emptyArray, "uuid" .= uuid]

  get "/result/:uuid" $ do
    uuid   <- param "uuid"
    result <- liftIO $ requestResult cache uuid
    case result of
      Just code -> jsonResponse ["errors" .= emptyArray, "ready" .= True,  "js" .= code]
      Nothing   -> jsonResponse ["errors" .= emptyArray, "ready" .= False, "js" .= ("" :: T.Text)]

  get "/ajax/echo/:word" $ do
    word <- param "word"
    html word

genMessageId :: IO TStrict.Text
genMessageId = do uuid <- UUID.V4.nextRandom
                  return . TStrict.pack $ UUID.toString uuid

jsonResponse :: [Pair] -> ActionM ()
jsonResponse = json . object
