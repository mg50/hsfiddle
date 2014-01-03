{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Data.Monoid
import Data.Maybe
import Data.Default
import Data.Aeson hiding (json)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Control.Concurrent

import System.Posix.Signals

import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip

import qualified Network.AMQP as AMQP
import qualified Database.Redis as Redis
import qualified Database.PostgreSQL.Simple as PG

import qualified Data.Hash.MD5 as MD5

import Text.Hastache
import Text.Hastache.Context

import Pending
import Fiddle
import ParseCredentials
import Types
import PG (saveFiddle, retrieveFiddle)
import AMQP (awaitCompilation)
import Redis (retrieveMd5, md5Exists)

main :: IO ()
main = do
  pending  <- newPending
  cred     <- readCredentials "./credentials.json"

  (amqpConn :: AMQP.Connection, amqpChan) <- connect cred pending
  (redisConn :: Redis.Connection , _)     <- connect cred ()
  (pgConn :: PG.Connection, _)            <- connect cred ()

  --config   <- readConfig amqp redis postgres <$> Strict.readFile "./config.json"
  let config = Config amqpConn amqpChan redisConn pgConn (Just 5000000) 3000
  template <- encodeStr <$> readFile "./public/html/index.html"

  mainTid <- myThreadId
  let stop = gracefulExit mainTid config
  forM_ [sigINT, sigTERM, sigQUIT, sigHUP] $ \sig ->
    installHandler sig (Catch stop) Nothing

  putStrLn "Starting webserver."
  runServer template config pending

runServer template config pending = scotty (port config) $ do
  middleware $ staticPolicy (noDots >-> addBase "./public")
  middleware logStdoutDev
  middleware $ gzip def

  get "/" $ do
    fileContents <- liftIO $ readFile "./public/html/index.html"
    html (TL.pack fileContents)

  get "/:slug" $ do
    slug <- param "slug"
    serveFiddle template config slug 0

  get "/:slug/:version" $ do
    slug    <- param' "slug"
    version <- param' "version"
    serveFiddle template config slug (read $ T.unpack version)

  post "/save" $ do
    html'  <- param' "html"
    css    <- param' "css"
    hs     <- param' "hs"
    slug'  <- param' "slug"
    let slug = if T.null slug' then Nothing else Just slug'
    liftIO $ saveFiddle slug (postgres config) hs css html'

  post "/compile" $ do
    code <- param' "code"
    let md5hex = MD5.md5s . MD5.Str $ T.unpack code
        md5hexT = T.pack md5hex
    result <- liftIO $ do
      redisHasHash <- md5Exists (redis config) md5hexT
      if redisHasHash
        then return (Just CompileSuccess)
        else awaitCompilation (md5hexT <> code) config pending
    jsonify md5hex result

  get "/js/:md5" $ do
    md5 <- param "md5"
    setHeader "Content-Type" "application/javascript"
    void . liftIO $ retrieveMd5 (redis config) md5

  get "/ajax/echo/:word" $ do
    word <- param "word"
    html word

param' = fmap TL.toStrict . param

serveFiddle template config slug version = do
  maybeFiddle <- liftIO $ retrieveFiddle (postgres config) slug version
  let fiddle   = fromMaybe def maybeFiddle
  rendered <- hastacheStr defaultConfig template (toContext fiddle)
  raw rendered

toContext (Fiddle hs css html) = mkStrContext $ ctx
  where ctx "hs"   = MuVariable hs
        ctx "css"  = MuVariable css
        ctx "html" = MuVariable html

jsonify hash result = json $ case result of
  Nothing                   -> object ["timeout" .= True]
  Just CompileSuccess       -> object ["hash" .= hash]
  Just (CompileFailure err) -> object ["error" .= err]

gracefulExit tid config = do
  disconnect (amqpConn config)
  disconnect (redis config)
  disconnect (postgres config)
  killThread tid
