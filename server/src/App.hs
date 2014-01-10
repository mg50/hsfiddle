{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module App (runApp', runApp, toMD5) where

import Data.Monoid
import Data.Aeson hiding (json)
import qualified Data.Text.Lazy.Encoding as EncL
import qualified Data.ByteString as BS

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Control.Concurrent

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Hastache
import Text.Hastache.Context

import Network.HTTP.Types.Status
import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip

import qualified Data.Hash.MD5 as MD5

import Service
import Service.HSFiddle
import Pending
import Types

runApp template config pending =
  scotty (port config) $ runApp' (encodeStr template) config pending

runApp' :: (Service (Config s)) =>
           BS.ByteString ->
           Config s ->
           PendingCompilations ->
           ScottyM ()
runApp' template config pending = do
  middleware $ staticPolicy (noDots >-> addBase "./public")
--  middleware logStdoutDev
  middleware $ gzip def

  get "/" $ do
    fileContents <- liftIO $ readFile "./public/html/index.html"
    html (TL.pack fileContents)

  post "/save" $ do
    html'  <- param' "html"
    css    <- param' "css"
    hs     <- param' "hs"
    slug   <- fmap Just (param' "slug") `rescue` const (return Nothing)
    liftIO $ saveFiddle config slug hs css html'

  post "/compile" $ do
    code <- param' "hs"
    when (T.null code) $ raise "invalid hs"
    let md5hex = toMD5 code
        md5hexT = T.pack md5hex
    result <- liftIO $ do
      isCached <- jsIsCached config md5hexT
      if isCached
        then return CompileSuccess
        else awaitCompilation config (md5hexT <> code) (compileTimeout config) pending
    jsonify md5hex result

  get "/compiled/:md5" $ do
    md5 <- param "md5"
    setHeader "Content-Type" "application/javascript"
    compiled <- liftIO $ retrieveCompiled config md5
    maybe (status status404) (raw . textToLazyByteString) compiled

  get "/ajax/echo/:word" $ html =<< param "word"

  get "/:slug" $ do
    slug <- param "slug"
    serveFiddle config slug 0 template

  get "/:slug/:version" $ do
    slug     <- param' "slug"
    version' <- param' "version"
    let version = read (T.unpack version') -- this can crash program
    serveFiddle config slug version template


serveFiddle config slug version template = do
  result <- liftIO $ retrieveFiddle config slug version
  case result of
    Just fiddle -> let ctx = toContext fiddle
                   in do rendered <- hastacheStr defaultConfig template ctx
                         html (EncL.decodeUtf8 rendered)
    Nothing     -> redirect "/"

param' = fmap TL.toStrict . param

toContext (Fiddle hs css html) = mkStrContext $ ctx
  where ctx "hs"   = MuVariable hs
        ctx "css"  = MuVariable css
        ctx "html" = MuVariable html

jsonify hash result = json =<< case result of
  CompileTimeout     -> status status408 >> return (object ["timeout" .= True])
  CompileFailure err -> status status500 >> return (object ["error" .= err])
  CompileSuccess     -> return $ object ["hash" .= hash]

textToLazyByteString = EncL.encodeUtf8 . TL.fromStrict

toMD5 str = MD5.md5s . MD5.Str $ T.unpack str
