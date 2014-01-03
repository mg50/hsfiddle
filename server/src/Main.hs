{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty
import Control.Monad.Trans
import Control.Applicative
import Control.Concurrent
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Data.Aeson hiding (json)
import Network.AMQP
import Pending
import System.Posix.Signals
import Data.Monoid
import Data.Maybe
import Data.Default
import Fiddle

main :: IO ()
main = do
  pending  <- newPending
  cred     <- readCredentials <$> Strict.readFile "./credentials.json"

  (amqpConn :: AMQP.Connection, amqpChan) <- connect cred pending
  (redisConn :: Redis.Redis , _)          <- connect cred ()
  (pgConn :: PG.PG, _)                    <- connect cred ()

  config   <- readConfig amqp redis postgres <$> Strict.readFile "./config.json"

  template <- encodeStr <$> readFile "./public/html/index.html"

  html (T.pack fileContents)

  let stop = gracefulExit config
  forM_ [sigINT, sigTERM, sigQUIT, sigHUP] $ \sig ->
    installHandler sig (Catch stop) Nothing

  putStrLn "Starting webserver."
  runServer template config pending

runServer :: Channel -> PendingCompilations -> IO ()
runServer template config pending = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "./public")
  middleware logStdoutDev
  middleware $ gzip def

  get "/" $ do
    fileContents <- liftIO $ readFile "./public/html/index.html"
    html (T.pack fileContents)

  get "/:slug" $ do
    slug <- param "slug"
    serveFiddle config template slug 0

  get "/:slug/:version" $ do
    slug <- param "slug"
    version  <- param "version"
    serveFiddle config template slug (read version)

  post "/save" $ do
    html'  <- param "html"
    css    <- param "css"
    hs     <- param "hs"
    slug'  <- param "slug"
    let slug = if T.empty slug' then Nothing else Just slug
    saveFiddle (postgres config) hs css html'

  post "/compile" $ do
    code <- param "code"
    let md5 = MD5.hash code
    redisHasHash <- md5Exists (redis config) md5
    result <- if redisHasHash
                 then return CompileSuccess
                 else liftIO $ awaitCompilation (md5 <> code) config pending
    jsonify md5Hash result

  get "/js/:md5" $ do
    md5 <- param "md5"
    setHeader "Content-Type" "application/javascript"
    retrieveMd5 (redis config) md5

  get "/ajax/echo/:word" $ do
    word <- param "word"
    html word

serveFiddle template config slug version = do
  maybeFiddle <- retrieveFiddle (postgres config) slug (read version)
  let fiddle   = fromMaybe def maybeFiddle
      rendered = hastacheStr defaultConfig template (toContext fiddle)
  html rendered

toContext (Fiddle hs css html) = ctx
  where ctx "hs"   = MuVariable hs
        ctx "css"  = MuVariable css
        ctx "html" = MuVariable html

jsonify :: CompileResult -> Value
jsonify hash result = json $ case result of
  Nothing                   -> object ["timeout" .= True]
  Just CompileSuccess       -> object ["hash" .= hash]
  Just (CompileFailure err) -> object ["error" .= err]

gracefulExit config = do mapM_ disconnect conns
                         myThreadId >>= killThread
  where conns = map ($ config) [amqpConn, redisConn, pgConn]
