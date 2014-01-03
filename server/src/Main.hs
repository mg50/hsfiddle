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

main :: IO ()
main = do
  pending  <- newPending
  cred     <- readCredentials <$> Strict.readFile "./credentials.json"

  (amqpConn :: AMQP.Connection, amqpChan) <- connect cred pending
  (redisConn :: Redis.Redis , _)          <- connect cred ()
  (pgConn :: PG.PG, _)                    <- connect cred ()

  config   <- readConfig amqp redis postgres <$> Strict.readFile "./config.json"

  let stop = gracefulExit config
  forM_ [sigINT, sigTERM, sigQUIT, sigHUP] $ \sig ->
    installHandler sig (Catch stop) Nothing

  putStrLn "Starting webserver."
  runServer config pending

runServer :: Channel -> PendingCompilations -> IO ()
runServer redis chan pending = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "./public")
  middleware logStdoutDev
  middleware $ gzip def

  get "/" $ do
    fileContents <- liftIO $ readFile "./public/html/index.html"
    html (T.pack fileContents)

  get "/:fiddleId" $ do
    fiddleId <- param "fiddleId"
    retrieveFiddle (postgres config) fiddleId 0

  get "/:fiddleId/:version" $ do
    fiddleId <- param "fiddleId"
    version  <- param "version"
    fiddle <- retrieveFiddle (postgres config) fiddleId (read version)

  post "/save" $ do
    html' <- param "html"
    css <- param "css"
    hs <- param "hs"
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

jsonifyFiddle (Fiddle hs css html) = object ["hs" .= hs, "css" .= css, "html" .= html]

jsonify :: CompileResult -> Value
jsonify hash result = json $ case result of
  Nothing                   -> object ["timeout" .= True]
  Just CompileSuccess       -> object ["hash" .= hash]
  Just (CompileFailure err) -> object ["error" .= err]

gracefulExit config = do mapM_ disconnect conns
                         myThreadId >>= killThread
  where conns = map ($ config) [amqpConn, redisConn, postgresConn]
