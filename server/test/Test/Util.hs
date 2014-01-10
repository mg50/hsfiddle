{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Test.Util where

import qualified Data.Text as T
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T
import Database.PostgreSQL.Simple hiding (connect)
import qualified Database.PostgreSQL.Simple as PG
import Database.Redis.Mock
import Types (PendingCompilations, service, connect, disconnect, Config(..))
import PG
import ParseCredentials
import Pending (newPending)
import App (runApp')
import Text.Hastache (encodeStr)
import Web.Scotty (scottyApp)
import Service.Mock
import Test.Types
import Network.Wai
import Network.Wai.Test
import qualified Network.HTTP.Types as H
import qualified Data.Aeson as Aeson
import Data.Monoid ((<>))
import Test.Hspec (shouldBe)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EncL


newTestConfig enqueuer = do
  cred                    <- readCredentials "./config/test.json"
  pgConn :: PG.Connection <- connect cred ()
  redis                   <- newMockRedis
  return $ Config{
    service = TestEnv{postgres'=pgConn, mockRedis=redis, enqueuer=enqueuer},
    compileTimeout = Nothing,
    port = undefined
    }

clearPending pending = atomicModifyIORef' pending $ \_ -> (M.empty, ())

getTemplate = readFile "./public/html/index.html"

clearDb = do
  cred <- readCredentials "./config/test.json"
  pgConn :: PG.Connection <- connect cred ()
  execute_ pgConn "DELETE FROM fiddles;"
  disconnect pgConn

withApp m = do
  pending <- newPending
  template <- readFile  "./public/html/index.html"
  config <- newTestConfig $ error "no enqueuer specified"
  app <- scottyApp $ runApp' (encodeStr template) config pending
  runSession m app

withPgConn f = do
  pending <- newPending
  template <- readFile  "./public/html/index.html"
  config <- newTestConfig undefined
  app <- scottyApp $ runApp' (encodeStr template) config pending
  runSession (f . postgres' $ service config) app

withEnqueuer enq m = do
  pending <- newPending
  template <- readFile  "./public/html/index.html"
  config <- newTestConfig (enq pending)
  app <- scottyApp $ runApp' (encodeStr template) config pending
  runSession m app

withTimeout timeout enq m = do
  pending <- newPending
  template <- readFile  "./public/html/index.html"
  config <- newTestConfig (enq pending)
  let config' = config{compileTimeout = timeout}
  app <- scottyApp $ runApp' (encodeStr template) config' pending
  runSession m app

withMockRedis enqueuer f = do
  pending <- newPending
  template <- readFile  "./public/html/index.html"
  config <- newTestConfig enqueuer
  app <- scottyApp $ runApp' (encodeStr template) config pending
  runSession (f . mockRedis $ service config) app

getReq url = do
  let req = defaultRequest{ pathInfo = T.splitOn "/" (fmtUrl url) }
  request req

postReq url body = do
  let headers = [(H.hContentType, "application/x-www-form-urlencoded")]
      req = defaultRequest{ pathInfo = T.splitOn "/" (fmtUrl url)
                          , requestMethod = H.methodPost
                          , requestHeaders = headers
                          }
      sreq = SRequest req body
  srequest sreq

fmtUrl url = T.pack $ case T.unpack url of
  ('/':u) -> u
  u       -> u

manuallyCreateFiddle conn slug version hs css html = void . liftIO . PG.withTransaction conn $ do
  let q = "INSERT INTO fiddles (slug, version, hs, css, html) VALUES (?, ?, ?, ?, ?)"
  PG.execute conn q (slug :: String, version :: Int, hs :: String, css :: String, html :: String)

latestVersion conn slug = do
  let q = "SELECT MAX(version) FROM fiddles WHERE slug=?"
  res :: [Only Int] <- query conn q (Only slug)
  case res of
    []         -> error $ "could not find any fiddle with slug " ++ slug
    (Only v:_) -> return v

assertJsonIs json resp = do
  case Aeson.decode (simpleBody resp) :: Maybe Aeson.Value of
    Nothing    -> error $ "could not decode json response " ++ show (simpleBody resp)
    Just json' -> liftIO $ json `shouldBe` json'

toLBS = EncL.encodeUtf8 . TL.fromStrict
