{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import qualified Data.Text as T
import Control.Concurrent (forkIO, threadDelay)
import Data.Monoid ((<>))
import Control.Monad
import Control.Monad.Trans
import Text.Hastache (encodeStr)
import Test.Hspec hiding (pending)
import Network.Wai.Test
import Data.Aeson

import App
import Pending
import Web.Scotty
import Web.Scotty.Trans
import Types (service, PendingCompilations, CompileResult(..))
import Database.Redis.Mock

import Service.Mock
import Test.Util
import Test.Types


spec = describe "runApp'" $ before clearDb $ do
  describe "/" $ do
    it "returns the index" $ withApp $ do
      resp <- getReq "/"
      assertStatus 200 resp
      assertBodyContains "hsfiddle" resp

  describe "retrieving fiddles" $ do
    context "no version specified" $ do
      it "redirects when not found" $ withApp $ do
        resp <- getReq "/asdfgh"
        assertStatus 302 resp

      it "serves the fiddle when found" $ withPgConn $ \conn -> do
        manuallyCreateFiddle conn "asdfgh" 0 "" "" "hello there"
        resp <- getReq "/asdfgh"
        assertStatus 200 resp
        assertBodyContains "hello there" resp

      it "serves version 0 of the fiddle" $ withPgConn $ \conn -> do
        manuallyCreateFiddle conn "asdfgh" 1 "" "" "goodbye"
        manuallyCreateFiddle conn "asdfgh" 0 "" "" "hello there"
        resp <- getReq "/asdfgh"
        assertStatus 200 resp
        assertBodyContains "hello there" resp

    context "version specified" $ do
      it "redirects when not found" $ withApp $ do
        resp <- getReq "/asdfgh/24"
        assertStatus 302 resp

      it "redirects when not found but similar slug exists" $ withPgConn $ \conn -> do
        manuallyCreateFiddle conn "asdfgh" 1 "" "" "hello there"
        resp <- getReq "/asdfgh/2"
        assertStatus 302 resp

      it "serves the fiddle when found" $ withPgConn $ \conn -> do
        manuallyCreateFiddle conn "asdfgh" 4 "" "" "hello there"
        resp <- getReq "/asdfgh/4"
        assertStatus 200 resp
        assertBodyContains "hello there" resp

  describe "saving a fiddle" $ do
    it "saves a new fiddle correctly if no slug is specified" $ withApp $ do
      resp <- postReq "/save" "hs=a&css=b&html=c"
      assertStatus 200 resp

    it "updates the version if slug already exists" $ withPgConn $ \conn -> do
      manuallyCreateFiddle conn "asdfgh" 0 "" "" ""
      resp <- postReq "/save" "hs=a&css=b&html=c&slug=asdfgh"
      assertStatus 200 resp
      liftIO $ latestVersion conn "asdfgh" `shouldReturn` 1

    it "updates the latest version" $ withPgConn $ \conn -> do
      forM_ [0..4] $ \i ->
        manuallyCreateFiddle conn "asdfgh" i "" "" "hello there"
      resp <- postReq "/save" "hs=a&css=b&html=c&slug=asdfgh"
      assertStatus 200 resp
      liftIO $ latestVersion conn "asdfgh" `shouldReturn` 5

  describe "compilation" $ do
    let msgId = "123"
        compile result timer pending code = do
          forkIO $ do threadDelay timer
                      deliverPending pending msgId result
          return msgId

    it "raises an error if submitted with no code" $ withApp $ do
      resp <- postReq "/compile" "hs="
      assertStatus 500 resp

    it "sends back code md5 when compilation succeeds" $ do
      let code = "somecode" :: T.Text
          compileSuccessfully = compile CompileSuccess 10000
      withEnqueuer compileSuccessfully $ do
        resp <- postReq "/compile" $ "hs=" <> toLBS code
        assertStatus 200 resp
        let obj = object ["hash" .= (toLBS $ T.pack $ toMD5 code)]
        assertJsonIs obj resp

    it "sends back md5 from cash when available, skipping compilation" $ do
      let code = "somecode" :: T.Text
          md5 = T.pack (toMD5 code)
      withMockRedis (error "this shouldn't be called; compilation shouldn't be enqueued") $ \mr -> do
        liftIO $ cache mr md5 ""
        resp <- postReq "/compile" $ "hs=" <> toLBS code
        assertStatus 200 resp
        let obj = object ["hash" .= (toLBS md5)]
        assertJsonIs obj resp

    it "reports compilation errors" $ do
      let code = "somecode" :: T.Text
          failCompilation = compile (CompileFailure "couldn't compile") 10000

      withEnqueuer failCompilation $ do
        resp <- postReq "/compile" $ "hs=" <> toLBS code
        assertStatus 500 resp
        let obj = object ["error" .= ("couldn't compile" :: String)]
        assertJsonIs obj resp

    it "reports compilation timeouts" $ do
      let code = "somecode" :: T.Text
          compileSlowly = compile CompileSuccess 1000000

      withTimeout (Just 1000) compileSlowly $ do
        resp <- postReq "/compile" $ "hs=" <> toLBS code
        assertStatus 408 resp
        let obj = object ["timeout" .= True]
        assertJsonIs obj resp
