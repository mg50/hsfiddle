{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty
import Control.Monad
import Control.Monad.Trans
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T
import qualified Data.Text as TStrict
import qualified Data.Text.Lazy.Encoding as Enc
import qualified Data.Map as M
import Data.Aeson hiding (json)
import Network.AMQP
import Control.Concurrent.MVar
import Control.Concurrent.SafeMVar
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import Pending

data CompileResult = CompileSuccess T.Text | CompileFailure T.Text
type PendingCompilations = Pending TStrict.Text CompileResult

main = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn

  declareExchange chan newExchange {exchangeName = "hsfiddle", exchangeType = "direct"}

  declareQueue chan newQueue{queueName = "uncompiled"}
  declareQueue chan newQueue{queueName = "compiled"  }
  declareQueue chan newQueue{queueName = "error"     }

  bindQueue chan "uncompiled" "hsfiddle" "uncompiled"
  bindQueue chan "compiled" "hsfiddle" "compiled"
  bindQueue chan "error" "hsfiddle" "error"

  pending <- newSafeMVar M.empty
  consumeMsgs chan "compiled" Ack (compiledCallback pending)
  consumeMsgs chan "error" Ack (errorCallback pending)
  runServer chan pending

compiledCallback = callback CompileSuccess
errorCallback = callback CompileFailure

callback :: (T.Text -> CompileResult) ->
            SafeMVar PendingCompilations ->
            (Message, Envelope) ->
            IO ()
callback ctor pending (msg, envelope) = do
  let body = msgBody msg :: BL.ByteString
  case msgReplyTo msg of
    Nothing -> return ()
    Just msgId -> modifySafeMVar pending $ \assoc -> do
      case M.lookup msgId assoc of
        Just mv -> putMVar mv $ ctor (Enc.decodeUtf8 body)
        Nothing -> return ()
      return assoc

runServer :: Channel -> SafeMVar PendingCompilations -> IO ()
runServer chan pending = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "./public")

  get "/" $ do
    file <- liftIO $ readFile "./public/html/index.html"
    html (T.pack file)

  post "/compile" $ do
    code <- param "code"
    result <- liftIO $ awaitCompilation code chan pending
    json $ jsonify result

  get "/ajax/echo/:word" $ do
    word <- param "word"
    html word

awaitCompilation :: T.Text -> Channel -> SafeMVar PendingCompilations -> IO CompileResult
awaitCompilation code chan pending = do
  id <- genMessageId
  let msg = newMsg{ msgBody = Enc.encodeUtf8 code
                  , msgDeliveryMode = Just Persistent
                  , msgID = Just id }
  publishMsg chan "hsfiddle" "uncompiled" msg
  awaitPending pending id

genMessageId :: IO TStrict.Text
genMessageId = do uuid <- UUID.V4.nextRandom
                  return . TStrict.pack $ UUID.toString uuid

jsonify :: CompileResult -> Value
jsonify (CompileSuccess js)  = object ["error" .= Null, "js" .= js]
jsonify (CompileFailure err) = object ["error" .= err, "js" .= Null]
