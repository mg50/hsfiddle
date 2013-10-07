{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty
import Control.Monad.Trans
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import qualified Data.Text.Lazy as T
import qualified Data.Text as TStrict
import qualified Data.Text.Lazy.Encoding as Enc
import Data.Aeson hiding (json)
import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import Pending

data CompileResult = CompileSuccess T.Text | CompileFailure T.Text
type PendingCompilations = Pending TStrict.Text CompileResult

main :: IO ()
main = do
  chan <- joinAMQP
  pending <- newPending
  consumeMsgs chan "compiled" Ack (compiledCallback pending)
  consumeMsgs chan "error" Ack (errorCallback pending)
  runServer chan pending

compiledCallback, errorCallback :: PendingCompilations -> (Message, Envelope) -> IO ()
compiledCallback = callback CompileSuccess
errorCallback = callback CompileFailure

callback :: (T.Text -> CompileResult) ->
            PendingCompilations ->
            (Message, Envelope) ->
            IO ()
callback ctor pending (msg, envelope) = do
  ackEnv envelope
  let bod = msgBody msg :: BL.ByteString
  case msgReplyTo msg of
    Just msgId -> deliverPending pending msgId $ ctor (Enc.decodeUtf8 bod)
    Nothing    -> return ()

runServer :: Channel -> PendingCompilations -> IO ()
runServer chan pending = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "./public")
  middleware logStdoutDev
  middleware $ gzip def

  get "/" $ do
    fileContents <- liftIO $ readFile "./public/html/index.html"
    html (T.pack fileContents)

  post "/compile" $ do
    code <- param "code"
    result <- liftIO $ awaitCompilation code chan pending
    json $ jsonify result

  get "/ajax/echo/:word" $ do
    word <- param "word"
    html word

awaitCompilation :: T.Text -> Channel -> PendingCompilations -> IO CompileResult
awaitCompilation code chan pending = do
  msgId <- genMessageId
  let msg = newMsg{ msgBody = Enc.encodeUtf8 code
                  , msgDeliveryMode = Just Persistent
                  , msgID = Just msgId }
  publishMsg chan "hsfiddle" "uncompiled" msg
  awaitPending pending msgId

genMessageId :: IO TStrict.Text
genMessageId = do uuid <- UUID.V4.nextRandom
                  return . TStrict.pack $ UUID.toString uuid

jsonify :: CompileResult -> Value
jsonify (CompileSuccess js)  = object ["error" .= Null, "js" .= js]
jsonify (CompileFailure err) = object ["error" .= err, "js" .= Null]

joinAMQP = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn

  declareExchange chan newExchange {exchangeName = "hsfiddle", exchangeType = "direct"}

  declareQueue chan newQueue{queueName = "uncompiled"}
  declareQueue chan newQueue{queueName = "compiled"  }
  declareQueue chan newQueue{queueName = "error"     }

  bindQueue chan "uncompiled" "hsfiddle" "uncompiled"
  bindQueue chan "compiled" "hsfiddle" "compiled"
  bindQueue chan "error" "hsfiddle" "error"

  return chan
