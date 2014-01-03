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
import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import Pending

type PendingCompilations = Pending TStrict.Text TStrict.Text

main :: IO ()
main = do
  chan <- joinAMQP
  pending <- newPending
  let callback = compileCallback pending
  consumeMsgs chan "compiled" Ack callback
  consumeMsgs chan "error" Ack callback
  runServer chan pending

runServer :: Channel -> PendingCompilations -> IO ()
runServer chan pending = scotty 80 $ do
  middleware $ staticPolicy (noDots >-> addBase "./public")
  middleware logStdoutDev
  middleware $ gzip def

  get "/" $ do
    fileContents <- liftIO $ readFile "./public/html/index.html"
    html (T.pack fileContents)

  post "/compile" $ do
    code <- param "code"
    result <- liftIO $ awaitCompilation code chan pending
    text $ T.fromStrict result
    header "Content-Type" "application/json"

  get "/ajax/echo/:word" $ do
    word <- param "word"
    html word

compileCallback :: PendingCompilations ->
                   (Message, Envelope) ->
                   IO ()
compileCallback pending (msg, envelope) = do
  ackEnv envelope
  let bod = msgBody msg :: BL.ByteString
  case msgReplyTo msg of
    Just msgId -> deliverPending pending msgId (T.toStrict $ Enc.decodeUtf8 bod)
    Nothing    -> return ()

awaitCompilation :: T.Text -> Channel -> PendingCompilations -> IO TStrict.Text
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
