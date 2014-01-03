{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module AMQP (awaitCompilation) where
import Network.AMQP
import Types
import Pending
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy.Encoding as Enc
import qualified Data.Text.Lazy as T
import qualified Data.Text as TStrict
import Data.Text (pack)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4

instance Connectable Connection Channel PendingCompilations where
  connect cred pending = do
    putStrLn "Connecting to AMQP."
    (conn, chan) <- joinAMQP cred
    listen chan pending
    return (conn, chan)

  disconnect conn = do putStrLn "Disconnecting from AMQP."
                       closeConnection conn

awaitCompilation code config pending = do
  msgId <- genMessageId
  let msg = newMsg{ msgBody = Enc.encodeUtf8 (T.fromStrict code)
                  , msgDeliveryMode = Just Persistent
                  , msgID = Just msgId }
  publishMsg (amqpChan config) "hsfiddle" "uncompiled" msg
  awaitPending pending msgId (compileTimeout config)

genMessageId :: IO TStrict.Text
genMessageId = do uuid <- UUID.V4.nextRandom
                  return . pack $ UUID.toString uuid

setupAMQP cred pending = do
  (conn, chan) <- joinAMQP cred
  listen chan pending
  return (conn, chan)

joinAMQP :: Credentials -> IO (Connection, Channel)
joinAMQP cred = do
  conn <- openConnection (amqpHost cred) "/" (pack $ amqpUser cred) (pack $ amqpPass cred)
  chan <- openChannel conn

  declareExchange chan newExchange {exchangeName = "hsfiddle", exchangeType = "direct"}

  declareQueue chan newQueue{queueName = "uncompiled"}
  declareQueue chan newQueue{queueName = "compiled"  }
  declareQueue chan newQueue{queueName = "error"     }

  bindQueue chan "uncompiled" "hsfiddle" "uncompiled"
  bindQueue chan "compiled" "hsfiddle" "compiled"
  bindQueue chan "error" "hsfiddle" "error"

  return (conn, chan)


listen :: Channel -> PendingCompilations -> IO ()
listen chan pending = do
  consumeMsgs chan "compiled" Ack (compiledCallback pending)
  consumeMsgs chan "error" Ack (errorCallback pending)
  return ()

compiledCallback, errorCallback :: PendingCompilations -> (Message, Envelope) -> IO ()
compiledCallback = callback (const CompileSuccess)
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
