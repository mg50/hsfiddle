{-# LANGUAGE OverloadedStrings #-}

module CompilationCache ( newCompilationCache
                        , requestCompilation
                        , requestResult
                        , Cache ()
                        ) where

import qualified Data.Text.Encoding as E

import Data.Text                    (Text ())
import Data.ByteString              (ByteString ())
import Data.ByteString.Lazy         (toStrict, fromStrict)
import Database.Redis               ( Connection ()
                                    , defaultConnectInfo
                                    , connect
                                    , get
                                    , del
                                    , setex
                                    , runRedis
                                    , connectMaxIdleTime
                                    )
import Network.AMQP                 ( Channel      ()
                                    , Message      ()
                                    , Envelope     ()
                                    , Ack          (Ack)
                                    , DeliveryMode (Persistent)
                                    , newQueue
                                    , newExchange
                                    , newMsg
                                    , publishMsg
                                    , declareQueue
                                    , declareExchange
                                    , exchangeType
                                    , exchangeName
                                    , msgReplyTo
                                    , msgID
                                    , msgDeliveryMode
                                    , ackEnv
                                    , msgBody
                                    , openConnection
                                    , openChannel
                                    , consumeMsgs
                                    , bindQueue
                                    , queueName
                                    )

data Cache = Cache { redis   :: Connection
                   , amqp    :: Channel
                   }

newCompilationCache :: IO Cache
newCompilationCache = do
  redisConnection <- connect defaultConnectInfo { connectMaxIdleTime = 600 }
  amqpChannel     <- joinAMQP
  let cache = Cache redisConnection amqpChannel
  consumeMsgs amqpChannel "compiled" Ack (cacheResponse redisConnection)
  return cache

requestCompilation :: Cache -> Text -> ByteString -> IO ()
requestCompilation cache uuid code = do
  let message = newMsg { msgBody         = fromStrict code
                       , msgDeliveryMode = Just Persistent
                       , msgID           = Just uuid
                       }
  publishMsg (amqp cache) "hsfiddle" "uncompiled" message

requestResult :: Cache -> ByteString -> IO (Maybe ByteString)
requestResult cache uuid = do
  cacheResult <- runRedis redisConnection $ get uuid
  case cacheResult of
    Left _            -> return Nothing
    Right Nothing     -> return Nothing
    Right (Just code) -> do
      runRedis redisConnection $ del [uuid]
      return $ Just code
  where redisConnection = redis cache

cacheResponse :: Connection -> (Message, Envelope) -> IO ()
cacheResponse redisConnection (message, envelope) = do
  ackEnv envelope
  case msgReplyTo message of
    Nothing        -> return ()
    Just messageId -> do
      let (uuid, compiledCode) = (E.encodeUtf8 messageId, toStrict $ msgBody message)
      runRedis redisConnection $ setex uuid 30000 compiledCode
      return ()

joinAMQP :: IO Channel
joinAMQP = do
  amqpConnection <- openConnection "127.0.0.1" "/" "guest" "guest"
  amqpChannel    <- openChannel amqpConnection

  declareExchange amqpChannel newExchange {exchangeName = "hsfiddle", exchangeType = "direct"}

  declareQueue amqpChannel newQueue {queueName = "uncompiled"}
  declareQueue amqpChannel newQueue {queueName = "compiled"  }

  bindQueue amqpChannel "uncompiled" "hsfiddle" "uncompiled"
  bindQueue amqpChannel "compiled" "hsfiddle" "compiled"

  return amqpChannel
