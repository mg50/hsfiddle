{-# LANGUAGE OverloadedStrings #-}

module Main where
import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

main = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn

  consumeMsgs chan "uncompiled" Ack (compile chan)
  getLine
  closeConnection conn
  putStrLn "connection closed"

compile chan (requestMsg, envelope) = do
  let body = "asdf"
  case msgID requestMsg of
    Nothing -> return ()
    Just requestId -> do
      let replyMsg = newMsg{ msgBody = "alert(4);"
                           , msgDeliveryMode = Just Persistent
                           , msgReplyTo = Just requestId }
      publishMsg chan "hsfiddle" "compiled" replyMsg
