{-# LANGUAGE OverloadedStrings #-}

module Main where
import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Compile

main = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn

  consumeMsgs chan "uncompiled" Ack (compile chan)
  getLine
  closeConnection conn
  putStrLn "connection closed"

compile chan (requestMsg, envelope) =
  case msgID requestMsg of
    Nothing -> return ()
    Just requestId -> do
      result <- compile msg
      let (txt, queue) = case result of
                           CompileSuccess js -> (js, "compiled")
                           CompileError err  -> (err, "error")
          replyMsg = newMsg{ msgBody = txt
                           , msgDeliveryMode = Just Persistent
                           , msgReplyTo = Just requestId }
      publishMsg chan "hsfiddle" queue replyMsg
