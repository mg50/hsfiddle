{-# LANGUAGE OverloadedStrings #-}

module Main where
import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.Lazy.Encoding as Enc
import Compile

main = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn

  consumeMsgs chan "uncompiled" Ack (tryCompile chan)
  getLine
  closeConnection conn
  putStrLn "connection closed"

tryCompile chan (requestMsg, envelope) =
  case msgID requestMsg of
    Nothing -> return ()
    Just requestId -> do
      result <- compile $ lazyBytestringToText $ msgBody requestMsg
      let (txt, queue) = case result of
                           CompileSuccess js -> (js, "compiled")
                           CompileError err  -> (err, "error")
          replyMsg = newMsg{ msgBody = textToLazyBytestring txt
                           , msgDeliveryMode = Just Persistent
                           , msgReplyTo = Just requestId }
      publishMsg chan "hsfiddle" queue replyMsg

lazyBytestringToText = TLazy.toStrict . Enc.decodeUtf8
textToLazyBytestring = Enc.encodeUtf8 . TLazy.fromStrict
