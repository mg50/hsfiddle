{-# LANGUAGE OverloadedStrings #-}

module Main where
import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.Lazy.Encoding as Enc
import Control.Monad
import Control.Concurrent
import System.Posix.Signals
import Config
import Compile
import Semaphore

main = do
  putStrLn "Starting worker..."
  tid <- myThreadId
  Config ampqServer ampqLogin ampqPass maxCompilations <- getConfig "./config.json"
  conn <- openConnection ampqServer "/" ampqLogin ampqPass
  chan <- openChannel conn
  putStrLn "Connected to AMQP server"

  sem <- newSemaphore maxCompilations
  tag <- consumeMsgs chan "uncompiled" Ack (tryCompile chan sem)

  let stop = gracefulExit chan conn tag sem tid
  forM_ [sigINT, sigTERM] $ \sig -> installHandler sig (Catch stop) Nothing
  getLine
  stop

tryCompile chan sem (requestMsg, envelope) = do
  ackEnv envelope
  acquire sem
  case msgID requestMsg of
    Nothing -> return ()
    Just requestId -> do
      print "About to compile"
      result <- compile $ lazyBytestringToText $ msgBody requestMsg
      let (txt, queue) = case result of
                           CompileSuccess js -> (js, "compiled")
                           CompileError err  -> (err, "error")
          replyMsg = newMsg{ msgBody = textToLazyBytestring txt
                           , msgDeliveryMode = Just Persistent
                           , msgReplyTo = Just requestId }
      print "Finished compiling"
      publishMsg chan "hsfiddle" queue replyMsg
  release sem

gracefulExit chan conn tag sem tid = do
  putStrLn "Waiting for compilations to finish..."
  cancelConsumer chan tag
  awaitDrain sem
  closeConnection conn
  putStrLn "Exiting..."
  killThread tid

lazyBytestringToText = TLazy.toStrict . Enc.decodeUtf8
textToLazyBytestring = Enc.encodeUtf8 . TLazy.fromStrict
