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
import Util
import Config
import Compile
import Semaphore

main = do
  putStrLn "Starting worker..."
  Config ampqServer ampqLogin ampqPass maxCompilations <- getConfig "./config.json"
  conn <- openConnection ampqServer "/" ampqLogin ampqPass
  chan <- openChannel conn
  putStrLn "Connected to AMQP server"

  sem <- newSemaphore maxCompilations
  tag <- consumeMsgs chan "uncompiled" Ack (tryCompile chan sem)

  done <- newEmptyMVar
  let stop = gracefulExit chan conn tag sem done
  forM_ [sigINT, sigTERM, sigQUIT, sigHUP] $ \sig ->
    installHandler sig (Catch stop) Nothing
  takeMVar done

tryCompile chan sem (requestMsg, envelope) = do
  ackEnv envelope
  acquire sem
  case msgID requestMsg of
    Nothing -> return ()
    Just requestId -> do
      putStrLn "About to compile"
      (result, dt) <- withDiffTime $ compile $ lazyBytestringToText $ msgBody requestMsg
      putStrLn $ "Finished compiling in " ++ show dt

      let (txt, queue) = case result of
                           CompileSuccess js -> (js, "compiled")
                           CompileError err  -> (err, "error")
          replyMsg = newMsg{ msgBody = textToLazyBytestring txt
                           , msgDeliveryMode = Just Persistent
                           , msgReplyTo = Just requestId }

      putStrLn "About to send back code"
      (_, dt) <- withDiffTime $ publishMsg chan "hsfiddle" queue replyMsg
      putStrLn $ "Code sent in " ++ show dt
  release sem

gracefulExit chan conn tag sem done = do
  putStrLn "Waiting for compilations to finish..."
  cancelConsumer chan tag
  awaitDrain sem
  closeConnection conn
  putMVar done ()
  putStrLn "Exiting..."

lazyBytestringToText = TLazy.toStrict . Enc.decodeUtf8
textToLazyBytestring = Enc.encodeUtf8 . TLazy.fromStrict
