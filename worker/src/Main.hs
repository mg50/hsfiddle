{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where
import Network.AMQP
import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.Lazy.Encoding as Enc
import Control.Monad
import Control.Concurrent
import System.Posix.Signals
import Util
import Config
import Compile
import Semaphore

main :: IO ()
main = do
  putStrLn "Starting worker..."
  Config{..} <- getConfig "./config.json"
  conn <- openConnection amqpServer "/" amqpLogin amqpPass
  chan <- openChannel conn
  putStrLn "Connected to AMQP server"

  sem <- newSemaphore maxCompilations
  tag <- consumeMsgs chan "uncompiled" Ack (tryCompile chan sem)

  hardExit <- newMVar False
  done <- newEmptyMVar

  let stop = signalHandler chan conn tag sem done hardExit
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
                           CompileSuccess json -> (json, "compiled")
                           CompileError err    -> (err, "error")
          replyMsg = newMsg{ msgBody = txt
                           , msgDeliveryMode = Just Persistent
                           , msgReplyTo = Just requestId }

      putStrLn "About to send back code"
      (_, dt) <- withDiffTime $ publishMsg chan "hsfiddle" queue replyMsg
      putStrLn $ "Code sent in " ++ show dt
  release sem

signalHandler chan conn tag sem done hardExit =
  modifyMVar_ hardExit $ \b ->
    if not b
       then do putStrLn "Waiting for compilations to finish before exiting"
               gracefulExit chan conn tag sem done
               return True
       else do putStrLn "Forcing exit"
               putMVar done ()
               return True

gracefulExit chan conn tag sem done = do
  cancelConsumer chan tag
  awaitDrain sem
  closeConnection conn
  putMVar done ()
  putStrLn "Exiting..."

lazyBytestringToText = TLazy.toStrict . Enc.decodeUtf8
textToLazyBytestring = Enc.encodeUtf8 . TLazy.fromStrict
