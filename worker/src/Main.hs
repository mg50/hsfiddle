{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where
import Network.AMQP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.Lazy.Encoding as EncL
import qualified Data.Text.Encoding as Enc
import Control.Monad
import Control.Concurrent
import System.Posix.Signals
import Util
import Config
import Compile
import Semaphore
import Database.Redis

main = do
  putStrLn "Starting worker..."
  Config{..} <- getConfig "./config.json"
  amqpConn <- openConnection amqpServer "/" amqpLogin amqpPass
  chan <- openChannel amqpConn
  putStrLn "Connected to AMQP server"

  redis <- undefined

  sem <- newSemaphore maxCompilations
  tag <- consumeMsgs chan "uncompiled" Ack (tryCompile redis chan sem)

  done <- newEmptyMVar
  let stop = gracefulExit redis chan amqpConn tag sem done
  forM_ [sigINT, sigTERM, sigQUIT, sigHUP] $ \sig ->
    installHandler sig (Catch stop) Nothing
  takeMVar done

tryCompile redis chan sem (requestMsg, envelope) = do
  ackEnv envelope
  acquire sem
  case msgID requestMsg of
    Nothing -> return ()
    Just requestId -> do
      putStrLn "About to compile"
      let (md5hash, code) = splitBL 32 (msgBody requestMsg)
      (result, dt) <- withDiffTime $ compile (Enc.decodeUtf8 code)
      putStrLn $ "Finished compiling in " ++ show dt

      (txt, queue) <- case result of
        CompileSuccess js -> do (_, dt) <- withDiffTime $ insertRedis redis md5hash js
                                putStrLn $ "Code sent to Redis in " ++ show dt
                                return ("", "compiled")
        CompileError err  -> return (err, "error")

      let replyMsg = newMsg{ msgBody = EncL.encodeUtf8 (TLazy.fromStrict txt)
                           , msgDeliveryMode = Just Persistent
                           , msgReplyTo = Just requestId }

      publishMsg chan "hsfiddle" queue replyMsg
  release sem

insertRedis redis hash js = runRedis redis $ set hash (Enc.encodeUtf8 js)

gracefulExit redis chan conn tag sem done = do
  putStrLn "Waiting for compilations to finish..."
  runRedis redis quit
  cancelConsumer chan tag
  awaitDrain sem
  closeConnection conn
  putMVar done ()
  putStrLn "Exiting..."

splitBL :: Int -> BL.ByteString -> (BS.ByteString, BS.ByteString)
splitBL offset bl = let text = TLazy.toStrict $ EncL.decodeUtf8 bl
                        (prefix, suffix) = T.splitAt offset text
                    in (Enc.encodeUtf8 prefix, Enc.encodeUtf8 suffix)
