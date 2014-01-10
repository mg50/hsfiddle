{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Applicative
import Control.Concurrent

import System.Posix.Signals

import Web.Scotty

import qualified Network.AMQP as AMQP
import qualified Database.Redis as Redis
import qualified Database.PostgreSQL.Simple as PG

import Pending
import ParseCredentials
import Types
import App

main :: IO ()
main = do
  pending  <- newPending
  cred     <- readCredentials "./config/production.json"

  (amqpConn, amqpChan) :: (AMQP.Connection, AMQP.Channel) <- connect cred pending
  redisConn :: Redis.Connection <- connect cred ()
  pgConn    :: PG.Connection    <- connect cred ()

  --config   <- readConfig amqp redis postgres <$> Strict.readFile "./config.json"
  let env = ProdEnv { amqpConn' = amqpConn
                    , amqpChan' = amqpChan
                    , redis'    = redisConn
                    , postgres' = pgConn
                    }
      config = Config { service = env
                      , compileTimeout = Just 5000000
                      , port = 3000
                      }
  template <- readFile "./public/html/index.html"

  mainTid <- myThreadId
  let stop = gracefulExit mainTid config
  forM_ [sigINT, sigTERM, sigQUIT, sigHUP] $ \sig ->
    installHandler sig (Catch stop) Nothing

  putStrLn "Starting webserver."
  runApp template config pending

gracefulExit tid config = do
  disconnect (amqpConn config, amqpChan config)
  disconnect (redis config)
  disconnect (postgres config)
  killThread tid
