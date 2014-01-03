{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables #-}

module PG where
import Database.PostgreSQL.Simple hiding (connect)
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromRow
import Types
import Data.Word
import Data.Maybe
import Control.Applicative
import Control.Monad

instance Connectable Connection () () where
  connect config () = do putStrLn "Connecting to PG database."
                         conn <- PG.connect (toConnectInfo config)
                         return (conn, ())
  disconnect conn   = do putStrLn "Closing connection to PG database."
                         close conn

retrieveFiddle :: Connection -> Int -> Int -> IO (Maybe Fiddle)
retrieveFiddle conn fiddleId version = do
  let q = "SELECT hs, css, html FROM fiddles WHERE fiddle_id = ? AND fiddle_version = ?"
  results :: [Fiddle] <- query conn q (fiddleId, version)
  return (listToMaybe results)

instance FromRow Fiddle where
  fromRow = Fiddle <$> field <*> field <*> field

saveFiddle conn fiddleId hs css html = void . withTransaction conn $ do
  let q1 = "SELECT COALESCE(MAX(version), -1) FROM fiddles WHERE fiddle_id=?"
  (Only version : _) :: [Only Int] <- query conn q1 fiddleId
  let q2 = "INSERT INTO fiddles (fiddle_id, version, hs, css, html) VALUES (?, ?, ?, ?, ?)"
  execute conn q2 (fiddleId, version+1, hs, css, html)

toConnectInfo config = ConnectInfo{ connectHost     = pgHost config
                                  , connectPort     = read (pgPort config)
                                  , connectUser     = pgUser config
                                  , connectPassword = pgPass config
                                  , connectDatabase = pgDb config
                                  }
