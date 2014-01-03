{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables #-}

module PG (retrieveFiddle, saveFiddle) where
import Database.PostgreSQL.Simple hiding (connect)
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromRow
import qualified Data.Text as T
import Types
import Data.Maybe
import Control.Applicative
import Control.Monad

instance Connectable Connection () () where
  connect config () = do putStrLn "Connecting to PG database."
                         conn <- PG.connect (toConnectInfo config)
                         return (conn, ())
  disconnect conn   = do putStrLn "Closing connection to PG database."
                         close conn

retrieveFiddle :: Connection -> T.Text -> Int -> IO (Maybe Fiddle)
retrieveFiddle conn slug version = do
  let q = "SELECT hs, css, html FROM fiddles WHERE slug = ? AND version = ?"
  results :: [Fiddle] <- query conn q (T.unpack slug, version)
  return (listToMaybe results)

instance FromRow Fiddle where
  fromRow = Fiddle <$> field <*> field <*> field

saveFiddle :: Maybe T.Text -> Connection -> T.Text -> T.Text -> T.Text -> IO ()
saveFiddle Nothing     = saveNewFiddle
saveFiddle (Just slug) = updateFiddle slug

saveNewFiddle :: Connection -> T.Text -> T.Text -> T.Text -> IO ()
saveNewFiddle conn hs css html = void . withTransaction conn $ do
  let q = "INSERT INTO fiddles (version, hs, css, html) VALUES (?, ?, ?, ?)"
  execute conn q (0 :: Int, T.unpack hs, T.unpack css, T.unpack html)

updateFiddle :: T.Text -> Connection -> T.Text -> T.Text -> T.Text -> IO ()
updateFiddle slug conn hs css html = void . withTransaction conn $ do
  let versionQuery = "SELECT COALESCE(MAX(version), -1) FROM fiddles WHERE fiddle_id=?"
  (Only version : _) :: [Only Int] <- query conn versionQuery (Only slug)
  let q = "INSERT INTO fiddles (slug, version, hs, css, html) VALUES (?, ?, ?, ?, ?)"
  execute conn q (T.unpack slug, version+1, T.unpack hs, T.unpack css, T.unpack html)

toConnectInfo config = defaultConnectInfo{ connectHost     = pgHost config
                                         , connectUser     = pgUser config
                                         , connectPassword = pgPass config
                                         , connectDatabase = pgDb config
                                         }
