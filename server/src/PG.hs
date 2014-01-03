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

retrieveFiddle :: Connection -> String -> Int -> IO (Maybe Fiddle)
retrieveFiddle conn slug version = do
  let q = "SELECT hs, css, html FROM fiddles WHERE slug = ? AND fiddle_version = ?"
  results :: [Fiddle] <- query conn q (slug, version)
  return (listToMaybe results)

instance FromRow Fiddle where
  fromRow = Fiddle <$> field <*> field <*> field

saveFiddle :: Maybe T.Text -> Connection -> T.Text -> T.Text -> T.Text -> IO ()
saveFiddle Nothing     = saveNewFiddle
saveFiddle (Just slug) = updateFiddle slug

saveNewFiddle conn hs css html = do
  let q = "INSERT INTO fiddles (version, hs, css, html) VALUES (?, ?, ?, ?)"
  execute conn q (0, hs, css, html)

updateFiddle slug conn hs css html = void . withTransaction conn $ do
  let versionQuery = "SELECT COALESCE(MAX(version), -1) FROM fiddles WHERE fiddle_id=?"
  (Only v : _) :: [Only Int] <- query conn versionQuery slug
  let q = "INSERT INTO fiddles (slug, version, hs, css, html) VALUES (?, ?, ?, ?, ?)"
  execute conn q (slug, version+1, hs, css, html)

toConnectInfo config = ConnectInfo{ connectHost     = pgHost config
                                  , connectPort     = read (pgPort config)
                                  , connectUser     = pgUser config
                                  , connectPassword = pgPass config
                                  , connectDatabase = pgDb config
                                  }
