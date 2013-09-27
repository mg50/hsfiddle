{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty
import Control.Monad
import Control.Monad.Trans
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T
import qualified System.Directory as D
import qualified System.IO.Strict as Strict
import qualified System.Random as Rand
import System.Process

main = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "./public")

  get "/" $ do
    file <- liftIO $ readFile "./public/html/index.html"
    html (T.pack file)

  post "/compile" $ do
    code <- param "code"
    liftIO $ print "asdf"
    liftIO $ print (T.unpack code)

-- compile :: String -> IO (Either String String)
-- compile code = withSystemTempDirectory "ghcjs" $ \dir -> do
--   let file = dir ++ "/code.hs"
--   writeFile file code
--   (exitCode, out, err) <- ghc file
--   getDirectoryContents dir >>= print
--   if exitCode == ExitSuccess
--      then liftM Right $ Strict.readFile $ dir ++ "/code.o"
--      else return $ Left err

compile :: String -> IO ()
compile code = do
  num' <- Rand.randomIO :: IO Int
  let num = show num
      dir = "/tmp/" ++ "ghcjs" ++ num
      file = dir ++ "/code.js"
  D.createDirectory dir
  writeFile file code
  ghcjs dir file
  return ()


ghcjs dir file =
  readProcessWithExitCode "ghcjs" ["-o", dir ++ "code", file] ""
