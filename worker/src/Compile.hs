{-# LANGUAGE OverloadedStrings #-}

module Compile (compile, CompileResult(..)) where
import qualified System.Random as Rand
import qualified System.Directory as D
import qualified System.Process as P
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode(..))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

data CompileResult = CompileSuccess BL.ByteString
                   | CompileError BL.ByteString

compile :: T.Text -> IO CompileResult
compile code = withTempDirectory $ \dir -> do
  let file = dir ++ "/code.hs"
  TIO.writeFile file code
  (exitCode, _, err) <- ghcjs dir
  if exitCode == ExitSuccess
     then readCompiledJS dir
     else return $ CompileError (BL.pack err)

withTempDirectory :: (String -> IO a) -> IO a
withTempDirectory f = do dir <- randomTempDir
                         D.createDirectory dir
                         result <- f dir
                         D.removeDirectoryRecursive dir
                         return result

randomTempDir :: IO String
randomTempDir = do num <- Rand.randomIO :: IO Int
                   return $ "/home/vagrant/ghcjs" ++ show num

ghcjs :: String -> IO (ExitCode, String, String)
ghcjs dir =
  P.readProcessWithExitCode "./bin/compile" [dir] ""

readCompiledJS :: String -> IO CompileResult
readCompiledJS dir = do [lib, lib1, out] <- forM ["lib", "lib1", "out"] $ \file -> do
                          TIO.readFile $ dir ++ "/code.jsexe/" ++ file ++ ".js"
                        return . CompileSuccess $ jsonify lib lib1 out

jsonify :: T.Text -> T.Text -> T.Text -> BL.ByteString
jsonify lib lib1 out = encode o
  where o = object ["lib" .= lib, "lib1" .= lib1, "out" .= out]
