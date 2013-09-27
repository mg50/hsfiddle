module Compile (compile, CompileResult(..)) where
import qualified System.Random as Rand
import qualified System.Directory as D
import qualified System.Process as P
import qualified System.IO.Strict as Strict
import System.Exit (ExitCode(..))

data CompileResult = CompileSuccess String | CompileError String

compile :: String -> IO CompileResult
compile code = withTempDirectory $ \dir -> do
  let file = dir ++ "/code.js"
      addHeaders code = "{-# LANGUAGE JavaScriptFFI -#}\n\n" ++ code
  writeFile file (addHeaders code)
  (exitCode, out, err) <- ghcjs dir file
  if exitCode == ExitSuccess
     then readCompiledJS dir
     else return (CompileError err)

withTempDirectory :: (String -> IO a) -> IO a
withTempDirectory f = do dir <- randomTempDir
                         D.createDirectory dir
                         result <- f dir
                         D.removeDirectoryRecursive dir
                         return result

randomTempDir :: IO String
randomTempDir = do num <- Rand.randomIO :: IO Int
                   return $ "/tmp/ghcjs" ++ show num

ghcjs :: String -> String -> IO (ExitCode, String, String)
ghcjs dir file =
  P.readProcessWithExitCode "ghcjs" ["-o", dir ++ "/code", file] ""

readCompiledJS :: String -> IO CompileResult
readCompiledJS dir = do js <- Strict.readFile $ dir ++ "/code.jsexe/all.js"
                        return (CompileSuccess js)
