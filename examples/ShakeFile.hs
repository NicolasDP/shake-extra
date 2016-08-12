module Main (main) where

import Development.Shake
import Development.Shake.Extra

project :: ProjectConfig AMD64 OSX CLANG
project = ProjectConfig
  { outputDir  = "_build"
  , buildDebug = True
  , getExtraFlags = ["-g", "-ggdb", "-W", "-Wall", "-Wextra"]
  , getArch = AMD64
  , getOS   = OSX
  , getCompiler = CLANG
  }

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
  want ["all"]
  libA <- makeSharedLib project "A" [] ["libA"] [File "libA/A.c"]
  libB <- makeSharedLib project "B" [] ["libB"] [File "libB/B.c"]
  exe <- makeExecutable project "app" [libA, libB] [] [File "app/main.c"]
  phony "all" $ need [dependencyName exe]
  phony "clean" $ do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]
