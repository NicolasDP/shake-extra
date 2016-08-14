{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Development.Shake
import Development.Shake.Extra

import System.Console.GetOpt

data CompilerOpt = CompilerGCC | CompilerCLANG deriving (Eq)

flags = [ Option "" ["gcc"] (NoArg $ Right CompilerGCC) "use G++ compiler."
        , Option "" ["clang"] (NoArg $ Right CompilerCLANG) "use CLANG++ compiler."
        ]

project c = ProjectConfig
  { outputDir  = "_build"
  , buildDebug = True
  , getExtraFlags = ["-g", "-ggdb", "-W", "-Wall", "-Wextra"]
  , getArch = AMD64
  , getOS   = OSX
  , getCompiler = c
  }

main :: IO ()
main = shakeArgsWith shakeOptions{shakeFiles="_build"} flags $ \flags targets -> return $ Just $ do
  if null targets then want ["all"] else want targets
  if CompilerGCC `elem` flags
    then shakeIt targets $ project (GXX     CxxStd11)
    else shakeIt targets $ project (CLANGXX CxxStd11)

shakeIt targets projCfg = do
  libA <- makeStaticLib projCfg "A" [] ["libA"] ["libA/A.cc"]
  libB <- makeSharedLib projCfg "B" [] ["libB"] ["libB/B.cc"]
  exe <- makeExecutable projCfg "app" [libA, libB] [] ["app/main.cc"]
  phony "all" $ need [dependencyName exe]
  phony "test" $ do
    need [dependencyName exe]
    cmd (dependencyName exe)
  phony "clean" $ do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]
