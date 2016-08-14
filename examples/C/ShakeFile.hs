{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Development.Shake
import Development.Shake.Extra

import System.Console.GetOpt

data CompilerOpt = CompilerGCC | CompilerCLANG deriving (Eq)

flags = [ Option "" ["gcc"] (NoArg $ Right CompilerGCC) "use GCC compiler."
        , Option "" ["clang"] (NoArg $ Right CompilerCLANG) "use CLANG compiler."
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
    then shakeIt targets $ project GCC
    else shakeIt targets $ project CLANG

shakeIt targets projCfg = do
  pthread <- findLib projCfg ["**pthread.*"] []
  libA <- makeStaticLib projCfg "A" [] ["libA"] ["libA/A.c"]
  libB <- makeSharedLib projCfg "B" [] ["libB"] ["libB/B.c"]
  exe <- makeExecutable projCfg "app" [pthread, libA, libB] [] ["app/main.c"]
  phony "all" $ need [dependencyName exe]
  phony "test" $ do
    need [dependencyName exe]
    cmd (dependencyName exe)
  phony "clean" $ do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]
