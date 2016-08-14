{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Development.Shake
import Development.Shake.Extra

import System.Console.GetOpt

data CompilerOpt = CompilerGCC | CompilerCLANG deriving (Eq)

flags = [ Option "" ["gcc"] (NoArg $ Right CompilerGCC) "use GCC compiler."
        , Option "" ["clang"] (NoArg $ Right CompilerCLANG) "use CLANG compiler."
        ]

project c os = ProjectConfig
  { outputDir  = "_build"
  , buildDebug = True
  , getExtraFlags = ["-g", "-ggdb", "-W", "-Wall", "-Wextra"]
  , getArch = AMD64
  , getOS   = os
  , getCompiler = c
  }

main :: IO ()
main = shakeArgsWith shakeOptions{shakeFiles="_build"} flags $ \flags targets -> return $ Just $ do
  if null targets then want ["all"] else want targets
  case currentOS of
    CurrentOSLinux ->
      if CompilerGCC `elem` flags
          then shakeIt targets $ project GCC Linux
          else shakeIt targets $ project CLANG Linux
    CurrentOSX ->
      if CompilerGCC `elem` flags
          then error "only CLang on OSX for this project"
          else shakeIt targets $ project CLANG OSX
    _ -> error "Windows is not supported yet"

class FindLibrary arch os compiler lib where
  findLibrary :: ProjectConfig arch os compiler -> lib -> [Hint] -> Rules (Dependency arch os compiler)

data PThread = PThread deriving (Show)
instance (Arch arch,CompilerCommon compiler) => FindLibrary arch Linux compiler PThread where
  findLibrary p _ = findLib p ["libpthread.so", "*/libpthread.so"]
instance Arch arch => FindLibrary arch OSX   CLANG    PThread where
  findLibrary _ p _ = do
    let emptyRule = "empty rule " ++ show p
    phony emptyRule $ return ()
    return $ Dependency emptyRule [] [] []

shakeIt targets projCfg = do
  pthread <- findLibrary projCfg PThread []
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
