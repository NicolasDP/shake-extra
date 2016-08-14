{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Development.Shake.Extra.Compiler
    ( CompilerCommon(..)
    , Compiler(..)

    , ModuleName(..)
    , IncludeDir(..)
    , File(..)
    , Source(..)

    , BuildType(..)
    , Object(..)
    , SharedLib(..)
    , StaticLib(..)
    , Executable(..)

    , CLANG(..)
    , GCC(..)
    , AR(..)

    , CxxSTDVersion(..)
    ) where

import Data.Typeable
import Data.Proxy
import Data.Data
import System.Info (arch)
import Data.String

import Development.Shake (CmdResult(..), Action, cmd)
import Development.Shake.FilePath ((-<.>))
import Development.Shake.Util (needMakefileDependencies)
import Development.Shake.Extra.AOC (AOC(..))

newtype ModuleName = ModuleName String
  deriving (Show, Eq, Ord, IsString, Typeable, Data)
newtype IncludeDir = IncludeDir FilePath
  deriving (Show, Eq, Ord, IsString, Typeable, Data)
newtype File ty    = File FilePath
  deriving (Show, Eq, Ord, IsString, Typeable, Data)

class BuildType bt where fromBT :: Proxy bt -> FilePath

data Source     = Source     deriving (Show, Typeable, Data)
data Object     = Object     deriving (Show, Typeable, Data)
data SharedLib  = SharedLib  deriving (Show, Typeable, Data)
data StaticLib  = StaticLib  deriving (Show, Typeable, Data)
data Executable = Executable deriving (Show, Typeable, Data)

instance BuildType Object     where fromBT _ = "objects"
instance BuildType SharedLib  where fromBT _ = "shared"
instance BuildType StaticLib  where fromBT _ = "static"
instance BuildType Executable where fromBT _ = "bin"

class (AOC compiler, Show compiler) => CompilerCommon compiler where
    toIncludeFlags :: compiler -> [IncludeDir] -> [String]
    toCmd :: compiler -> [String]
class CompilerCommon compiler => Compiler compiler input output where
    genOutputExt :: compiler -> Proxy input -> Proxy output -> String
    compile :: CmdResult r
            => compiler
            -> input
            -> output
            -> [String] -- extras
            -> Action r

data AR = AR deriving (Show, Typeable, Data)
instance AOC AR where aoc = show
instance CompilerCommon AR where
    toIncludeFlags _ = map (\(IncludeDir i) -> "-I" ++ i)
    toCmd _ = ["ar"]
instance Compiler AR [File Object] (File StaticLib) where
    genOutputExt _ _ _ = "a"
    compile c is (File o) e = cmd (toCmd c) "-rcs" o $ map (\(File i) -> i) is

data CxxSTDVersion = CxxStd | CxxStd11 | CxxStd14
  deriving (Show, Eq, Ord, Enum, Bounded, Typeable, Data)

data CLANG
    = CLANG | CLANGXX CxxSTDVersion
  deriving (Show, Eq, Typeable, Data)

instance AOC CLANG where aoc _ = "clang"
instance CompilerCommon CLANG where
    toIncludeFlags _ = map (\(IncludeDir i) -> "-I" ++ i)
    toCmd CLANG = ["clang"]
    toCmd (CLANGXX CxxStd)   = ["clang++"]
    toCmd (CLANGXX CxxStd11) = ["clang++", "-std=c++11"]
    toCmd (CLANGXX CxxStd14) = ["clang++", "-std=c++14"]
instance Compiler CLANG (File Source) (File Object) where
    genOutputExt _ _ _ = "o"
    compile c (File i) (File o) e = do
      let m = o -<.> "m"
      r <- cmd (toCmd c) "-fPIC" "-c" e "-MMD -MF" [m] "-o" o i
      needMakefileDependencies m
      return r
instance Compiler CLANG [File Object] (File SharedLib) where
    genOutputExt _ _ _ = ".so"
    compile c is (File o) e = cmd (toCmd c) "-shared" e "-o" o $ map (\(File f) -> f) is
instance Compiler CLANG [File Object] (File StaticLib) where
    genOutputExt _ = genOutputExt AR
    compile _ = compile AR
instance Compiler CLANG [File Object] (File Executable) where
    genOutputExt _ _ _ = ""
    compile c is (File o) = cmd (toCmd c) "-o" o (map (\(File f) -> f) is)

data GCC = GCC | GXX CxxSTDVersion deriving (Show, Typeable, Data)
instance AOC GCC where aoc _ = "gcc"
instance CompilerCommon GCC where
    toIncludeFlags _ = toIncludeFlags CLANG
    toCmd GCC = ["gcc"]
    toCmd (GXX CxxStd)   = ["g++"]
    toCmd (GXX CxxStd11) = ["g++", "-std=c++11"]
    toCmd (GXX CxxStd14) = ["g++", "-std=c++14"]
instance Compiler GCC (File Source) (File Object) where
    genOutputExt _ = genOutputExt CLANG
    compile c (File i) (File o) e = do
      let m = o -<.> "m"
      r <- cmd (toCmd c) "-fPIC" "-c" "-o" o e "-MMD -MF" [m] i
      needMakefileDependencies m
      return r
instance Compiler GCC [File Object] (File SharedLib) where
    genOutputExt _ _ _ = ".so"
    compile c is (File o) e = cmd (toCmd c) "-shared" "-o" o e $ map (\(File f) -> f) is
instance Compiler GCC [File Object] (File StaticLib) where
    genOutputExt _ = genOutputExt AR
    compile _ = compile AR
instance Compiler GCC [File Object] (File Executable) where
    genOutputExt _ _ _ = ""
    compile c is (File o) = cmd (toCmd c) "-o" o (map (\(File f) -> f) is)
