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
    ) where

import Data.Typeable
import Data.Proxy
import Data.Data
import System.Info (arch)
import Data.String

import Development.Shake (CmdResult(..), Action, cmd)
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
    toLibraryFlags :: compiler -> [FilePath] -> [String]
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
    toLibraryFlags _ = map ("-l" ++)
instance Compiler AR [File Object] (File StaticLib) where
    genOutputExt _ _ _ = "a"
    compile _ is (File o) e = cmd "ar" "-rcs" o $ map (\(File i) -> i) is

data CLANG = CLANG deriving (Show, Typeable, Data)
instance AOC CLANG where aoc _ = "clang"
instance CompilerCommon CLANG where
    toIncludeFlags _ = map (\(IncludeDir i) -> "-I" ++ i)
    toLibraryFlags _ = map ("-L" ++)
instance Compiler CLANG (File Source) (File Object) where
    genOutputExt _ _ _ = "o"
    compile _ (File i) (File o) e = cmd "clang" "-c" e "-o" o i
instance Compiler CLANG [File Object] (File SharedLib) where
    genOutputExt _ _ _ = ".dynlib"
    compile _ is (File o) e = cmd "clang" "-shared" e "-o" o $ map (\(File f) -> f) is
instance Compiler CLANG [File Object] (File StaticLib) where
    genOutputExt _ = genOutputExt AR
    compile _ = compile AR
instance Compiler CLANG [File Object] (File Executable) where
    genOutputExt _ _ _ = ""
    compile _ is (File o) e = cmd "clang" e "-o" o $ map (\(File f) -> f) is
