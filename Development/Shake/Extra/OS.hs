{-# LANGUAGE DeriveDataTypeable #-}

module Development.Shake.Extra.OS
    ( OS(..)
    , OSX(..)
    , Linux(..)
    , Windows(..)

    , currentOS
    , CurrentOS(..)
    ) where

import Data.Typeable
import Data.Data
import System.Info (os)

import Development.Shake.Extra.AOC (AOC(..))

class AOC os => OS os

data OSX = OSX deriving (Show, Typeable, Data)
instance AOC OSX where aoc _ = "osx"
instance OS OSX

data Linux = Linux deriving (Show, Typeable, Data)
instance AOC Linux where aoc _ = "linux"
instance OS Linux

data Windows = Windows deriving (Show, Typeable, Data)
instance AOC Windows where aoc = show
instance OS Windows

data CurrentOS
    = CurrentOSWindows
    | CurrentOSLinux
    | CurrentOSX
  deriving (Show, Eq, Ord, Enum, Bounded, Typeable, Data)

currentOS :: CurrentOS
currentOS = case os of
  "darwin"  -> CurrentOSX
  "mingw32" -> CurrentOSWindows
  "linux"   -> CurrentOSLinux
  str       -> error $ "Shake.Development.Extra.OS.currentOS: unknown OS " ++ str
