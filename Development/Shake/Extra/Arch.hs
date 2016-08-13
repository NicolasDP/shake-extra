{-# LANGUAGE DeriveDataTypeable #-}

module Development.Shake.Extra.Arch
    ( Arch(..)

    , AMD64(..)
    , X86(..)
    ) where

import Data.Typeable
import Data.Data
import System.Info (arch)

import Development.Shake.Extra.AOC (AOC(..))

class AOC arch => Arch arch

data AMD64 = AMD64 | X86_64
  deriving (Show, Eq, Ord, Enum, Bounded, Typeable, Data)
instance AOC AMD64 where aoc _ = "amd64"
instance Arch AMD64

data X86 = X86 deriving (Show, Typeable, Data)
instance AOC X86 where aoc _ = "x86"
instance Arch X86
