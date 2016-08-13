module Development.Shake.Extra.AOC
    ( AOC(..)
    ) where

-- | Architecture-OS-Compiler
--
-- not really Show, but a translation from the given type to its
-- entity within the `AOC`.
--
-- > amd64-osx-clang
--
-- > x86-linux-g++
--
class AOC a where
    aoc :: a -> String
