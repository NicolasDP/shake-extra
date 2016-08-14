module Development.Shake.Extra.ProjectConfig
    ( ProjectConfig(..)
    , projectConfigOutputResultDir
    , projectConfigOutputResultDir_
    ) where

import Data.List (find)
import Data.Proxy
import Development.Shake as Shake
import Development.Shake.FilePath as Shake

import Development.Shake.Extra.AOC
import Development.Shake.Extra.OS (OS)
import Development.Shake.Extra.Arch (Arch)
import Development.Shake.Extra.Compiler

data ProjectConfig a o c = ProjectConfig
    { outputDir   :: FilePath
    , buildDebug  :: Bool
    , getExtraFlags :: [String]
    , getArch     :: a
    , getOS       :: o
    , getCompiler :: c
    }

projectConfigDebugOrRelease :: (CompilerCommon compiler, Arch arch, OS os)
                            => ProjectConfig arch os compiler
                            -> FilePath
projectConfigDebugOrRelease pc = if buildDebug pc then "debug" else "release"


projectConfigOutputDir :: (CompilerCommon compiler, Arch arch, OS os)
                       => ProjectConfig arch os compiler
                       -> ModuleName
                       -> FilePath
projectConfigOutputDir pc (ModuleName mn) =
    outputDir pc </> mn </> (aoc (getArch pc) ++ "-" ++ aoc (getOS pc) ++ "-" ++ aoc (getCompiler pc))

projectConfigOutputResultDir :: (CompilerCommon compiler, Arch arch, OS os, BuildType buildtype)
                             => ProjectConfig arch os compiler
                             -> ModuleName
                             -> File buildtype
projectConfigOutputResultDir = projectConfigOutputResultDir_ Proxy
projectConfigOutputResultDir_ :: (CompilerCommon compiler, Arch arch, OS os, BuildType buildtype)
                              => Proxy buildtype
                              -> ProjectConfig arch os compiler
                              -> ModuleName
                              -> File buildtype
projectConfigOutputResultDir_ p pc mn = File $
  projectConfigOutputDir pc mn </> fromBT p </> projectConfigDebugOrRelease pc
