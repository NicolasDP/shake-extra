{-# LANGUAGE MultiParamTypeClasses #-}

module Development.Shake.Extra.FindLib
  ( Hint
  , FindLibrary(..)
  , findDependency
  ) where

import Data.Data
import Data.Typeable
import Data.Proxy
import Control.Monad (when)

import Development.Shake (Rules, FilePattern, getDirectoryFilesIO, liftIO)
import Development.Shake.FilePath
import Development.Shake.Extra.Dependency
import Development.Shake.Extra.Arch (Arch(..))
import Development.Shake.Extra.OS   (OS(..))
import Development.Shake.Extra.Compiler
import Development.Shake.Extra.ProjectConfig

type Hint = FilePath

class FindLibrary arch os compiler lib where
  findLibrary :: ProjectConfig arch os compiler -> lib -> [Hint] -> Rules (Dependency arch os compiler)

findDependency :: (OS os, Arch arch, CompilerCommon compiler)
               => ProjectConfig arch os compiler
               -> [FilePattern] -- Lib Name (<lib>name<.(a|so|dynlib|dll)>)
               -> [Hint]
               -> (FilePath -> Result arch os compiler)
               -> Rules (Dependency arch os compiler)
findDependency pc libpattern hints f = do
  rs <- concat <$> (liftIO $ mapM (\h -> map (h </>) <$> getDirectoryFilesIO h libpattern) hints)
  when (null rs) $
    error $ "error, cannot find " ++ show libpattern ++ " in " ++ show hints
  return Dependency
    { dependencyName         = head rs
    , dependencyIncludeDirs  = []
    , dependencyDependencies = []
    , dependencyResults      = map f rs
    }

--
-- class SearchableDependency dep => GetDependency dep
