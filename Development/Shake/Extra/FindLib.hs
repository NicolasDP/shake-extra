{-# LANGUAGE MultiParamTypeClasses #-}

module Development.Shake.Extra.FindLib
  ( Hint
  , findLib
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

findLib :: (OS os, Arch arch, CompilerCommon compiler)
        => ProjectConfig arch os compiler
        -> [FilePattern] -- Lib Name (<lib>name<.(a|so|dynlib|dll)>)
        -> [Hint]
        -> Rules (Dependency arch os compiler)
findLib pc libpattern userHints = do
  let hints = userHints ++ ["/usr/lib", "/usr/local/lib"]
  rs <- concat <$> (liftIO $ mapM (\h -> map (h </>) <$> getDirectoryFilesIO h libpattern) hints)
  when (null rs) $
    error $ "error, cannot find " ++ show libpattern ++ " in " ++ show hints
  return Dependency
    { dependencyName         = head rs
    , dependencyIncludeDirs  = []
    , dependencyDependencies = []
    , dependencyResults      = map (ResultDepLib . File) rs
    }

--
-- class SearchableDependency dep => GetDependency dep
