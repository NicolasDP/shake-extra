module Development.Shake.Extra.Dependency
  ( Dependency(..)
  , Dependencies
  , Result(..)
  , includeDependencies
  , addDependenciesLib
  ) where

import Development.Shake.Extra.Compiler

data Result arch os compiler
    = ResultSharedLib (File SharedLib)
    | ResultStaticLib (File StaticLib)
    | ResultExecutable (File Executable)
  deriving (Show, Eq)

type Dependencies arch os compiler = [Dependency arch os compiler]
data Dependency arch os compiler = Dependency
    { dependencyName         :: String
    , dependencyIncludeDirs  :: [IncludeDir]
    , dependencyDependencies :: [String]
    , dependencyResults      :: [Result arch os compiler]
    }
  deriving (Show, Eq)
instance Ord (Dependency os arch compiler) where
    compare d1 d2 = compare (dependencyName d1) (dependencyName d2)

includeDependencies :: Dependencies arch os compiler -> [IncludeDir]
includeDependencies = concatMap dependencyIncludeDirs

addDependenciesLib :: Dependencies arch os compiler -> [FilePath]
addDependenciesLib = concatMap addDependencyLib
  where
    addDependencyLib :: Dependency arch os compiler -> [FilePath]
    addDependencyLib = extractLib . dependencyResults
    extractLib :: [Result arch os compiler] -> [FilePath]
    extractLib []                            = []
    extractLib (ResultStaticLib (File f):xs) = f : extractLib xs
    extractLib (ResultSharedLib (File f):xs) = f : extractLib xs
    extractLib (_                       :xs) =     extractLib xs
