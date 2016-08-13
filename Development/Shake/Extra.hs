{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Development.Shake.Extra
    (
      ProjectConfig(..)
    , makeSharedLib
    , makeExecutable
    , makeStaticLib
    , Dependency(..)

    , module X
    ) where

import Data.List (find)
import Data.Proxy
import Development.Shake as Shake
import Development.Shake.FilePath as Shake

import Development.Shake.Extra.AOC  (AOC(..))
import Development.Shake.Extra.OS       as X
import Development.Shake.Extra.Arch     as X
import Development.Shake.Extra.Compiler as X

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

makeObject :: (Compiler compiler (File Source) (File Object), OS os, Arch arch)
           => ProjectConfig arch os compiler
           -> ModuleName
           -> [IncludeDir]
           -> File Source
           -> Rules (File Object)
makeObject pc mn includes sf@(File input) = do
    let compiler = getCompiler pc
    let ext      = genOutputExt compiler (Proxy :: Proxy (File Source)) (Proxy :: Proxy (File Object))
    let (File outdir) = projectConfigOutputResultDir pc mn :: File Object
    let out@(File fp) = File $ outdir </> input -<.> ext :: File Object
    fp %> \_ -> compile compiler sf out (toIncludeFlags compiler includes ++ getExtraFlags pc)
    return out

makeObjects :: (Compiler compiler (File Source) (File Object), OS os, Arch arch)
            => ProjectConfig arch os compiler
            -> ModuleName
            -> [IncludeDir]
            -> [File Source]
            -> Rules [File Object]
makeObjects p m i = mapM (makeObject p m i)

makeOutput :: ( Compiler compiler (File Source) (File Object)
              , Compiler compiler [File Object] (File a)
              , OS os, Arch arch, BuildType a
              )
           => Proxy (File a)
           -> (File a -> Result arch os compiler)
           -> ProjectConfig arch os compiler
           -> ModuleName
           -> Dependencies arch os compiler
           -> [IncludeDir]
           -> [File Source]
           -> Rules (Dependency arch os compiler)
makeOutput proxy makeResult pc m@(ModuleName mn) deps includes sources = do
    let compiler = getCompiler pc
    let ext      = genOutputExt compiler (Proxy :: Proxy [File Object]) proxy
    objs <- makeObjects pc m (includes ++ includeDependencies deps) sources
    let (File outdir) = projectConfigOutputResultDir_ (getProxy proxy) pc m
    let out@(File fp) = File $ outdir </> mn -<.> ext
    fp %> \_ -> do
        need $ map dependencyName deps
        need $ map (\(File f) -> f) objs
        compile compiler objs out (getExtraFlags pc ++ addDependenciesLib deps)
    return $ Dependency fp includes (map dependencyName deps) [makeResult out]
  where
    getProxy :: Proxy (File a) -> Proxy a
    getProxy _ = Proxy

makeSharedLib :: ( Compiler compiler (File Source) (File Object)
                 , Compiler compiler [File Object] (File SharedLib)
                 , OS os, Arch arch
                 )
              => ProjectConfig arch os compiler
              -> ModuleName
              -> Dependencies arch os compiler
              -> [IncludeDir]
              -> [File Source]
              -> Rules (Dependency arch os compiler)
makeSharedLib = makeOutput Proxy ResultSharedLib

makeStaticLib :: ( Compiler compiler (File Source) (File Object)
                 , Compiler compiler [File Object] (File StaticLib)
                 , OS os, Arch arch
                 )
              => ProjectConfig arch os compiler
              -> ModuleName
              -> Dependencies arch os compiler
              -> [IncludeDir]
              -> [File Source]
              -> Rules (Dependency arch os compiler)
makeStaticLib = makeOutput Proxy ResultStaticLib

makeExecutable :: ( Compiler compiler (File Source) (File Object)
                  , Compiler compiler [File Object] (File Executable)
                  , OS os, Arch arch
                  )
               => ProjectConfig arch os compiler
               -> ModuleName
               -> Dependencies arch os compiler
               -> [IncludeDir]
               -> [File Source]
               -> Rules (Dependency arch os compiler)
makeExecutable = makeOutput Proxy ResultExecutable

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
    addDependencyLib = map extractLib . dependencyResults
    extractLib :: Result arch os compiler -> FilePath
    extractLib (ResultStaticLib (File f)) = f
    extractLib (ResultSharedLib (File f)) = f
    extractLib (ResultExecutable (File f)) = f
