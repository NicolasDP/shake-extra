{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Development.Shake.Extra
    (
      module Shake
    , AMD64(..), OSX(..), CLANG(..)
    , ProjectConfig(..)
    , makeSharedLib
    , makeExecutable
    , File(..)
    , Dependency(..)
    ) where

import Data.List (find)
import Data.Proxy
import Development.Shake as Shake
import Development.Shake.FilePath as Shake

type ModuleName    = String
newtype File ty    = File FilePath
  deriving (Show, Eq)
type IncludeDir = FilePath
data Source = Source
data Object    = Object
data SharedLib = SharedLib
data StaticLib = StaticLib
data Executable = Executable

data AMD64 = AMD64
data OSX = OSX
data CLANG = CLANG
  deriving (Show)

class ToAOC a where
    aoc :: a -> String

instance ToAOC AMD64 where aoc _ = "amd64"
instance ToAOC OSX where aoc _ = "osx"
instance ToAOC CLANG where aoc _ = "clang"

class ToAOC os   => OS   os

instance OS OSX

class ToAOC arch => Arch arch

instance Arch AMD64

data ProjectConfig a o c = ProjectConfig
    { outputDir   :: FilePath
    , buildDebug  :: Bool
    , getExtraFlags :: [String]
    , getArch     :: a
    , getOS       :: o
    , getCompiler :: c
    }

projectConfigDebugOrRelease :: (CompilerConfig compiler, Arch arch, OS os)
                            => ProjectConfig arch os compiler
                            -> FilePath
projectConfigDebugOrRelease pc = if buildDebug pc then "debug" else "release"


projectConfigOutputDir :: (CompilerConfig compiler, Arch arch, OS os)
                       => ProjectConfig arch os compiler
                       -> ModuleName
                       -> FilePath
projectConfigOutputDir pc mn =
    outputDir pc </> mn </> (aoc (getArch pc) ++ "-" ++ aoc (getOS pc) ++ "-" ++ aoc (getCompiler pc))

projectConfigOutputObjDir :: (CompilerConfig compiler, Arch arch, OS os)
                          => ProjectConfig arch os compiler
                          -> ModuleName
                          -> FilePath
projectConfigOutputObjDir pc mn =
    projectConfigOutputDir pc mn </> "objects" </> projectConfigDebugOrRelease pc

projectConfigOutputStaticDir :: (CompilerConfig compiler, Arch arch, OS os)
                             => ProjectConfig arch os compiler
                             -> ModuleName
                             -> FilePath
projectConfigOutputStaticDir pc mn =
    projectConfigOutputDir pc mn </> "static" </> projectConfigDebugOrRelease pc

projectConfigOutputSharedDir :: (CompilerConfig compiler, Arch arch, OS os)
                             => ProjectConfig arch os compiler
                             -> ModuleName
                             -> FilePath
projectConfigOutputSharedDir pc mn =
    projectConfigOutputDir pc mn </> "shared" </> projectConfigDebugOrRelease pc

projectConfigOutputBinDir :: (CompilerConfig compiler, Arch arch, OS os)
                          => ProjectConfig arch os compiler
                          -> ModuleName
                          -> FilePath
projectConfigOutputBinDir pc mn =
    projectConfigOutputDir pc mn </> "bin" </> projectConfigDebugOrRelease pc


class (ToAOC compiler, Show compiler) => CompilerConfig compiler where
    toIncludeFlags :: compiler -> [FilePath] -> [String]
    toLibraryFlags :: compiler -> [FilePath] -> [String]

instance CompilerConfig CLANG where
    toIncludeFlags _ = map ("-I" ++)
    toLibraryFlags _ = map ("-L" ++)

instance Compiler CLANG (File Source) (File Object) where
    genOutputExt _ _ _ = "o"
    compile _ (File i) (File o) e = cmd "clang" "-c" e "-o" o i
instance Compiler CLANG [File Object] (File SharedLib) where
    genOutputExt _ _ _ = ".dynlib"
    compile _ is (File o) e = cmd "clang" "-shared" e "-o" o $ map (\(File f) -> f) is
instance Compiler CLANG [File Object] (File Executable) where
    genOutputExt _ _ _ = ""
    compile _ is (File o) e = cmd "clang" e "-o" o $ map (\(File f) -> f) is

class CompilerConfig compiler => Compiler compiler input output where
    genOutputExt :: compiler -> Proxy input -> Proxy output -> String
    compile :: CmdResult r
            => compiler
            -> input
            -> output
            -> [String] -- extras
            -> Action r

makeObject :: (Compiler compiler (File Source) (File Object), OS os, Arch arch)
           => ProjectConfig arch os compiler
           -> ModuleName
           -> [IncludeDir]
           -> File Source
           -> Rules (File Object)
makeObject pc mn includes sf@(File input) = do
    let compiler = getCompiler pc
    let ext      = genOutputExt compiler (Proxy :: Proxy (File Source)) (Proxy :: Proxy (File Object))
    let out@(File fp) = File $ projectConfigOutputObjDir pc mn </> input -<.> ext :: File Object
    fp %> \_ -> compile compiler sf out (toIncludeFlags compiler includes ++ getExtraFlags pc)
    return out

makeObjects :: (Compiler compiler (File Source) (File Object), OS os, Arch arch)
            => ProjectConfig arch os compiler
            -> ModuleName
            -> [IncludeDir]
            -> [File Source]
            -> Rules [File Object]
makeObjects p m i = mapM (makeObject p m i)

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
makeSharedLib pc mn deps includes sources = do
    let compiler = getCompiler pc
    let ext      = genOutputExt compiler (Proxy :: Proxy [File Object]) (Proxy :: Proxy (File SharedLib))
    objs <- makeObjects pc mn (includes ++ includeDependencies deps) sources
    let out@(File fp) = File $ projectConfigOutputSharedDir pc mn </> mn -<.> ext :: File SharedLib
    fp %> \_ -> do
        need $ map dependencyName deps
        need $ map (\(File f) -> f) objs
        compile compiler objs out (getExtraFlags pc ++ addDependenciesLib deps)
    return $ Dependency fp includes (map dependencyName deps) [ResultSharedLib out]

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
makeExecutable pc mn deps includes sources = do
    let compiler = getCompiler pc
    let ext      = genOutputExt compiler (Proxy :: Proxy [File Object]) (Proxy :: Proxy (File Executable))
    objs <- makeObjects pc mn (includes ++ includeDependencies deps) sources
    let out@(File fp) = File $ projectConfigOutputSharedDir pc mn </> mn -<.> ext
    fp %> \_ -> do
        need $ map dependencyName deps
        need $ map (\(File f) -> f) objs
        compile compiler objs out (getExtraFlags pc ++ addDependenciesLib deps)
    return $ Dependency fp includes (map dependencyName deps) [ResultExecutable out]

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
