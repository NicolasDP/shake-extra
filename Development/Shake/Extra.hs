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

import Development.Shake.Extra.AOC        as X
import Development.Shake.Extra.OS         as X
import Development.Shake.Extra.Arch       as X
import Development.Shake.Extra.Compiler   as X
import Development.Shake.Extra.Dependency as X
import Development.Shake.Extra.ProjectConfig as X
import Development.Shake.Extra.FindLib as X

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
