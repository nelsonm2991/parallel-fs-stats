{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_fs_project (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/matthew/.cabal/bin"
libdir     = "/home/matthew/.cabal/lib/x86_64-linux-ghc-8.8.4/fs-project-0.1.0.0-inplace"
dynlibdir  = "/home/matthew/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/matthew/.cabal/share/x86_64-linux-ghc-8.8.4/fs-project-0.1.0.0"
libexecdir = "/home/matthew/.cabal/libexec/x86_64-linux-ghc-8.8.4/fs-project-0.1.0.0"
sysconfdir = "/home/matthew/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fs_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fs_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "fs_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "fs_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fs_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fs_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
