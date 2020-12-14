{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_proj (
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

bindir     = "/Users/merden/.cabal/bin"
libdir     = "/Users/merden/.cabal/lib/x86_64-osx-ghc-8.6.5/proj-0.1.0.0-inplace-nscheme"
dynlibdir  = "/Users/merden/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/merden/.cabal/share/x86_64-osx-ghc-8.6.5/proj-0.1.0.0"
libexecdir = "/Users/merden/.cabal/libexec/x86_64-osx-ghc-8.6.5/proj-0.1.0.0"
sysconfdir = "/Users/merden/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "proj_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "proj_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "proj_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "proj_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "proj_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "proj_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
