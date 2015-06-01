module Paths_GameOfSymbolGrounding (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/yuichiro/Haskell/GameOfSymbolGrounding/.cabal-sandbox/bin"
libdir     = "/home/yuichiro/Haskell/GameOfSymbolGrounding/.cabal-sandbox/lib/i386-linux-ghc-7.6.3/GameOfSymbolGrounding-0.1.0.0"
datadir    = "/home/yuichiro/Haskell/GameOfSymbolGrounding/.cabal-sandbox/share/i386-linux-ghc-7.6.3/GameOfSymbolGrounding-0.1.0.0"
libexecdir = "/home/yuichiro/Haskell/GameOfSymbolGrounding/.cabal-sandbox/libexec"
sysconfdir = "/home/yuichiro/Haskell/GameOfSymbolGrounding/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "GameOfSymbolGrounding_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "GameOfSymbolGrounding_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "GameOfSymbolGrounding_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GameOfSymbolGrounding_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "GameOfSymbolGrounding_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
