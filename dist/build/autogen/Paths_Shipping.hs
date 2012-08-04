module Paths_Shipping (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/michael/src/Shipping/cabal-dev//bin"
libdir     = "/home/michael/src/Shipping/cabal-dev//lib/Shipping-0.0.0/ghc-7.4.2"
datadir    = "/home/michael/src/Shipping/cabal-dev//share/Shipping-0.0.0"
libexecdir = "/home/michael/src/Shipping/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Shipping_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Shipping_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Shipping_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Shipping_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
