module Paths_demangler (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/marcelosousa/.cabal/bin"
libdir     = "/home/marcelosousa/.cabal/lib/demangler-1.0/ghc-7.6.1"
datadir    = "/home/marcelosousa/.cabal/share/demangler-1.0"
libexecdir = "/home/marcelosousa/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "demangler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "demangler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "demangler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "demangler_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
