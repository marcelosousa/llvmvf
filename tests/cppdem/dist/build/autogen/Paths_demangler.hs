module Paths_demangler (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/marcelosousa/.cabal/bin"
libdir     = "/home/marcelosousa/.cabal/lib/demangler-1.0/ghc-7.0.3"
datadir    = "/home/marcelosousa/.cabal/share/demangler-1.0"
libexecdir = "/home/marcelosousa/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "demangler_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "demangler_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "demangler_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "demangler_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
