module Paths_llvmvf (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/mabs/Library/Haskell/ghc-7.0.3/lib/llvmvf-1.0/bin"
libdir     = "/Users/mabs/Library/Haskell/ghc-7.0.3/lib/llvmvf-1.0/lib"
datadir    = "/Users/mabs/Library/Haskell/ghc-7.0.3/lib/llvmvf-1.0/share"
libexecdir = "/Users/mabs/Library/Haskell/ghc-7.0.3/lib/llvmvf-1.0/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "llvmvf_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "llvmvf_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "llvmvf_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "llvmvf_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
