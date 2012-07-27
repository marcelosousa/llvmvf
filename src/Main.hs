{-# LANGUAGE DeriveDataTypeable #-}

-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Main where

import Control.Monad             
import System.Console.CmdArgs
import System.IO               
import System.FilePath
import Language.LLVMIR             (Module)
import Language.LLVMIR.Extractor   (extract)
import Language.LTL.Base
import Mutation.Core               (mutate)
import UU.PPrint 

import Concurrent.Model

import Debug.Trace

-- Options 
data Options = Parse
             | Mutate
  deriving (Show, Data, Typeable)

instance Default Options where
  def = Parse

runOption :: FilePath -> Options -> IO ()
runOption bc Parse = do mdl <- extract bc
                        print $ pretty mdl
runOption bc Mutate = mutate bc

data ProgramOptions = LLVMVF {
    input  :: FilePath
  , typeoutput :: Options
}
  deriving (Show, Data, Typeable)

standard = cmdArgsMode $ LLVMVF 
           { 
             input         = (def &= args )
           , typeoutput    = (def &= help "Parse" &= typ "Parse")
           } &= summary usage

main = do args <- cmdArgsRun standard
          runllvmvf args
          
runllvmvf :: ProgramOptions -> IO ()
runllvmvf options = do let filename = input options
                           option   = typeoutput options
                       runOption filename option
                    
usage :: String
usage = unlines ["LLVM Verification Framework"]
