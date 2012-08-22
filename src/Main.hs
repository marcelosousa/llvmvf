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
import Language.LLVMIR.Printer
import qualified Language.LTL.Base as LTL
import Mutation.Core               (mutate)
import UU.PPrint 
import Language.SMTLib2.Printer    (prettyprint)

import Concurrent.Model
import Concurrent.Model.PThread
import Concurrent.Model.Visualizer
-- import Concurrent.Model.ESEncoder  (esencode)    
import Concurrent.Model.Encoder    (encode)    

import Debug.Trace

-- Options 
data Options = Parse
             | Mutate
             | Visualize
             | Extract
  deriving (Show, Data, Typeable)

instance Default Options where
  def = Parse

runOption :: FilePath -> Options -> Int -> IO ()
runOption bc Parse k = do mdl <- extract bc
                          let bf  = dropExtension bc
                              mod = (model mdl) :: Model PThread
                          writeFile (addExtension bf "llvf")  (show $ pretty mdl)
                          writeFile (addExtension bf "model") (show $ mod) 
                          writeFile (addExtension bf "dot")   (show $ pretty mod)
                          writeFile (addExtension bf "dfg")   (show $ dataflow mod)
                          writeFile (addExtension bf "smt2")  (show $ prettyprint $ encode mod k)
runOption bc Mutate    _ = mutate bc
runOption bc Visualize _ = do mdl <- extract bc
                              let bf = dropExtension bc
                                  mod = (model mdl) :: Model PThread
                              writeFile (addExtension bf "dot") (show $ pretty mod)
runOption bc Extract   _ = do mdl <- extract bc
                              let bf = dropExtension bc
                              writeFile (addExtension bf "llvf") (show $ pretty mdl)

data ProgramOptions = LLVMVF {
    input  :: FilePath
  , typeoutput :: Options
  , bound :: Int
}
  deriving (Show, Data, Typeable)

standard = cmdArgsMode $ LLVMVF 
           { 
             input         = (def &= args )
           , typeoutput    = (def &= help "Parse" &= typ "Parse")
           , bound         = (def &= help "bound" &= opt (1 :: Int))
           } &= summary usage

main = do args <- cmdArgsRun standard
          runllvmvf args
          
runllvmvf :: ProgramOptions -> IO ()
runllvmvf options = do let filename = input options
                           option   = typeoutput options
                           k        = bound options
                       runOption filename option k
                    
usage :: String
usage = unlines ["LLVM Verification Framework"]
