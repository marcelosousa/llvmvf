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

import Text.Blaze.Html.Renderer.String as P
import Language.LLVMIR.Printer.NamedTypes 

import qualified Language.LTL.Base as LTL
import UU.PPrint 
import Language.SMTLib2.Printer    (prettyprint)

import Concurrent.Model
import Concurrent.Model.PThread
import Concurrent.Model.SystemC
import Concurrent.Model.Visualizer
-- import Concurrent.Model.ESEncoder  (esencode)    
import Concurrent.Model.Encoder    (encode, encodeSysC)    

import Util.Demangler
import Debug.Trace

-- This module needs re-factoring for elegance.

-- Options 
data Options = Parse
             | Visualize
             | Extract
             | SystemC
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
runOption bc Visualize _ = do mdl <- extract bc
                              let bf = dropExtension bc
                                  mod = (model mdl) :: Model PThread
                              writeFile (addExtension bf "dot") (show $ pretty mod)
runOption bc Extract   _ = do mdl <- extract bc
                              let bf = dropExtension bc
                              writeFile (addExtension bf "llvf") (show $ pretty mdl)
runOption bc SystemC   k = do print "SystemC version"
                              mdl <- extract bc
                              let bf = dropExtension bc
                                  mod = (model mdl) :: Model SystemC
                              writeFile (addExtension bf "llvf")  (show $ pretty mdl)
                              writeFile (addExtension bf "model") (show $ mod)
                              writeFile (addExtension bf "arch")  (show $ retrieveSCArch mdl)
--                              writeFile "index.html" $ P.renderHtml (index $ nmdtys mod)
--                              writeFile "types.html" $ P.renderHtml (types $ nmdtys mod)
--                              writeFile (addExtension bf "dot")   (show $ pretty mod)
--                              writeFile (addExtension bf "rawm")  (show $ mdl)
--                              writeFile (addExtension bf "smt2")  (show $ prettyprint $ encodeSysC mod k)

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

main :: IO ()
main = do args <- cmdArgsRun standard
          runllvmvf args
          
runllvmvf :: ProgramOptions -> IO ()
runllvmvf options = do let filename = input options
                           option   = typeoutput options
                           k        = bound options
                       runOption filename option k

usage :: String
usage = unlines ["LLVM Verification Framework"]
