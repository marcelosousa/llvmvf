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
import Language.HTm.Base
import Language.LLVMIR.Converter (llvmir2Htm)

import Text.Blaze.Html.Renderer.String as P
import Language.LLVMIR.Printer.NamedTypes 

import qualified Language.LTL.Base as LTL
import UU.PPrint 
import Language.SMTLib2.Printer    (prettyprint)

import Concurrent.Model
import Concurrent.Model.PThread
-- import Concurrent.Model.SystemC
import Concurrent.Model.Visualizer
-- import Concurrent.Model.ESEncoder  (esencode)    
import Concurrent.Model.Encoder    (encode) 
-- , encodeSysC)    

import Util.Demangler
import Debug.Trace
import Analysis.Memory.Type

_program, _summary :: String
_summary = unlines ["LLVM Verification Framework - v0.2","llvmvf supports verification of pthread code at the LLVM IR level.","Copyright 2012 @ Marcelo Sousa"]
_program = "llvmvf"
_help    = "The input files of llvmvf are byte code files generated from a LLVM front-end (eg. clang)"
_helpBMC = "Example: llvmvf bmc -d=pthread -b=10 x.bc"
_helpExtract = unlines ["llvmvf extract pretty prints the internal LLVM IR representations into the .llvf file.","Example: llvmvf extract x.bc"]
_helpModel = unlines ["llvmvf model outputs a model representation into the .model file.","Example: llvmvf model -d=pthread x.bc"]
_helpType = unlines ["llvmvf type uses a refined type system for separation of regular (user/kernel) and I/O memory"]

data Option = Extract   {input :: FilePath}
            | Visualize {input :: FilePath}
            | Model     {input :: FilePath, domain :: Domain}
            | BMC       {input :: FilePath, domain :: Domain, bound :: Int}
            | Convert   {input :: FilePath}
            | Type      {input :: FilePath}
  deriving (Show, Data, Typeable, Eq)

data Domain = PThread | SystemC
  deriving (Show, Data, Typeable, Eq)

instance Default Domain where
  def = PThread

extractMode :: Option
extractMode = Extract  { input = def &= args } &= help _helpExtract

modelMode :: Option
modelMode = Model { input = def &= args
                  , domain = def &= help "domain of verification: PThread | SystemC (Super Beta)" 
                  } &= help _helpModel

bmcMode :: Option
bmcMode = BMC { input = def &= args
              , domain = def &= help "domain of verification: PThread | SystemC (Super Beta)" 
              , bound  = def &= help "bound (k): Int"
              } &= help _helpBMC

typeMode :: Option
typeMode = Type { input = def &= args } &= help _helpType

progModes :: Mode (CmdArgs Option)
progModes = cmdArgsMode $ modes [extractMode, modelMode, bmcMode, typeMode]
         &= help _help
         &= program _program
         &= summary _summary

-- | 'main' function 
main :: IO ()
main = do options <- cmdArgsRun progModes
          runOption options
          
runOption :: Option -> IO ()
runOption (Extract bc) = do mdl <- extract bc
                            let bf = dropExtension bc
                            writeFile (addExtension bf "llvf") (show $ pretty mdl)
runOption (Model bc d) = runModel bc d                           
runOption (BMC bc d k) = do print $ "Working " ++ show bc ++ show d ++ show k
                            runBMC bc d k
runOption (Type bc) = do mdl <- extract bc
                         print $ typeAnalysis mdl
{-runOption bc Htm     = do mdl <- extract bc
                          let bf = dropExtension bc
                          writeFile (addExtension bf "htm") (show $ pretty $ llvmir2Htm mdl)
-}
--runOption bc Parse k = do mdl <- extract bc
--                          let bf  = dropExtension bc
--                              mod = (model mdl) :: Model PThread
--                          writeFile (addExtension bf "llvf")  (show $ pretty mdl)
--                          writeFile (addExtension bf "model") (show $ mod) 
--                          writeFile (addExtension bf "dot")   (show $ pretty mod)
--                          writeFile (addExtension bf "dfg")   (show $ dataflow mod)
--                          writeFile (addExtension bf "smt2")  (show $ prettyprint $ encode mod k)
--runOption bc Visualize _ = do mdl <- extract bc
--                              let bf = dropExtension bc
--                                  mod = (model mdl) :: Model PThread
--                              writeFile (addExtension bf "dot") (show $ pretty mod)
--runOption bc SystemC   k = do print "SystemC version"
--                              mdl <- extract bc
--                              let bf = dropExtension bc
--                                  mod = (model mdl) :: Model SystemC
--                              writeFile (addExtension bf "llvf")  (show $ pretty mdl)
--                              writeFile (addExtension bf "model") (show $ mod)
--                              writeFile (addExtension bf "arch")  (show $ retrieveSCArch mdl)
--                              writeFile "index.html" $ P.renderHtml (index $ nmdtys mod)
--                              writeFile "types.html" $ P.renderHtml (types $ nmdtys mod)
--                              writeFile (addExtension bf "dot")   (show $ pretty mod)
--                              writeFile (addExtension bf "rawm")  (show $ mdl)
--                              writeFile (addExtension bf "smt2")  (show $ prettyprint $ encodeSysC mod k)

-- | 'runModel' - extract the model
runModel :: FilePath -> Domain -> IO ()
runModel bc SystemC = error "llvmvf for SystemC is currently not available."
runModel bc PThread = do mdl <- extract bc
                         let bf  = dropExtension bc
                             mod = (model mdl) :: Model PThread
                             outfile = addExtension bf "model"
                         writeFile outfile $ show mod

-- | 'runBMC' - main bmc function
runBMC :: FilePath -> Domain -> Bound -> IO ()
runBMC bc SystemC _ = error "llvmvf for SystemC is currently not available."
runBMC bc PThread k = do mdl <- extract bc
                         let bf  = dropExtension bc
                             mod = (model mdl) :: Model PThread
                             outfile = addExtension bf "smt2"
                         putStrLn $ "Generating " ++ outfile ++ "..."  
                         writeFile outfile (show $ prettyprint $ encode mod k)