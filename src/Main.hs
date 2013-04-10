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
--import Concurrent.Model.Encoder    (encode)
import Test.Example

import Util.Demangler
import Debug.Trace
import Analysis.Type

_program, _summary :: String
_summary = unlines ["LLVM Verification Framework - v0.2","llvmvf supports verification of pthread code at the LLVM IR level.","Copyright 2012-2013 @ Marcelo Sousa"]
_program = "llvmvf"
_help    = "The input files of llvmvf are byte code files generated from a LLVM front-end (eg. clang)"
_helpBMC = "Example: llvmvf bmc -d=pthread -b=10 x.bc"
_helpExtract = unlines ["llvmvf extract pretty prints the internal LLVM IR representations into the .llvf file.","Example: llvmvf extract x.bc"]
_helpCCFG = unlines ["llvmvf ccfg output a .dot file with the concurrent control flow graph.","Example: llvmvf ccfg x.bc"]
_helpArch = unlines ["llvmvf arch outputs the concurrent architecture representation of the model into the .model file.","Example: llvmvf arch -d=pthread x.bc"]
_helpTypeCheck = unlines ["llvmvf typecheck checks if the LLVM IR types are consistent with the typing rules defined in the Language Reference."]
_helpType = unlines ["llvmvf type uses a refined type system for separation of regular (user/kernel) and I/O memory"]

data Option = Extract   {input :: FilePath}
            | CCFG      {input :: FilePath, domain :: Domain}
            | Arch      {input :: FilePath, domain :: Domain}
            | BMC       {input :: FilePath, domain :: Domain, bound :: Int}
            | Convert   {input :: FilePath}
            | Type      {input :: FilePath}
            | TypeCheck {input :: FilePath}
  deriving (Show, Data, Typeable, Eq)

data Domain = PThread | SystemC
  deriving (Show, Data, Typeable, Eq)

instance Default Domain where
  def = PThread

extractMode :: Option
extractMode = Extract  { input = def &= args
                       } &= help _helpExtract

ccfgMode :: Option
ccfgMode = CCFG  { input = def &= args 
                 , domain = def &= help "domain of verification: PThread | SystemC (Super Beta)" 
                 } &= help _helpCCFG

archMode :: Option
archMode = Arch { input = def &= args
                , domain = def &= help "domain of verification: PThread | SystemC (Super Beta)" 
                } &= help _helpArch

bmcMode :: Option
bmcMode = BMC { input = def &= args
              , domain = def &= help "domain of verification: PThread | SystemC (Super Beta)" 
              , bound  = def &= help "bound (k): Int"
              } &= help _helpBMC

typeMode :: Option
typeMode = Type { input = def &= args } &= help _helpType

typeCheckMode :: Option
typeCheckMode = TypeCheck { input = def &= args } &= help _helpTypeCheck

progModes :: Mode (CmdArgs Option)
progModes = cmdArgsMode $ modes [extractMode, archMode, bmcMode, typeCheckMode, typeMode, ccfgMode]
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
runOption (Arch bc d) = runArch bc d                           
runOption (BMC bc d k) = runBMC bc d k
runOption (TypeCheck bc) = do mdl <- extract bc
                              print mdl
                              print $ typeCheck mdl
runOption (Type bc) = do mdl <- extract bc
                         print $ typeAnalysis mdl
runOption (CCFG bc d) = runCCFG bc d
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
runArch :: FilePath -> Domain -> IO ()
runArch bc SystemC = error "llvmvf for SystemC is currently not available."
runArch bc PThread = do mdl <- extract bc
                        let bf  = dropExtension bc
                            mod = (model mdl) :: Model PThread
                            outfile = addExtension bf "model"
                        writeFile outfile (show mod)

-- | 'runBMC' - main bmc function
runBMC :: FilePath -> Domain -> Bound -> IO ()
runBMC bc SystemC _ = error "llvmvf for SystemC is currently not available."
runBMC bc PThread k = do mdl <- extract bc
                         let bf  = dropExtension bc
                             mod = (model mdl) :: Model PThread
                             outfile = addExtension bf "smt2"
                         putStrLn $ "Generating " ++ outfile ++ "..."  
                         --writeFile outfile (show $ prettyprint $ encode mod k)

-- | 'runCCFG' - extract the concurrent control flow graph
runCCFG :: FilePath -> Domain -> IO ()
runCCFG bc SystemC = error "llvmvf for SystemC is currently not available."
runCCFG bc PThread = do mdl <- extract bc
                        let bf = dropExtension bc
                            mod = (model mdl) :: Model PThread
                        writeFile (addExtension bf "dot") (show $ pretty mod)