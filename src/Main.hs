{-# LANGUAGE DeriveDataTypeable, UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Main where

import Control.Monad             
import System.Console.CmdArgs
import System.IO               
import System.FilePath
import System.Directory
import Language.LLVMIR             (Module)
import Language.LLVMIR.Extractor   (extract)
import Language.LLVMIR.Printer
import Language.LLVMIR.Util        
import Language.HTm.Base
import Language.LLVMIR.Converter (llvmir2Htm)
import Language.FeatherIR

import Text.Blaze.Html.Renderer.String as P
import Language.LLVMIR.Printer.NamedTypes 

import qualified Language.LTL.Base as LTL
import UU.PPrint 
import Language.SMTLib2.Printer    (prettyprint)
import qualified Data.Map as M
import Analysis.Asm.Lift
import Analysis.Simplify.Intrinsics
import Analysis.Metadata.Call
import qualified Concurrent.Model as M
import Concurrent.Model.Domain.PThread
-- import Concurrent.Model.SystemC
import Concurrent.Model.Visualizer
-- import Concurrent.Model.ESEncoder  (esencode)    
--import Concurrent.Model.Encoder    (encode)
import Test.Example

import Util.Demangler
import Debug.Trace
import Analysis.Type

import Analysis.Type.Inference.Module (typeAnnInference,typeConstraints,typeAnnInferenceIP,typeAnnInferenceGlobals)

_program, _summary :: String
_summary = unlines ["LLVM Verification Framework - v0.2 (type beta)","llvmvf supports verification of pthread code at the LLVM IR level.","Copyright 2012-2013 @ Marcelo Sousa"]
_program = "llvmvf"
_help    = "The input files of llvmvf are byte code files generated from a LLVM front-end (eg. clang)"
_helpBMC = "Example: llvmvf bmc -d=pthread -b=10 x.bc"
_helpExtract = unlines ["llvmvf extract pretty prints the internal LLVM IR representations into the .llvf file.","Example: llvmvf extract x.bc"]
_helpCCFG = unlines ["llvmvf ccfg output a .dot file with the concurrent control flow graph.","Example: llvmvf ccfg x.bc"]
_helpArch = unlines ["llvmvf arch outputs the concurrent architecture representation of the model into the .model file.","Example: llvmvf arch -d=pthread x.bc"]
_helpTypeCheck = unlines ["llvmvf typecheck checks if the LLVM IR types are consistent with the typing rules defined in the Language Reference."]
_helpType = unlines ["llvmvf type uses a refined type system for separation of regular (user/kernel) and I/O memory"]
_helpTypeAnalyze = unlines ["llvmvf typeanalyze performs  inter and intra-procedural lightweight type analyses"]
_helpLiftAsm = unlines ["llvmvf liftasm lifts inline assembly code to LLVM IR functions"]

data Option = Extract   {input :: FilePath, emode  ∷ ExtractMode}
            | CCFG      {input :: FilePath, domain :: Domain}
            | Model     {input :: FilePath, domain :: Domain}
            | BMC       {input :: FilePath, domain :: Domain, mainName ∷ String, bound :: Int}
            | Convert   {input :: FilePath}
            | Type      {input :: FilePath, tmode ∷ TypeMode}
            | LiftInline {input ∷ FilePath}
            | TypeCheck {input :: FilePath}
            | TypeConstrs {input ∷ FilePath}
            | TypeAnalyze {files ∷ [FilePath]}
            | Analyse {input :: FilePath}
  deriving (Show, Data, Typeable, Eq)

data ExtractMode = Raw | Pretty | LiftAsm | Isolated
  deriving (Show, Data, Typeable, Eq)

data Domain = PThread | SystemC
  deriving (Show, Data, Typeable, Eq)

data TypeMode = Intra | Inter | Globals
  deriving (Show, Data, Typeable, Eq)

instance Default ExtractMode where
  def = Pretty

instance Default Domain where
  def = PThread

instance Default TypeMode where
  def = Intra

extractMode :: Option
extractMode = Extract  { input = def &= args
                       , emode = def &= help "mode of extraction: LiftAsm | Raw | Pretty (default)" 
                       } &= help _helpExtract

ccfgMode :: Option
ccfgMode = CCFG  { input = def &= args 
                 , domain = def &= help "domain of verification: PThread | SystemC (Super Beta)" 
                 } &= help _helpCCFG

modelMode :: Option
modelMode = Model { input = def &= args
                  , domain = def &= help "domain of verification: PThread | SystemC (Super Beta)" 
                  } &= help _helpArch

bmcMode :: Option
bmcMode = BMC { input = def &= args
              , domain = def &= help "domain of verification: PThread | SystemC (Super Beta)" 
              , bound  = def &= help "bound (k): Int"
              , mainName = "main" &= help "name of the main function: String"
              } &= help _helpBMC

typeMode :: Option
typeMode = Type { input = def &= args 
                , tmode = def &= help "kind of analysis: Intra | Inter | Globals"
                } &= help _helpType

typeCMode :: Option
typeCMode = TypeConstrs { input = def &= args } &= help _helpType

typeCheckMode :: Option
typeCheckMode = TypeCheck { input = def &= args } &= help _helpTypeCheck

analyseMode :: Option
analyseMode = Analyse { input = def &= args } &= help _helpTypeCheck

typeAnalyze ∷ Option
typeAnalyze = TypeAnalyze { files = def &= args &= typ "FILES/DIRS" }

progModes :: Mode (CmdArgs Option)
progModes = cmdArgsMode $ modes [extractMode, modelMode, ccfgMode, bmcMode, typeCheckMode, typeMode,typeCMode,typeAnalyze, analyseMode]
         &= help _help
         &= program _program
         &= summary _summary

-- | 'main' function 
main :: IO ()
main = do options <- cmdArgsRun progModes
          runOption options
          
runOption :: Option -> IO ()
runOption (Extract bc m) = do mdl <- extract bc
                              let bf = dropExtension bc
                                  p = case m of
                                    Raw → show $ liftDebug mdl
                                    Pretty → show $ pretty mdl
                                    LiftAsm → show $ pretty $ liftAsm $ liftDebug mdl
				    Isolated -> show $ liftDebug $ isolateFunction (makeGlobal "e1000_cfg_on_link_up_80003es2lan") mdl
                              writeFile (addExtension bf "llvf") p
runOption (Model bc d) = extractModel bc d                           
runOption (CCFG bc d)  = runCCFG bc d
runOption (BMC bc d n k) = undefined --runBMC bc d n k
runOption (TypeCheck bc) = do mdl <- extract bc
                             -- print mdl
                              print $ typeCheck mdl
runOption (Type bc m ) = do mdl <- extract bc
                            case m of
                              Intra → typeInfIntra $ liftDebug mdl -- liftAsm $ liftDebug mdl  
                              Inter → typeInfInter $ liftDebug mdl -- liftAsm $ liftDebug mdl
                              Globals → typeInfGlobals $ liftAsm $ liftDebug mdl
runOption (TypeConstrs bc) = do mdl <- extract bc
                                typeConstraint $ liftAsm $ liftDebug mdl  --typeAnalysis mdl
runOption (Analyse dir) = if isOptBytecode dir
                          then do res <- iterateFile M.empty dir
                                  putStrLn $ infoToString 0 res
                          else do res <- iterateAnalyse M.empty dir
                                  putStrLn $ infoToString 50 res
runOption (TypeAnalyze lbc) = do 
  mdls ← mapM extract lbc
  let mdls' = map (liftAsm . liftDebug) mdls
  undefined --typeAnalysis mdls'
--runOption bc Htm     = do mdl <- extract bc
--                          let bf = dropExtension bc
--                          writeFile (addExtension bf "htm") (show $ pretty $ llvmir2Htm mdl)
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

-- | 'extractModel' - extract the model


iterateAnalyse :: Info -> FilePath -> IO Info
iterateAnalyse i d = do --print $ "processing directory " ++ d
                        filex <- getDirectoryContents d
                        let files = map (\s -> d ++ "/" ++ s) $ filter adhocF filex
	                dirs <- filterM doesDirectoryExist files
	                rdirs <- foldM iterateAnalyse i dirs 
	                let bfiles = filter isOptBytecode files
                        foldM iterateFile rdirs bfiles

iterateFile :: Info -> FilePath -> IO Info
iterateFile i bc = do --print $ "processing " ++ bc
                      mdl <- extract bc
		      return $ debugInfo i $ liftDebug mdl

adhocF :: FilePath -> Bool
adhocF "." = False
adhocF ".." = False
adhocF x = True

isOptBytecode :: FilePath -> Bool
isOptBytecode p = snd (splitExtensions p) == ".o.bc" 

extractModel :: FilePath -> Domain -> IO ()
extractModel bc SystemC = error "llvmvf for SystemC is currently not available."
extractModel bc PThread = do mdl <- extract bc
                             let bf  = dropExtension bc
                                 mod = (M.model mdl) :: M.Model PThread
                                 outfile = addExtension bf "model"
                             writeFile outfile (show mod)

-- | 'runCCFG' - extract the concurrent control flow graph
runCCFG :: FilePath -> Domain -> IO ()
runCCFG bc SystemC = error "llvmvf for SystemC is currently not available."
runCCFG bc PThread = do mdl <- extract bc
                        let bf = dropExtension bc
                            mod = (M.model mdl) :: M.Model PThread
                            (m,ccfg,_) = M.analyse "main" mod
                            outfile = addExtension bf "dot"
                        writeFile outfile (show $ dumpccfg m ccfg)

{-
-- | 'runBMC' - main bmc function
runBMC ∷ FilePath → Domain → String → M.Bound → IO ()
runBMC bc SystemC _ = error "llvmvf for SystemC is currently not available."
runBMC bc PThread m k = do mdl <- extract bc
                           let bf  = dropExtension bc
                               mod = (M.model mdl) :: M.Model PThread
                               (m,ccfg,_) = M.analyse m mod
                               outfile = addExtension bf "smt2"
                           putStrLn $ "Generating " ++ outfile ++ "..."  
                           writeFile outfile (show $ prettyprint $ encode mod m ccfg k)
-}
