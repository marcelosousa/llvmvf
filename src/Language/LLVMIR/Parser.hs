-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Parser
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Parser where

import Control.Monad(forM_, forM)
import System.FilePath

import qualified Language.LLVMIR.Base as LL

import LLVM.Core hiding (Value) 
import LLVM.ExecutionEngine

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.Target as FFI

import Foreign.C.String

type FunctionName = String
type Value = FFI.ValueRef

{-
parse :: FilePath -> IO ()
parse file = do mdl <- readBitcodeFromFile file                         
                parseModule mdl
                printDataLayoutModule mdl
                trg <- getDataLayoutModule mdl
                print $ littleEndian trg 
-}

parse :: FilePath -> IO LL.Module 
parse file = do mdl <- readBitcodeFromFile file
                layout <- getSDataLayoutModule mdl
                funs <- getFuncs mdl
                gvars <- getGlobalVar mdl
                return $ LL.Module layout funs gvars

-- Data Layout    
getSDataLayoutModule :: Module -> IO LL.TargetData
getSDataLayoutModule mdl = withModule mdl $ \mdlPtr -> do 
                            cs <- FFI.getDataLayout mdlPtr
                            s <- peekCString cs                            
                            return $ LL.TargetData s
                            
getDataLayoutModule :: Module -> IO TargetData
getDataLayoutModule mdl = withModule mdl $ \mdlPtr -> do 
                            cs <- FFI.getDataLayout mdlPtr
                            trgPtr <- FFI.createTargetData cs
                            return $ makeTargetData trgPtr

-- Global Variables
getGlobalVar :: Module -> IO LL.GlobalVars
getGlobalVar mdl = do globals <- getGlobalVariables mdl
                      let gvars = map (\(x,_) -> LL.GlobalVar x) globals
                      return gvars
                      
-- Functions
getFuncs :: Module -> IO LL.Functions
getFuncs mdl = do funs <- getFunctions mdl
                  forM funs getFunction

getFunction :: (String, Value) -> IO LL.Function
getFunction (fname, fvalue) = do bbs <- getBasicBlocks fvalue
                                 bbs' <- forM bbs getBasicBlock
                                 let res = if bbs == []
                                           then LL.FunctionDecl fname
                                           else LL.FunctionDef fname bbs'
                                 return res

-- Basic Blocks                                 
getBasicBlock :: (String, Value) -> IO LL.BasicBlock
getBasicBlock (bbname, bbvalue) = do instr  <- getInstructions bbvalue
                                     instrs <- forM instr getInstruction
                                     return $ LL.BasicBlock bbname instrs
                   
getInstruction :: (String, Value) -> IO LL.Instruction
getInstruction (instr, instrv) = return $ LL.Instruction instr