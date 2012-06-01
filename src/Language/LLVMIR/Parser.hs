{-#LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Parser
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Parser where

import Control.Monad(forM_, forM, foldM)
import System.FilePath

import qualified Language.LLVMIR as LL

import LLVM.Core hiding (Value) 
import LLVM.ExecutionEngine hiding (getTargetData)

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.Target as FFI

import Text.ParserCombinators.UU hiding (parse)
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances

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
                target <- getTargetData mdl
                funs <- getFuncs mdl
                gvars <- getGlobalVar mdl
                return $ LL.Module layout target funs gvars

-- Target data

pSomewhere :: String -> Parser String
pSomewhere x =  pToken x <* pList pAscii
            <|> pAscii *> pSomewhere x

pTarget :: Parser LL.Target
pTarget =  const LL.MacOs <$> pSomewhere "apple"
       <|> const LL.Linux <$> pSomewhere "linux"  
          
getTargetData :: Module -> IO LL.TargetData
getTargetData mdl = withModule mdl $ \mdlPtr -> do 
                     cs <- FFI.getTarget mdlPtr
                     s <- peekCString cs                            
                     return $ LL.TargetData s $ runParser "parsing target" pTarget s
  
-- Data Layout    
getSDataLayoutModule :: Module -> IO LL.DataLayout
getSDataLayoutModule mdl = withModule mdl $ \mdlPtr -> do 
                            cs <- FFI.getDataLayout mdlPtr
                            s <- peekCString cs                            
                            return $ LL.DataLayout $ runParser "error" pDataLayout s
                            
getDataLayoutModule :: Module -> IO TargetData
getDataLayoutModule mdl = withModule mdl $ \mdlPtr -> do 
                            cs <- FFI.getDataLayout mdlPtr
                            trgPtr <- FFI.createTargetData cs
                            return $ makeTargetData trgPtr

pChar :: Parser Char
pChar = pLetter <|> pDigit <|> pSym ':'

pElem :: Parser String
pElem = pList1 pChar

pDataLayout :: Parser [String]
pDataLayout = (:) <$> pElem <*> pList (pSym '-' *> pElem)

-- pEndianness :: Parser Endianness
-- pEndianness =  const BigEndian    <$> pToken "E"
--            <|> const LittleEndian <$> pToken "e"
           
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
getInstruction (instr, instrv) = do (s,inst) <- getInstrDesc instrv
                                   -- sops <- foldM (\s (v,_) -> return $ v ++ " " ++ s) "" ops
                                    return $ LL.Instruction (s ++ "=" ++ show inst)