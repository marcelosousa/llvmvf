{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE DoAndIfThenElse #-}
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

import Language.LLVMIR.Converter
import Language.LLVMIR.Util

import Foreign.C.String
import Foreign.C.Types

type FunctionName = String

parse :: FilePath -> IO LL.Module 
parse file = do mdl <- readBitcodeFromFile file
                i <- getModuleIdentifier mdl
                layout <- getDataLayout mdl
                target <- getTargetData mdl
                funs <- getFuncs mdl
                gvars <- getGlobalVar mdl
--                aliases <- getAliases mdl
                return $ LL.Module i layout target gvars funs [] -- aliases 

-- Module Identifier
getModuleIdentifier :: Module -> IO String
getModuleIdentifier mdl = withModule mdl $ \mdlPtr -> do
                           cs <- FFI.getModuleIdentifier mdlPtr
                           peekCString cs

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
getDataLayout :: Module -> IO LL.DataLayout
getDataLayout mdl = withModule mdl $ \mdlPtr -> do 
                     cs <- FFI.getDataLayout mdlPtr
                     s <- peekCString cs                            
                     return $ LL.DataLayout $ runParser "error" pDataLayout s

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
getGlobalVar :: Module -> IO LL.Globals
getGlobalVar mdl = do globals <- getGlobalVariables mdl
                      forM globals getGlobal

getInitVal :: Value -> Bool -> IO (Maybe LL.Value)
getInitVal gv isC | isC == False = return $ Nothing
                  | otherwise    = do cval <- FFI.getInitializer gv
                                      llval <- FFI.constantValueGetAsString cval >>= peekCString
                                      num   <- FFI.constantValueGetNumElem cval
                                      ety   <- FFI.constantValueGetElemType cval
                                      llety <- getType ety 
                                      let ty = LL.TyArray (fromEnum num) llety
                                      return $ Just $ LL.Const $ LL.ArrayC ty llval 


getGlobal :: (String, Value) -> IO LL.Global
getGlobal (gname, gval) = do link  <- FFI.getLinkage gval
                             isC   <- isConstant gval
                             align <- FFI.getAlignment gval
                             unadd <- FFI.hasUnnamedAddr gval
                             let llalign = LL.Align $ fromEnum align
                                 llunadd = cUInt2Bool unadd
                                 lllink  = convertLinkage $ FFI.toLinkage link
                             --llival <- getInitVal gval isC
                             return $ LL.GlobalVar gname lllink isC llunadd llalign -- llival llalign

 
-- Functions
getFuncs :: Module -> IO LL.Functions
getFuncs mdl = do funs <- getFunctions mdl
                  forM funs getFunction

getFunction :: (String, Value) -> IO LL.Function
getFunction (fname, fval) = do b <- FFI.isDeclaration fval
                               rty <- (FFI.getFunctionReturnType fval) >>= getType
                               link <- FFI.getLinkage fval
                               pars <- getParams fval
                               params <- forM pars getParam
                               let lllink = convertLinkage $ FFI.toLinkage link
                               if cInt2Bool b
                               then return $ LL.FunctionDecl fname lllink rty params
                               else do bbs <- getBasicBlocks fval
                                       llbbs <- forM bbs getBasicBlock 
                                       return $ LL.FunctionDef fname lllink rty params llbbs

getParam :: (String, Value) -> IO LL.Parameter
getParam (pname, pval) = do ty <- (FFI.typeOf pval) >>= getType 
                            return $ LL.Parameter pname ty

-- Basic Blocks                                 
getBasicBlock :: (String, Value) -> IO LL.BasicBlock
getBasicBlock (bbname, bbvalue) = do instr  <- getInstructions bbvalue
                                     instrs <- forM instr getInstruction
                                     return $ LL.BasicBlock bbname instrs
                   
getInstruction :: (String, Value) -> IO LL.Instruction
getInstruction (instr, instrv) = getInst instrv


{-
-- Aliases
getAliases :: Module -> IO LL.Aliases
getAliases mdl = do aliases <- getAlias mdl
                    hasAlias <- FFI.aliasEmpty (fromModule mdl)
                    print $ "Module has aliases :" ++ (show hasAlias)
                    print $ "It has " ++ (show $ length aliases)
                    forM aliases getAlias'

getAlias' :: (String, Value) -> IO LL.Alias
getAlias' (aname, aval) = return $ LL.Alias aname
-}
isConstant :: Value -> IO Bool
isConstant v = do ci <- FFI.isGlobalConstant v
		  return $ cInt2Bool ci
