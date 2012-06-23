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

import Language.LLVMIR.Converter

import Foreign.C.String
import Foreign.C.Types

type FunctionName = String

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
                i <- getModuleIdentifier mdl
                layout <- getDataLayout mdl
                target <- getTargetData mdl
                funs <- getFuncs mdl
                gvars <- getGlobalVar mdl
                aliases <- getAliases mdl
                return $ LL.Module i layout target gvars funs aliases 

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
getGlobalVar :: Module -> IO LL.GlobalVars
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


getGlobal :: (String, Value) -> IO LL.GlobalVar
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
                                  -- paraml = (fromIntegral . FFI.countParams) fval
                                   f = if cInt2Bool b
                                       then LL.FunctionDecl fname lllink rty params
                                       else LL.FunctionDef  fname lllink rty params
                               return f 

getParam :: (String, Value) -> IO LL.Parameter
getParam (pname, pval) = do ty <- (FFI.typeOf pval) >>= getType 
                            return $ LL.Parameter pname ty

{-
getFunction :: (String, Value) -> IO LL.Function
getFunction (fname, fvalue) = do bbs <- getBasicBlocks fvalue
                                 bbs' <- forM bbs getBasicBlock
                                 let res = if bbs == []
                                           then LL.FunctionDecl fname
                                           else LL.FunctionDef fname bbs'
                                 return res
-}
-- Basic Blocks                                 
getBasicBlock :: (String, Value) -> IO LL.BasicBlock
getBasicBlock (bbname, bbvalue) = do instr  <- getInstructions bbvalue
                                     instrs <- forM instr getInstruction
                                     return $ LL.BasicBlock bbname instrs
                   
getInstruction :: (String, Value) -> IO LL.Instruction
getInstruction (instr, instrv) = getInst instrv
                                   -- sops <- foldM (\s (v,_) -> return $ v ++ " " ++ s) "" ops
                                 --   return $ LL.Instruction (s ++ "=" ++ show inst)


-- Aliases
getAliases :: Module -> IO LL.Aliases
getAliases mdl = do aliases <- getAlias mdl
                    hasAlias <- FFI.aliasEmpty (fromModule mdl)
                    print $ "Module has aliases :" ++ (show hasAlias)
                    print $ "It has " ++ (show $ length aliases)
                    forM aliases getAlias'

getAlias' :: (String, Value) -> IO LL.Alias
getAlias' (aname, aval) = return $ LL.Alias aname

cUInt2Bool :: CUInt -> Bool
cUInt2Bool 0 = False
cUInt2Bool _ = True

cInt2Bool :: CInt -> Bool
cInt2Bool 0 = False
cInt2Bool _ = True

isConstant :: Value -> IO Bool
isConstant v = do ci <- FFI.isGlobalConstant v
		  return $ cInt2Bool ci
