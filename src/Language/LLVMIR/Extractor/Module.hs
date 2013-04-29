{-#LANGUAGE FlexibleContexts, Rank2Types #-}
{-#LANGUAGE DoAndIfThenElse, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Module
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Module (extract) where

import Control.Monad(forM_, forM, foldM)
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import qualified Data.Map as Map

import System.FilePath

import qualified Language.LLVMIR as LL

import LLVM.Core hiding (Value) 
import LLVM.ExecutionEngine hiding (getTargetData)

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.Target as FFI

import Text.ParserCombinators.UU hiding (parse)
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances

import Language.LLVMIR.Extractor.Instruction
import Language.LLVMIR.Extractor.Type
import Language.LLVMIR.Extractor.Util
import Language.LLVMIR.Extractor.Linkage
import Language.LLVMIR.Extractor.Value
import Language.LLVMIR.Extractor.Context

import Util.Demangler

import Foreign.C.String
import Foreign.C.Types

type FunctionName = String

isConstant :: Value -> Context IO Bool
isConstant v = do ci <- liftIO $ FFI.isGlobalConstant v 
                  return $ cInt2Bool ci

hasInitializer :: Value -> Context IO Bool
hasInitializer v = do hi <- liftIO $ FFI.hasInitializer v
                      return $ cInt2Bool hi

extract :: FilePath -> IO LL.Module
extract file = do mdl <- readBitcodeFromFile file
                  let env = Env mdl Map.empty Nothing 1
                  runContext extractModule env

extractModule :: Context IO LL.Module
extractModule = do ident  <- getModuleIdentifier
                   layout <- getDataLayout
                   target <- getTargetData
                   gvars  <- getGlobalVar
                   funs   <- getFuns
                   e@Env{..} <- getEnv
                   return $ LL.Module ident layout target gvars funs nmdtys

-- Module Identifier
getModuleIdentifier :: Context IO String
getModuleIdentifier = do e@Env{..} <- getEnv
                         liftIO $ withModule mdl $ \mdlPtr -> do
                           cs <- FFI.getModuleIdentifier mdlPtr
                           peekCString cs
 
-- Target data
pSomewhere :: String -> Parser String
pSomewhere x =  pToken x <* pList pAscii
            <|> pAscii *> pSomewhere x

pTarget :: Parser LL.Target
pTarget =  const LL.MacOs <$> pSomewhere "apple"
       <|> const LL.Linux <$> pSomewhere "linux"  
          
getTargetData :: Context IO LL.TargetData
getTargetData = do e@Env{..} <- getEnv
                   liftIO $ withModule mdl $ \mdlPtr -> do 
                     cs <- FFI.getTarget mdlPtr
                     s <- peekCString cs                            
                     return $ LL.TargetData s $ runParser "parsing target" pTarget s
 
-- Data Layout    
getDataLayout :: Context IO LL.DataLayout
getDataLayout = do e@Env{..} <- getEnv
                   liftIO $ withModule mdl $ \mdlPtr -> do 
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
getGlobalVar :: Context IO LL.Globals
getGlobalVar = do e@Env{..} <- getEnv
                  globals   <- liftIO $ getGlobalVariables mdl
                  forM globals getGlobal

getInitVal :: Value -> Bool -> Context IO (Maybe LL.Constant)
getInitVal gv isC | isC == False = return Nothing
                  | otherwise    = do cval <- liftIO $ FFI.getInitializer gv
                                      val  <- getConstantValue cval
                                      return $ Just val

getGlobal :: (String, Value) -> Context IO LL.Global
getGlobal (gname, gval) = do link  <- liftIO $ FFI.getLinkage gval
                             isC   <- hasInitializer gval
                             align <- liftIO $ FFI.getAlignment gval
                             unadd <- liftIO $ FFI.hasUnnamedAddr gval
                             let llalign = LL.Align $ fromEnum align
                                 llunadd = cUInt2Bool unadd
                                 lllink  = convertLinkage $ FFI.toLinkage link
                             llival <- getInitVal gval isC
                             ty <- typeOf gval
                             return $ LL.GlobalVar (LL.Global gname) lllink isC llunadd ty llival llalign

-- Functions
getFuns :: Context IO LL.Functions 
getFuns = do e@Env{..} <- getEnv
             funs <- liftIO $ getFunctions mdl
             l <- forM funs getFunction
             return $ Map.fromList l

getFunction :: (String, Value) -> Context IO (String, LL.Function)
getFunction (fname, fval) = do b     <- liftIO $ FFI.isDeclaration fval
                               rty   <- (liftIO $ FFI.getFunctionReturnType fval) >>= getType
                               link  <- liftIO $ FFI.getLinkage fval
                               isVar <- liftIO $ FFI.getFunctionType fval >>= FFI.isFunctionVarArg
                              -- liftIO$ print fname
                               fname' <- liftIO $ demangler fname
                               pars <- liftIO $ getParams fval
                               params <- forM pars getParam
                               let lllink = convertLinkage $ FFI.toLinkage link
                                   isFnVar = cInt2Bool isVar
                               if cInt2Bool b
                               then return (fname', LL.FunctionDecl (LL.Global fname') lllink rty isFnVar params)
                               else do bbs <- liftIO $ getBasicBlocks fval
                                       llbbs <- forM bbs getBasicBlock 
                                       return (fname', LL.FunctionDef (LL.Global fname') lllink rty isFnVar params llbbs)

getParam :: (String, Value) -> Context IO LL.Parameter
getParam (pname, pval) = do ty <- typeOf pval
                            return $ LL.Parameter (LL.Local pname) ty

-- Basic Blocks                                 
getBasicBlock :: (String, Value) -> Context IO LL.BasicBlock
getBasicBlock (bbname, bbvalue) = do instr  <- liftIO $ getInstructions bbvalue
                                     instrs <- forM instr getInst
                                     return $ LL.BasicBlock (LL.Local bbname) instrs
                   
getInst :: (String, Value) -> Context IO LL.Instruction
getInst (_,instrv) = do e@Env{..} <- getEnv
                        putEnv $ e {instr = Just instrv}
                        getInstruction

