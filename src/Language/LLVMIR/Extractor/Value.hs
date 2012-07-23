{-#LANGUAGE DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Value
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Value where

import qualified LLVM.FFI.Core as FFI
import LLVM.Core hiding (Value)

import qualified Language.LLVMIR as LL

import Foreign.C.Types
import Foreign.C.String

import Language.LLVMIR.Extractor.Type
import Language.LLVMIR.Extractor.Util
import Language.LLVMIR.Extractor.Ident
import Language.LLVMIR.Extractor.Opcode
import Language.LLVMIR.Extractor.ConstantClass 

getValue :: (String, Value) -> IO LL.Value
getValue (n, v) = do isC <- (FFI.isConstant v)
                     if cInt2Bool isC
                     then getConstantValue v
                     else getIdentValue n v

getIdentValue :: String -> Value -> IO LL.Value
getIdentValue n v = do ty <- typeOf v 
                       return $ LL.Id (LL.Local n) ty

-- Constants
getConstantValue :: Value -> IO LL.Value
getConstantValue v = do vc <- FFI.getConstantClass v
                        let constant = case toConstantClass vc of
                             BlockAddr              -> getBlockAddr v
                             ConstantAggregateZero  -> getConstantAggregateZero v
                             ConstantArray          -> getConstantArray v
                             ConstantDataSequential -> (getConstantDataSequential v >>= (return . LL.ConstantDataSequential))
                             ConstantExpr           -> getConstantExpr v
                             ConstantFP             -> getConstantFP v
                             ConstantInt            -> getConstantInt v
                             ConstantPointerNull    -> getConstantPointerNull v
                             ConstantStruct         -> getConstantStruct v
                             ConstantVector         -> getConstantVector v
                             GlobalValue            -> (getGlobalValue v >>= (return . LL.GlobalValue))
                             UndefValue             -> return $ LL.UndefValue
                        constant >>= return . LL.Constant

getBlockAddr :: Value -> IO LL.Constant
getBlockAddr = error "TODO getBlockAddr"

getConstantAggregateZero :: Value -> IO LL.Constant
getConstantAggregateZero v = do ty <- typeOf v
                                return $ LL.ConstantAggregateZero ty 

getConstantArray :: Value -> IO LL.Constant
getConstantArray = error "TODO getConstantArray"

getConstantDataSequential :: Value -> IO LL.ConstantDataSequential
getConstantDataSequential v = do vc  <- FFI.getConstantDataSequentialClass v
                                 val <- FFI.constantValueGetAsString v >>= peekCString
                                 num <- FFI.constantValueGetNumElem v >>= (return . fromEnum)
                                 ety <- FFI.constantValueGetElemType v
                                 ty' <- getType ety
                                 case toConstantDataSequentialClass vc of
                                   ConstantDataArray  -> return $ LL.ConstantDataArray  (LL.TyArray  num ty') val       
                                   ConstantDataVector -> return $ LL.ConstantDataVector (LL.TyVector num ty') val

-- TODO: | UnaryConstantExpr
getConstantExpr :: Value -> IO LL.Constant
getConstantExpr v = do opcode <- (FFI.getConstOpcode v) >>= (return . fromIntegral)
                       let op = toOpcode opcode
                           expr = case opcodeClass opcode of
                              Terminator -> error $ "'getConstantExpr': Terminator Class" 
                              Binary     -> return LL.BinaryConstantExpr
                              Logical    -> return LL.BinaryConstantExpr 
                              Memory     -> case op of
                                GetElementPtr -> getElementPtrConstant v
                                _             -> error $ "'getConstantExpr': Memory Class" 
                              Cast       -> error $ "'getConstantExpr': Cast Class"  
                              Other      -> case op of
                                ICmp           -> return LL.CompareConstantExpr
                                FCmp           -> return LL.CompareConstantExpr
                                Select         -> return LL.SelectConstantExpr
                                ExtractElement -> return LL.ExtractElementConstantExpr
                                InsertElement  -> return LL.InsertElementConstantExpr
                                ShuffleVector  -> return LL.ShuffleVectorConstantExpr
                                ExtractValue   -> return LL.ExtractValueConstantExpr
                                InsertValue    -> return LL.InsertValueConstantExpr 
                                _              -> error $ "'getConstantExpr': Other Class"
                       expr >>= (return . LL.ConstantExpr) -- $ error "TODO getConstantExpr"
{-do ty <- (FFI.typeOf v) >>= getType
                       (struct, args) <- (getOperands v) >>= getElemPtrArgs
                       return $ undefined -- LL.Pointer ty struct args
-}

getElementPtrConstant :: Value -> IO LL.ConstantExpr
getElementPtrConstant v = do op <- (getOperands v) >>= mapM getValue
                             return $ LL.GetElementPtrConstantExpr (head op) (tail op)

getConstantFP :: Value -> IO LL.Constant
getConstantFP = error "TODO getConstantFP"

getConstantInt :: Value -> IO LL.Constant
getConstantInt v = do ty <- typeOf v 
                      av <- FFI.constIntGetSExtValue v
                      return $ LL.ConstantInt (fromIntegral av) ty

getConstantPointerNull :: Value -> IO LL.Constant
getConstantPointerNull v = do ty <- typeOf v
                              return $ LL.ConstantPointerNull ty 

getConstantStruct :: Value -> IO LL.Constant
getConstantStruct = error "TODO getConstantStruct"

getConstantVector :: Value -> IO LL.Constant
getConstantVector = error "TODO getConstantVector"

-- | Retrieves a Constant Global Value
getGlobalValue :: Value -> IO LL.GlobalValue
getGlobalValue v = do ty <- typeOf v
                      i  <- getIdent v
                      gb <- FFI.getGlobalValueClass v
                      case toGlobalValueClass gb of
                           FunctionValue  -> return $ LL.FunctionValue (LL.Global i) ty
                           GlobalAlias    -> error "TODO GlobalAlias"
                           GlobalVariable -> return $ LL.GlobalVariable (LL.Global i) ty

