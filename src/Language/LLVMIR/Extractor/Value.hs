{-#LANGUAGE DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Value
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Value where

import qualified LLVM.FFI.Core as FFI
import LLVM.Core hiding (Value)

import Foreign.C.String
import Foreign.C.Types

import qualified Language.LLVMIR as LL

import Language.LLVMIR.Extractor.ConstantClass
import Language.LLVMIR.Extractor.Type
import Language.LLVMIR.Extractor.Util
import Language.LLVMIR.Extractor.Ident

getValue :: (String, Value) -> IO LL.Value
getValue (n, v) = do isC <- (FFI.isConstant v)
                     if cInt2Bool isC
                     then getConstantValue v
                     else getIdentValue n v

getIdentValue :: String -> Value -> IO LL.Value
getIdentValue n v = do ty <- (FFI.typeOf v) >>= getType
                       return $ LL.Id (LL.Local n) ty

-- Constants
getConstantValue :: Value -> IO LL.Value
getConstantValue v = do vc <- FFI.getConstantClass v
                        let constant = case toConstantClass vc of
                             BlockAddr              -> getBlockAddr v
                             ConstantAggregateZero  -> getConstantAggregateZero v
                             ConstantArray          -> getConstantArray v
                             ConstantDataSequential -> getConstantDataSequential v
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
getConstantAggregateZero = error "TODO getConstantAggregateZero"

getConstantArray :: Value -> IO LL.Constant
getConstantArray = error "TODO getConstantArray"

getConstantDataSequential :: Value -> IO LL.Constant
getConstantDataSequential = error "TODO getConstantDataSequential"

getConstantExpr :: Value -> IO LL.Constant
getConstantExpr v = error "TODO getConstantExpr" 
{-do ty <- (FFI.typeOf v) >>= getType
                       (struct, args) <- (getOperands v) >>= getElemPtrArgs
                       return $ undefined -- LL.Pointer ty struct args
-}
getConstantFP :: Value -> IO LL.Constant
getConstantFP = error "TODO getConstantFP"

getConstantInt :: Value -> IO LL.Constant
getConstantInt v = do ty <- (FFI.typeOf v) >>= getType
                      av <- FFI.constIntGetSExtValue v
                      return $ LL.ConstantInt (fromIntegral av) ty

getConstantPointerNull :: Value -> IO LL.Constant
getConstantPointerNull = error "TODO getConstantPointerNull"

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

