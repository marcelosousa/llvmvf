{-#LANGUAGE DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Value
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Value where

import qualified LLVM.FFI.Core as FFI

import qualified Language.LLVMIR as LL

import Language.LLVMIR.Extractor.Constant
import Language.LLVMIR.Extractor.Type
import Language.LLVMIR.Extractor.Util

getValue :: (String, Value) -> IO LL.Value
getValue (n, v) = do isC <- (FFI.isConstant v)
                     if cInt2Bool isC
                     then getConstantValue v
                     else getIdentValue n v

getIdentValue :: String -> Value -> IO LL.Value
getIdentValue n v = do ty <- typeOf v 
                       return $ LL.Id (LL.Local n) ty


