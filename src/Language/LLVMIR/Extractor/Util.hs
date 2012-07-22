-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Util
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Util where

import qualified LLVM.FFI.Core as FFI
import LLVM.Core hiding (Value)

import Foreign.C.Types

type Value = FFI.ValueRef

cUInt2Bool :: CUInt -> Bool
cUInt2Bool 0 = False 
cUInt2Bool _ = True

cInt2Bool :: CInt -> Bool
cInt2Bool 0 = False
cInt2Bool _ = True

