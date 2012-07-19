module Language.LLVMIR.Util where

import Foreign.C.String
import Foreign.C.Types

cUInt2Bool :: CUInt -> Bool
cUInt2Bool 0 = False 
cUInt2Bool _ = True

cInt2Bool :: CInt -> Bool
cInt2Bool 0 = False
cInt2Bool _ = True
