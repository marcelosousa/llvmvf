{-# LANGUAGE ForeignFunctionInterface #-}
module Util.Demangler where

-- we need CDouble for C's double type; Haskell's Double may be different
import Foreign.C.String (CString, peekCString, newCString)
import Data.Char (isAlphaNum)
-- we need function pointer type and free function
--import Foreign.Ptr(FunPtr, freeHaskellFunPtr)

-- a "wrapper" import gives a factory for converting a Haskell function to a foreign function pointer
--foreign import ccall "wrapper"
--  wrap :: (CDouble -> CDouble) -> IO (FunPtr (CDouble -> CDouble))

-- import the foreign function as normal
foreign import ccall unsafe "demangler.h _Z9demanglerPc"
  cppdemangler :: CString -> IO CString 

demangler :: String -> IO String
demangler "_ZN3tlm9tlm_arrayIPNS_18tlm_extension_baseEE6expandEj5_" = return "_ZN3tlm9tlm_arrayIPNS_18tlm_extension_baseEE6expandEj5_"
demangler "_ZN9tlm_utils20simple_target_socketI6MemoryLj32EN3tlm23tlm_base_protocol_typesEED2Ev42_" = return "_ZN9tlm_utils20simple_target_socketI6MemoryLj32EN3tlm23tlm_base_protocol_typesEED2Ev42_"
demangler s@('_':'Z':xs) = do ncs <- newCString s 
                              c   <- cppdemangler ncs
                              cs  <- peekCString c
                              return cs    
demangler s = return s       
