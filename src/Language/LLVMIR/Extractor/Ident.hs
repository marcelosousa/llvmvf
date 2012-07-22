-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Ident
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Ident where

import qualified LLVM.FFI.Core as FFI

import Language.LLVMIR.Extractor.Util

import Foreign.C.String

getIdent :: Value -> IO String
getIdent v = (FFI.getValueName v) >>= peekCString
