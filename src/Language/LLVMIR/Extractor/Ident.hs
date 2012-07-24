-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Ident
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Ident where

import Control.Monad.IO.Class (liftIO)

import qualified LLVM.FFI.Core as FFI

import Language.LLVMIR.Extractor.Util
import Language.LLVMIR.Extractor.Context

import Foreign.C.String

getIdent :: Value -> Context IO String
getIdent v = liftIO $ (FFI.getValueName v) >>= peekCString
