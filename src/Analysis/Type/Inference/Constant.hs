{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Constant
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Inference Constant
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Constant where

import Language.LLVMIR
import Analysis.Type.Inference.Base
import Analysis.Type.Memory.Util

instance Constr Constant where
	π = undefined