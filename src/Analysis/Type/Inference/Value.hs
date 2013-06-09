{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Value
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Constraints for Value and Constants
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Value where

import Language.LLVMIR
import Analysis.Type.Inference.Base
import Analysis.Type.Memory.Util

-- Type Constraints for values
instance TyConstr Value where
	τℂ (Id n τ) = do
		let τα = (↑)τ
		    nℂ = (ℂπ n) :=: (ℂτ τα) ∘ ε
		(↣) nℂ
	τℂ c = (↣) ε

instance Constr Value where
	π (Id n τ) = ℂπ n
	π (Constant c) = π c

instance Constr Constant where
	π = undefined