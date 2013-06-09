{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Global
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Constraints Global
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Global where

import Language.LLVMIR

import Analysis.Type.Inference.Base
import Analysis.Type.Inference.Value

import Analysis.Type.Memory.Util

instance TyConstr Global where
	-- τℂ ∷ → Global → State Γ (S.Set Τℂ)
	τℂ (GlobalVar i _ False _ τ Nothing   _) = (↣) $ (ℂπ i) :=: (ℂτ $ (↑)τ) ∘ ε
	τℂ (GlobalVar i _ True  _ τ (Just cn) _) = 
		let τα1 = (↑)τ 
		    τα2 = π cn
		    c1 = (ℂπ i) :=: (ℂτ τα1)
		    c2 = (ℂτ τα1) :=: τα2
		in (↣) $ c1 ∘ (c2 ∘ ε)
	τℂ gv = error $ "τℂ(2): " ++ show gv
