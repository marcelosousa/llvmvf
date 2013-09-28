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
import Analysis.Type.Memory.TyAnn as T

instance TyConstr Global where
	-- τℂ ∷ → Global → State Γ (S.Set Τℂ)
	τℂ (GlobalVar i _ False _ τ Nothing   _) = (↣) $ liftΤℂ 0 $ (ℂπ i) :=: (ℂτ $ (↑)τ) ∘ ε
	τℂ (GlobalVar i _ True  _ τ (Just cn) _) = 
		let τα1 = (↑)τ 
		    τα2 = π cn
		    τcn = case τα2 of
		    	ℂτ τn → ℂτ $ TyDer $ T.TyPtr τn T.AnyAddr
		    	_     → τα2
		    c1 = (ℂπ i) :=: (ℂτ τα1)
		    c2 = (ℂτ τα1) :=: τcn
		in (↣) $ liftΤℂ 0 $ c1 ∘ (c2 ∘ ε)
	τℂ gv = error $ "τℂ(2): " ++ show gv
