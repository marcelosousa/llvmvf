{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Function
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Constraints Function 
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Function where

import Language.LLVMIR

import Analysis.Type.Inference.Base
import Analysis.Type.Inference.Instruction

import Analysis.Type.Memory.Util

instance TyConstr Function where
	-- τℂ ∷ → Function → State Γ (S.Set Τℂ)
	τℂ (FunctionDef  n _ τ _ ς bbs) = do
		let τα = (↑)τ
		    nℂ = (ℂπ n) :⊑: (ℂτ τα) ∘ ε
		ςℂs ← τList nℂ ς
		νfn n
		τList ςℂs bbs
	τℂ (FunctionDecl _ _ _ _ _) = (↣) ε

instance TyConstr Parameter where
	τℂ (Parameter n τ) = do
		let τα = (↑)τ
		    nℂ = (ℂπ n) :⊑: (ℂτ τα) ∘ ε
		(↣) nℂ 

instance TyConstr BasicBlock where
	τℂ (BasicBlock n φs is tmn) = do
		νbb n 
		φℂs   ← τList ε φs
		isℂs  ← τList φℂs is
		tmnℂs ← τℂ tmn
		(↣) $ isℂs ∪ tmnℂs
