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
import Analysis.Type.Inference.Initial

import Analysis.Type.Memory.Util
import Analysis.Type.Memory.TyAnn

instance TyConstr Function where
	-- τℂ ∷ → Function → State Γ (S.Set Τℂ)
	τℂ (FunctionDef  n _ τ _ ς bbs) = do
		let πτ = ℂτ $ (↑)τ
		    πς = map π ς
		    πn = ℂp (ℂλ πς πτ) AnyAddr
		    nℂ = liftΤℂ 0 $ (ℂπ n) :=: πn ∘ ε
		ςℂs ← τList nℂ ς
		νfn (n,πς)
		τList ςℂs bbs
	τℂ (FunctionDecl n _ τ _ ς) = (↣) ε 

instance TyConstr Parameter where
	τℂ (Parameter n τ) = do
		let τα = (↑)τ
		    nℂ = (ℂπ n) :=: (ℂτ τα) ∘ ε
		(↣) $ liftΤℂ 0 nℂ 

instance Constr Parameter where
	π (Parameter n τ) = ℂπ n

instance TyConstr BasicBlock where
	τℂ (BasicBlock n φs is tmn) = do
		νbb n 
		φℂs   ← τList ε φs
		isℂs  ← τList φℂs is
		tmnℂs ← τℂ tmn
		(↣) $ isℂs ∪ tmnℂs ∪ (liftΤℂ 0 iτℂ)
