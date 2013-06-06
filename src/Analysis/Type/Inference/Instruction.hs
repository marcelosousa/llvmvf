{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Instruction
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Constraints Instruction 
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Instruction where

import Language.LLVMIR 
import Analysis.Type.Inference.Base
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.TyAnn as T

import Analysis.Type.Inference.Constant
import Prelude.Unicode ((⧺))
import Control.Monad.State
import Analysis.Type.Util

import qualified Data.Set as S

instance TyConstr PHI where
	-- τℂ ∷ → PHI → State Γ (S.Set Τℂ)
	τℂ (PHI _ n τ v) = do
		let τα = (↑)τ                     -- lift type
		    nℂ = (ℂπ n) :⊑: (ℂτ τα) ∘ ε   -- constraint the id to type τα
		    vi  = fst $ unzip v           -- get values
		    viℂ = S.fromList $ map π vi   -- compute elementary constraints
		    aℂs = S.map ((ℂπ n) :⊑:) viℂ  -- constraint the current name to viℂ
		τList (nℂ ∪ aℂs) vi

instance TyConstr Terminator where
	-- τℂ ∷ → Terminator → State Γ (S.Set Τℂ)
	τℂ tmn = do
		fn ← δfn
		bb ← δbb
		case tmn of
			Ret _ VoidRet → do
				let τα = T.TyPri T.TyVoid
				    fnℂ = (ℂπ fn) :⊑: (ℂτ τα)
				    bbℂ = (ℂπ bb) :⊑: (ℂτ τα)
				(↣) $ fnℂ ∘ (bbℂ ∘ ε)
			Ret _ (ValueRet v) → do
				τℂv ← τℂ v
				let πv = π v
				    fnℂ = (ℂπ fn) :⊑: πv
				    bbℂ = (ℂπ bb) :⊑: πv
				(↣) $ fnℂ ∘ (bbℂ ∘ τℂv)
			Unreachable _ → do
				let τα = T.TyUndef 
				    fnℂ = (ℂπ fn) :⊑: (ℂτ τα)
				    bbℂ = (ℂπ bb) :⊑: (ℂτ τα)
				(↣) $ fnℂ ∘ (bbℂ ∘ ε)
			Br _ c t f → do
				τℂv ← τList ε [c,t,f]
				let (πc,πt,πf) = (π c,π t,π f)
				    cℂ = πc :⊑: (ℂτ $ T.TyPri $ T.TyInt 1)
				    tfℂ = πt :⊑: πf
				    fnℂ = (ℂπ fn) :⊑: πt
				    bbℂ = (ℂπ bb) :⊑: πt
				(↣) $ cℂ ∘ (tfℂ ∘ (fnℂ ∘ (bbℂ ∘ τℂv)))
			UBr _ d → do
				τℂd ← τℂ d
				let πd = π d
				    fnℂ = (ℂπ fn) :⊑: πd
				    bbℂ = (ℂπ bb) :⊑: πd
				(↣) $ fnℂ ∘ (bbℂ ∘ τℂd)
			_ → error $ show tmn ⧺ " not supported"

instance TyConstr Instruction where
	τℂ i = case i of
	-- Standard Binary Operations
	-- Integer Operations
 		Add  _ n τ α β → τℂbin TyClassInt n τ α β 
 		Sub  _ n τ α β → τℂbin TyClassInt n τ α β  
 		Mul  _ n τ α β → τℂbin TyClassInt n τ α β  
 		UDiv _ n τ α β → τℂbin TyClassInt n τ α β  
 		SDiv _ n τ α β → τℂbin TyClassInt n τ α β  
 		URem _ n τ α β → τℂbin TyClassInt n τ α β  
 		SRem _ n τ α β → τℂbin TyClassInt n τ α β
 	-- Bitwise Binary Operations
		Shl  _ n τ α β → τℂbin TyClassInt n τ α β 
		LShr _ n τ α β → τℂbin TyClassInt n τ α β 
		AShr _ n τ α β → τℂbin TyClassInt n τ α β 
		And  _ n τ α β → τℂbin TyClassInt n τ α β 
		Or   _ n τ α β → τℂbin TyClassInt n τ α β 
		Xor  _ n τ α β → τℂbin TyClassInt n τ α β 
    -- Float Operations
 		FAdd _ n τ α β → τℂbin TyClassFloat n τ α β
 		FSub _ n τ α β → τℂbin TyClassFloat n τ α β
 		FMul _ n τ α β → τℂbin TyClassFloat n τ α β
 		FDiv _ n τ α β → τℂbin TyClassFloat n τ α β
 		FRem _ n τ α β → τℂbin TyClassFloat n τ α β


τℂbin ∷ TyClass → Identifier → Τ → Value → Value → State Γ (S.Set Τℂ)
τℂbin τc n τ α β = do
	τℂα ← τℂ α
	τℂβ ← τℂ β
	let cτρ = ℂτ $ (↑)τ
	    (πα,πβ) = (π α,π β)
	    αℂ = cτρ :⊑: πα
	    βℂ = cτρ :⊑: πβ
	    αβℂ = πα :⊑: πβ
	    cℂ = cτρ :⊑: (ℂc τc) 
	(↣) $ αℂ ∘ (βℂ ∘ (αβℂ ∘ (cℂ ∘ (τℂα ∪ τℂβ))))

-- Type Constraints for values
instance TyConstr Value where
	τℂ (Id n τ) = do
		let τα = (↑)τ
		    nℂ = (ℂπ n) :⊑: (ℂτ τα) ∘ ε
		(↣) nℂ
	τℂ c = (↣) ε

instance Constr Value where
	π (Id n τ) = ℂπ n
	π (Constant c) = π c
