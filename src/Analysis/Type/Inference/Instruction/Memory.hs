{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Instruction.Memory
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Constraints Memory Instruction 
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Instruction.Memory where

import Language.LLVMIR hiding (Id)
import Analysis.Type.Inference.Base
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.TyAnn as T

import Analysis.Type.Inference.Value
import Control.Monad.State
import Analysis.Type.Util

import qualified Data.Set as S

-- Type Constraints for Alloca
τℂalloca ∷ Int → Id → Τ → ℂState
τℂalloca pc n τ = do
	let cτρ = ℂτ $ T.TyDer $ T.TyPtr (τ ↑^ T.TyAny) T.TyAny --anyRegAddr) anyRegAddr
	    nℂ = ℂπ n :=: cτρ
	(↣) $ liftΤℂ pc $ nℂ ∘ ε

-- Type Constraints for Store
τℂstore ∷ Int → Τ → Value → Value → ℂState
τℂstore pc τ α β = do
	τℂα ← τℂr α               -- τℂ of value
	τℂβ ← τℂr β               -- τℂ of pointer
	let cτρ = ℂτ $ (↑) τ -- ref τ of value
	    (πα,πβ) = (π α,π β)  -- 
	    τℂ = ℂτ (T.TyPri T.TyVoid) :=: cτρ
	    βℂ = πβ :=: (πα ⤜ T.TyAny) --anyRegAddr)
	    αℂ = πα :=: ℂc T1
	(↣) $ liftΤℂ pc $ τℂ ∘ (βℂ ∘ (τℂα ∪ τℂβ))
--	(↣) $ liftΤℂ pc $ τℂ ∘ (αℂ ∘ (βℂ ∘ (τℂα ∪ τℂβ))) 

-- Type Constraints for Load
τℂload ∷ Int → Id → Value → ℂState
τℂload pc n α = do
	τℂα ← τℂr α               -- τℂ of value
	let πα = π α
	    πn = ℂπ n
	    αℂ = πα :=: (πn ⤜ T.TyAny) --anyRegAddr)
	    nℂ = πn :=: ℂc T1
	(↣) $ liftΤℂ pc $ αℂ ∘ τℂα
	--(↣) $ liftΤℂ pc $ αℂ ∘ (nℂ ∘ τℂα)

-- Type Constraints for GEP
τℂgep ∷ Int → Id → Τ → Value → Values → ℂState
τℂgep pc n τn α δs = do
	τℂα ← τℂr α	
	τℂs ← τListR τℂα δs
	let cτn = ℂτ $ τn ↑^ T.TyAny --anyRegAddr                -- OK
	    πα  = π α
	    cℂ  = ℂp (ℂc TAgg) T.TyAny --anyRegAddr              -- Pointer to agg in reg mem
	    πδs = map π δs
	    δsℂ = S.fromList $ map ((ℂc TInt) :=:) πδs
	    n1ℂ = ℂπ n :=: cτn
	   -- n2ℂ = ℂπ n :=: πgep α δs
	    αℂ  = πα :=: cℂ
	(↣) $ liftΤℂ pc $ n1ℂ ∘ ε
--	(↣) $ n1ℂ ∘ (n2ℂ ∘ (αℂ ∘ (δsℂ ∪ τℂs)))

-- Type contraints for atomic instructions
τℂaop ∷ Int → Id → Value → Values → ℂState
τℂaop pc n α βs = do
--	τℂα ← τℂ α
--	τv ← τList τℂα βs
	let πn = ℂπ n
	    πα = π α
	    nℂ = S.fromList $ map ((πn :=:) . π) βs
	    αℂ = πα :=: (πn ⤜ kLogAddr)
	(↣) $ liftΤℂ pc $ αℂ ∘ ε --(nℂ ∪ τv)
