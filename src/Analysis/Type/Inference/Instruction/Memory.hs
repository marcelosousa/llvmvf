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
τℂalloca ∷ Id → Τ → ℂState
τℂalloca n τ = do
	let cτρ = ℂτ $ T.TyDer $ T.TyPtr (τ ↑^ T.TyRegAddr) T.TyRegAddr
	    nℂ = ℂπ n :=: cτρ
	(↣) $ nℂ ∘ ε

-- Type Constraints for Store
τℂstore ∷ Τ → Value → Value → ℂState
τℂstore τ α β = do
--	τℂα ← τℂ α               -- τℂ of value
--	τℂβ ← τℂ β               -- τℂ of pointer
	let cτρ = ℂτ $ (↑)τ      -- ref τ of value
	    (πα,πβ) = (π α,π β)  -- 
	    τℂ = ℂτ (T.TyPri T.TyVoid) :=: cτρ
	    βℂ = πβ :=: (πα ⤜ T.TyRegAddr)
	    αℂ = πα :=: ℂc T1
	(↣) $ τℂ ∘ (αℂ ∘ (βℂ ∘ ε)) -- (τℂα ∪ τℂβ)))

-- Type Constraints for Load
τℂload ∷ Id → Value → ℂState
τℂload n α = do
	τℂα ← τℂ α               -- τℂ of value
	let πα = π α
	    πn = ℂπ n
	    αℂ = πα :=: (πn ⤜ T.TyRegAddr)
	    nℂ = πn :=: ℂc T1
	(↣) $ αℂ ∘ (nℂ ∘ τℂα)

-- Type Constraints for GEP
τℂgep ∷ Id → Τ → Value → Values → ℂState
τℂgep n τn α δs = do
	τℂα ← τℂ α
	τℂs ← τList τℂα δs
	let cτn = ℂτ $ τn ↑^ T.TyRegAddr                -- OK
	    πα  = π α
	    cℂ  = ℂp (ℂc TAgg) T.TyRegAddr              -- Pointer to agg in reg mem
	    πδs = map π δs
	    δsℂ = S.fromList $ map ((ℂc TInt) :=:) πδs
	    n1ℂ = ℂπ n :=: cτn
	    n2ℂ = ℂπ n :=: πgep α δs
	    αℂ  = πα :=: cℂ
	(↣) $ n1ℂ ∘ (n2ℂ ∘ (αℂ ∘ (δsℂ ∪ τℂs)))

-- Type contraints for atomic instructions
τℂaop ∷ Id → Value → Values → ℂState
τℂaop n α βs = do
--	τℂα ← τℂ α
--	τv ← τList τℂα βs
	let πn = ℂπ n
	    πα = π α
	    nℂ = S.fromList $ map ((πn :=:) . π) βs
	    αℂ = πα :=: (πn ⤜ T.TyRegAddr)
	(↣) $ αℂ ∘ ε --(nℂ ∪ τv)
