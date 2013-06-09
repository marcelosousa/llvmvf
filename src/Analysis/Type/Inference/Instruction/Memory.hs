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
τℂalloca ∷ Id → Τ → State Γ (S.Set Τℂ)
τℂalloca n τ = do
	let cτρ = ℂτ $ T.TyDer $ T.TyPtr (τ ↑^ T.TyRegAddr) T.TyRegAddr
	    nℂ = (ℂπ n) :=: cτρ
	(↣) $ nℂ ∘ ε

-- Type Constraints for Store
τℂstore ∷ Τ → Value → Value → State Γ (S.Set Τℂ)
τℂstore τ α β = do
	τℂα ← τℂ α               -- τℂ of value
	τℂβ ← τℂ β               -- τℂ of pointer
	let cτρ = ℂτ $ (↑)τ      -- ref τ of value
	    (πα,πβ) = (π α,π β)  -- 
	    αℂ = πα :=: cτρ
	    βℂ = πβ :=: (πα ⤜ T.TyRegAddr)
	    cℂ = πα :=: (ℂc Τ1)
	(↣) $ αℂ ∘ (βℂ ∘ (cℂ ∘ (τℂα ∪ τℂβ)))

-- Type Constraints for Load
τℂload ∷ Id → Value → State Γ (S.Set Τℂ)
τℂload n α = do
	τℂα ← τℂ α               -- τℂ of value
	let πα = π α
	    πn = ℂπ n
	    αℂ = πα :=: (πn ⤜ T.TyRegAddr)
	    nℂ = πn :=: (ℂc Τ1)
	(↣) $ αℂ ∘ (nℂ ∘ τℂα)

-- Type Constraints for GEP
τℂgep ∷ Id → Τ → Value → Values → State Γ (S.Set Τℂ)
τℂgep n τ α δs = do
	τℂα ← τℂ α
	τℂs ← τList τℂα δs
	let cτρ = ℂτ $ τ ↑^ T.TyRegAddr                 -- OK
	    πα  = π α
	    cℂ  = ℂp (ℂc ΤAgg) T.TyRegAddr              -- Pointer to agg in reg mem
	    πδs = map π δs
	    δsℂ = S.fromList $ map ((ℂc ΤInt) :=:) πδs
	    α1ℂ = πα :=: cτρ
	    α2ℂ = πα :=: cℂ
	    πn  = ℂπ n
	    (δi:δr) = map getIntValue δs
	    πδi = ℂι πα δi
	    nℂ  = πn :=: foldr (\δ πδ → ℂι πδ δ) πδi δr
	(↣) $ nℂ ∘ (α1ℂ ∘ (α2ℂ ∘ (δsℂ ∪ τℂs)))

-- Type contraints for atomic instructions
τℂaop ∷ Id → Value → Values → State Γ (S.Set Τℂ)
τℂaop n α βs = do
	τℂα ← τℂ α
	τv ← τList τℂα βs
	let πn = ℂπ n
	    πα = π α
	    nℂ = S.fromList $ map ((πn :=:) . π) βs
	    αℂ = πα :=: (πn ⤜ T.TyRegAddr)
	(↣) $ αℂ ∘ (nℂ ∪ τv)
