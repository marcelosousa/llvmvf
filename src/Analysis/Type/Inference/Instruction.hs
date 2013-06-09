{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Instruction
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Constraints Instruction 
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Instruction where

import Language.LLVMIR hiding (Id)
import Analysis.Type.Inference.Base
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.TyAnn as T
import Analysis.Type.Util

import Analysis.Type.Inference.Value
import Analysis.Type.Inference.Instruction.Memory

import Prelude.Unicode ((⧺))
import Control.Monad.State

import qualified Data.Set as S

instance TyConstr PHI where
	-- τℂ ∷ → PHI → State Γ (S.Set Τℂ)
	τℂ (PHI _ n τ v) = do
		let τα = (↑)τ                     -- lift type
		    nℂ = (ℂπ n) :=: (ℂτ τα) ∘ ε   -- constraint the id to type τα
		    vi  = fst $ unzip v           -- get values
		    viℂ = S.fromList $ map π vi   -- compute elementary constraints
		    aℂs = S.map ((ℂπ n) :=:) viℂ  -- constraint the current name to viℂ
		τList (nℂ ∪ aℂs) vi

instance TyConstr Terminator where
	-- τℂ ∷ → Terminator → State Γ (S.Set Τℂ)
	τℂ tmn = do
		fn ← δfn
		bb ← δbb
		case tmn of
			Ret _ VoidRet → do
				let τα = T.TyPri T.TyVoid
				    fnℂ = (ℂπ fn) :=: (ℂτ τα)
				    bbℂ = (ℂπ bb) :=: (ℂτ τα)
				(↣) $ fnℂ ∘ (bbℂ ∘ ε)
			Ret _ (ValueRet v) → do
				τℂv ← τℂ v
				let πv = π v
				    fnℂ = (ℂπ fn) :=: πv
				    bbℂ = (ℂπ bb) :=: πv
				(↣) $ fnℂ ∘ (bbℂ ∘ τℂv)
			Unreachable _ → do
				let τα = T.TyUndef 
				    fnℂ = (ℂπ fn) :=: (ℂτ τα)
				    bbℂ = (ℂπ bb) :=: (ℂτ τα)
				(↣) $ fnℂ ∘ (bbℂ ∘ ε)
			Br _ c t f → do
				τℂv ← τList ε [c,t,f]
				let (πc,πt,πf) = (π c,π t,π f)
				    cℂ = πc :=: (ℂτ $ T.TyPri $ T.TyInt 1)
				    tfℂ = πt :=: πf
				    fnℂ = (ℂπ fn) :=: πt
				    bbℂ = (ℂπ bb) :=: πt
				(↣) $ cℂ ∘ (tfℂ ∘ (fnℂ ∘ (bbℂ ∘ τℂv)))
			UBr _ d → do
				τℂd ← τℂ d
				let πd = π d
				    fnℂ = (ℂπ fn) :=: πd
				    bbℂ = (ℂπ bb) :=: πd
				(↣) $ fnℂ ∘ (bbℂ ∘ τℂd)
			_ → error $ show tmn ⧺ " not supported"

instance TyConstr Instruction where
	τℂ i = case i of
	-- Standard Binary Operations
	-- Integer Operations
 		Add  _ n τ α β → τℂbin ΤInt n τ α β 
 		Sub  _ n τ α β → τℂbin ΤInt n τ α β  
 		Mul  _ n τ α β → τℂbin ΤInt n τ α β  
 		UDiv _ n τ α β → τℂbin ΤInt n τ α β  
 		SDiv _ n τ α β → τℂbin ΤInt n τ α β  
 		URem _ n τ α β → τℂbin ΤInt n τ α β  
 		SRem _ n τ α β → τℂbin ΤInt n τ α β
 	-- Bitwise Binary Operations
		Shl  _ n τ α β → τℂbin ΤInt n τ α β 
		LShr _ n τ α β → τℂbin ΤInt n τ α β 
		AShr _ n τ α β → τℂbin ΤInt n τ α β 
		And  _ n τ α β → τℂbin ΤInt n τ α β 
		Or   _ n τ α β → τℂbin ΤInt n τ α β 
		Xor  _ n τ α β → τℂbin ΤInt n τ α β 
    -- Float Operations
 		FAdd _ n τ α β → τℂbin ΤFlt n τ α β
 		FSub _ n τ α β → τℂbin ΤFlt n τ α β
 		FMul _ n τ α β → τℂbin ΤFlt n τ α β
 		FDiv _ n τ α β → τℂbin ΤFlt n τ α β
 		FRem _ n τ α β → τℂbin ΤFlt n τ α β
    -- Cast Operations
		Trunc    _ n α τ → τℂcast n (α,ΤInt) (τ,ΤInt) (>:)  -- Truncate integers
		ZExt     _ n α τ → τℂcast n (α,ΤInt) (τ,ΤInt) (:<:) -- Zero extend integers
		SExt     _ n α τ → τℂcast n (α,ΤInt) (τ,ΤInt) (:<:) -- Sign extend integers
		FPTrunc  _ n α τ → τℂcast n (α,ΤFlt) (τ,ΤFlt) (>:)  -- Truncate floating point
		FPExt    _ n α τ → τℂcast n (α,ΤFlt) (τ,ΤFlt) (:≤:) -- Extend floating point
		FPToUI   _ n α τ → τℂcast n (α,ΤFlt) (τ,ΤInt) (:=:) -- floating point → UInt
		FPToSI   _ n α τ → τℂcast n (α,ΤFlt) (τ,ΤInt) (:=:) -- floating point → SInt
		UIToFP   _ n α τ → τℂcast n (α,ΤInt) (τ,ΤFlt) (:=:) -- UInt → floating point
		SIToFP   _ n α τ → τℂcast n (α,ΤInt) (τ,ΤFlt) (:=:) -- SInt → floating point
		PtrToInt _ n α τ → τℂcast n (α,ΤPtr) (τ,ΤInt) (:=:) -- Pointer → integer 
		IntToPtr _ n α τ → τℂcast n (α,ΤInt) (τ,ΤPtr) (:=:) -- integer → Pointer
		BitCast  _ n α τ → τℂcast n (α,Τ1NA) (τ,Τ1NA) (:≌:) -- 1stclass non agg → 1stclass non agg
    -- Comparison Operations
		ICmp _ n _ τ α β → τℂcmp ΤInt n τ α β
		FCmp _ n _ τ α β → τℂcmp ΤFlt n τ α β 
    -- Memory Operations
		Alloca _ n τ   _ → τℂalloca n τ
		Store  _ τ α β _ → τℂstore τ α β
		Load   _ n α   _ → τℂload n α
		GetElementPtr _ n τ α δs → τℂgep n τ α δs
    -- Atomic Operations
  		Cmpxchg   _ n α β η _ → τℂaop n α [β,η]
		AtomicRMW _ n α β _ _ → τℂaop n α [β]
    -- Call
		Call _ n τ c χ → τℂcall n τ c χ
    -- Vector Operations
		Select       _ n α β η  → error "vector operations not supported" 
		ExtractValue _ n α δs   → error "vector operations not supported"
		InsertValue  _ n α β δs → error "vector operations not supported"

-- Type Constraints for Binary Operations
τℂbin ∷ ΤClass → Id → Τ → Value → Value → State Γ (S.Set Τℂ)
τℂbin τc n τ α β = do
	τℂα ← τℂ α
	τℂβ ← τℂ β
	let cτρ = ℂτ $ (↑)τ
	    (πα,πβ) = (π α,π β)
	    αℂ = cτρ :=: πα
	    βℂ = cτρ :=: πβ
	    αβℂ = πα :=: πβ
	    cℂ = cτρ :=: (ℂc τc)
	    nℂ = (ℂπ n) :=: cτρ
	(↣) $ nℂ ∘ (αℂ ∘ (βℂ ∘ (αβℂ ∘ (cℂ ∘ (τℂα ∪ τℂβ)))))

-- Type Constraints for Cast Operations
τℂcast ∷ Id → (Value, ΤClass) → (Τ, ΤClass) → (ℂ → ℂ → Τℂ) → State Γ (S.Set Τℂ)
τℂcast n (α,τcα) (τ,τcτ) (?:) = do
	τℂα ← τℂ α
	let cτρ = ℂτ $ (↑)τ
	    πα = π α
	    cℂα = πα :=: (ℂc τcα)
	    cℂτ = cτρ :=: (ℂc τcτ)
	    αℂ = πα ?: cτρ
	    nℂ = (ℂπ n) :=: cτρ
	(↣) $ nℂ ∘ (αℂ ∘ (cℂτ ∘ (cℂα ∘ τℂα)))

-- Type Constraints for comparison operations
τℂcmp ∷ ΤClass → Id → Τ → Value → Value → State Γ (S.Set Τℂ)
τℂcmp τc n τ α β = do
	τℂα ← τℂ α
	τℂβ ← τℂ β
	let cτρ = ℂτ $ (↑)τ
	    (πα,πβ) = (π α,π β)
	    αℂ = cτρ :=: πα
	    βℂ = cτρ :=: πβ
	    αβℂ = πα :=: πβ
	    cℂ = cτρ :=: (ℂc τc)
	    cτn = ℂτ $ T.TyPri $ T.TyInt 1 
	    nℂ = (ℂπ n) :=: cτn
	(↣) $ nℂ ∘ (αℂ ∘ (βℂ ∘ (αβℂ ∘ (cℂ ∘ (τℂα ∪ τℂβ)))))

τℂcall ∷ Id → Τ → Id → Values → State Γ (S.Set Τℂ)
τℂcall n τ c χ = do
	τℂχ ← τList ε χ
	let (πn,πc) = (ℂπ n,ℂπ c)
	    cτρ = ℂτ $ (↑)τ       -- OK
	    πχ = map π χ
	    nℂ = (ℂπ n) :=: cτρ   -- OK
	    ς  = ℂp (ℂλ πχ cτρ) T.TyRegAddr
	    cℂ = πc :=: ς
	(↣) $ nℂ ∘ (cℂ ∘ τℂχ)