{-# LANGUAGE UnicodeSyntax, DoAndIfThenElse #-}
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
import Data.List.Unicode ((∈))
import Control.Monad.State

import qualified Data.Set as S

instance TyConstr PHI where
	-- τℂ ∷ → PHI → ℂState
	τℂ (PHI _ n τ v) = do
		let τα = (↑)τ                     -- lift type
		    nℂ = (ℂπ n) :=: (ℂτ τα) ∘ ε   -- constraint the id to type τα
		    (vi,vl) = unzip v             -- get values
		    viℂ = S.fromList $ map π vi   -- compute elementary constraints
		    aℂs = S.map ((ℂπ n) :=:) viℂ  -- constraint the current name to viℂ
		τList (nℂ ∪ aℂs) vi -- ⧺vl)

instance TyConstr Terminator where
	-- τℂ ∷ → Terminator → ℂState
	τℂ tmn = do
		(fn,πς) ← δfn
		bb ← δbb
		let cλ c = ℂp (ℂλ πς c) T.TyRegAddr
		case tmn of
			Ret _ VoidRet → do
				let τα = T.TyPri T.TyVoid
				    fnℂ = ℂπ fn :=: cλ (ℂτ τα)
				    bbℂ = ℂπ bb :=: ℂτ τα
				(↣) $ fnℂ ∘ (bbℂ ∘ ε)
			Ret _ (ValueRet v) → do
				τℂv ← τℂ v
				let πv = π v
				    fnℂ = ℂπ fn :=: cλ πv
				    bbℂ = ℂπ bb :=: πv
				(↣) $ fnℂ ∘ (bbℂ ∘ τℂv)
			Unreachable _ → do
				let τα = T.TyUndef 
				    fnℂ = ℂπ fn :=: cλ (ℂτ τα)
				    bbℂ = ℂπ bb :=: ℂτ τα
				(↣) $ fnℂ ∘ (bbℂ ∘ ε)
			Br _ c t f → do
				τℂv ← τList ε [c]--,t,f]
				let (πc,πt,πf) = (π c,π t,π f)
				    cℂ = πc :=: (ℂτ $ T.TyPri $ T.TyInt 1)
				    tfℂ = πt :=: πf
				    fnℂ = ℂπ fn :=: cλ πt
				    bbℂ = ℂπ bb :=: πt
				(↣) $ cℂ ∘ (tfℂ ∘ (fnℂ ∘ (bbℂ ∘ τℂv)))
			UBr _ d → do
				--τℂd ← τℂ d
				let πd = π d
				    fnℂ = ℂπ fn :=: cλ πd
				    bbℂ = ℂπ bb :=: πd
				(↣) $ fnℂ ∘ (bbℂ ∘ ε) -- τℂd)
			_ → error $ show tmn ⧺ " not supported"

instance TyConstr Instruction where
	τℂ i = case i of
	-- Standard Binary Operations
	-- Integer Operations
 		Add  _ n τ α β → τℂbin TInt n τ α β 
 		Sub  _ n τ α β → τℂbin TInt n τ α β  
 		Mul  _ n τ α β → τℂbin TInt n τ α β  
 		UDiv _ n τ α β → τℂbin TInt n τ α β  
 		SDiv _ n τ α β → τℂbin TInt n τ α β  
 		URem _ n τ α β → τℂbin TInt n τ α β  
 		SRem _ n τ α β → τℂbin TInt n τ α β
 	-- Bitwise Binary Operations
		Shl  _ n τ α β → τℂbin TInt n τ α β 
		LShr _ n τ α β → τℂbin TInt n τ α β 
		AShr _ n τ α β → τℂbin TInt n τ α β 
		And  _ n τ α β → τℂbin TInt n τ α β 
		Or   _ n τ α β → τℂbin TInt n τ α β 
		Xor  _ n τ α β → τℂbin TInt n τ α β 
    -- Float Operations
 		FAdd _ n τ α β → τℂbin TFlt n τ α β
 		FSub _ n τ α β → τℂbin TFlt n τ α β
 		FMul _ n τ α β → τℂbin TFlt n τ α β
 		FDiv _ n τ α β → τℂbin TFlt n τ α β
 		FRem _ n τ α β → τℂbin TFlt n τ α β
    -- Cast Operations
		Trunc    _ n α τ → τℂcast n (α,TInt) (τ,TInt) (>:)  -- Truncate integers
		ZExt     _ n α τ → τℂcast n (α,TInt) (τ,TInt) (:<:) -- Zero extend integers
		SExt     _ n α τ → τℂcast n (α,TInt) (τ,TInt) (:<:) -- Sign extend integers
		FPTrunc  _ n α τ → τℂcast n (α,TFlt) (τ,TFlt) (>:)  -- Truncate floating point
		FPExt    _ n α τ → τℂcast n (α,TFlt) (τ,TFlt) (:≤:) -- Extend floating point
		FPToUI   _ n α τ → τℂnastyCast n (α,TFlt) (τ,TInt) -- floating point → UInt
		FPToSI   _ n α τ → τℂnastyCast n (α,TFlt) (τ,TInt) -- floating point → SInt
		UIToFP   _ n α τ → τℂnastyCast n (α,TInt) (τ,TFlt) -- UInt → floating point
		SIToFP   _ n α τ → τℂnastyCast n (α,TInt) (τ,TFlt) -- SInt → floating point
		PtrToInt _ n α τ → τℂnastyCast n (α,TPtr) (τ,TInt) -- Pointer → integer 
		IntToPtr _ n α τ → τℂnastyCast n (α,TInt) (τ,TPtr) -- integer → Pointer
		BitCast  _ n α τ → τℂcast n (α,T1NA) (τ,T1NA) (:≌:) -- 1stclass non agg → 1stclass non agg
    -- Comparison Operations
		ICmp _ n _ τ α β → τℂcmp TInt n τ α β
		FCmp _ n _ τ α β → τℂcmp TFlt n τ α β 
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
		InlineAsm _ n τ β _ _ _ _ _ χ → 
			if β
			then error "error TypeInference InlineAsm: Could be lifted"
			else do
				χℂ ← τList ε χ
				let nτ = ℂτ $ (↑)τ
				    nℂ = ℂπ n :=: nτ
				(↣) $ nℂ ∘ χℂ
    -- Vector Operations
		Select       _ n α β η  → τselect n α β η
		ExtractValue _ n τ α δs → τextract n τ α
		InsertValue  _ n α β δs → error "insert agg operations not supported"

-- Type Constraints for Binary Operations
τℂbin ∷ TClass → Id → Τ → Value → Value → ℂState
τℂbin τc n τ α β = do
	τℂα ← τℂ α
	τℂβ ← τℂ β
	let cτρ = ℂτ $ (↑)τ
	    (πα,πβ) = (π α,π β)
	    αℂ = πα :=: cτρ  
	    βℂ = πβ :=: cτρ  
	    αβℂ = πα :=: πβ
	    cℂ = cτρ :=: ℂc τc
	    nℂ = ℂπ n :=: cτρ
	(↣) $ nℂ ∘ (αℂ ∘ (βℂ ∘ (αβℂ ∘ (cℂ ∘ (τℂα ∪ τℂβ)))))

-- Type Constraints for Cast Operations
τℂcast ∷ Id → (Value, TClass) → (Τ, TClass) → (ℂ → ℂ → Τℂ) → ℂState
τℂcast n (α,τcα) (τ,τcτ) (?:) = do
	τℂα ← τℂ α
	let cτρ = ℂτ $ (↑)τ
	    πα = π α
	    cℂα = πα :=: ℂc τcα
	    cℂτ = cτρ :=: ℂc τcτ
	    αℂ = πα ?: cτρ
	    nℂ = ℂπ n :=: cτρ
	(↣) $ nℂ ∘ (αℂ ∘ (cℂτ ∘ (cℂα ∘ τℂα)))

τℂnastyCast ∷ Id → (Value, TClass) → (Τ, TClass) → ℂState
τℂnastyCast n (α,τcα) (τ,τcτ) = do
	τℂα ← τℂ α
	let cτρ = ℂτ $ (↑)τ
	    πα = π α
	    cℂα = πα :=: ℂc τcα
	    cℂτ = cτρ :=: ℂc τcτ
	    nℂ = ℂπ n :=: cτρ
	(↣) $ nℂ ∘ (cℂτ ∘ (cℂα ∘ τℂα))

-- Type Constraints for comparison operations
τℂcmp ∷ TClass → Id → Τ → Value → Value → ℂState
τℂcmp τc n τ α β = do
	τℂα ← τℂ α
	τℂβ ← τℂ β
	let cτρ = ℂτ $ (↑)τ
	    (πα,πβ) = (π α,π β)
	    αβℂ = πα :=: πβ
	    --cℂ  = πα :=: cτρ
	    --cℂ  = πα :=: ℂc τc
	    cτn = ℂτ $ T.TyPri $ T.TyInt 1
	    nℂ = ℂπ n :=: cτn
	    --τℂ = cτρ :=: cτn
	(↣) $ nℂ ∘ (αβℂ ∘ (τℂα ∪ τℂβ))
--	(↣) $ nℂ ∘ (τℂ ∘ (αβℂ ∘ (cℂ ∘ (τℂα ∪ τℂβ))))

τℂcall ∷ Id → Τ → Id → Values → ℂState
τℂcall n τ c χ = do
	τℂχ ← τList ε χ
	let (πn,πc) = (ℂπ n,ℂπ c)
	    cτρ = ℂτ $ τncall c τ       -- OK
	    πχ = map π χ
	    nℂ = ℂπ n :=: cτρ   -- OK
	    ς  = ℂp (ℂλ πχ cτρ) T.TyRegAddr -- ℂλ πχ cτρ
	    cℂ = πc :=: ς
	vfns ← δvfns
	if c ∈ vfns
	then (↣) $ nℂ ∘ τℂχ
	else (↣) $ nℂ ∘ (cℂ ∘ τℂχ)

τncall ∷ Id → Τ → Τα
τncall (Global "ioremap") τ = τ ↑^ T.TyIOAddr
τncall n τ = (↑)τ

τselect ∷ Id → Value → Value → Value → ℂState
τselect n α β η = do
	let (πα,πβ,πη) = (π α,π β,π η)
	    αcτ = ℂτ $ T.TyPri $ T.TyInt 1
	    αℂ = πα :=: αcτ
	    nℂ = ℂπ n :=: πβ
	    βηℂ = πβ :=: πη
	    βℂ = πβ :=: ℂc T1
	(↣) $ αℂ ∘ (nℂ ∘ (βηℂ ∘ (βℂ ∘ ε))) 

τextract ∷ Id → Τ → Value → ℂState
τextract n τ α = do
	αℂ ← τℂ α
	let (πn, πα) = (ℂπ n, π α)
	    nτ = ℂτ $ (↑)τ
	    nℂ = πn :=: nτ
	    cℂ = πα :=: ℂc TAgg
	(↣) $ nℂ ∘ (cℂ ∘ αℂ)