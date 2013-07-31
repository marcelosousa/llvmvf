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

import Debug.Trace
instance TyConstr PHI where
	-- τℂ ∷ → PHI → ℂState
	τℂ (PHI pc n τ v) = do
		let τα = (↑)τ                     -- lift type
		    nℂ = (ℂπ n) :=: (ℂτ τα) ∘ ε   -- constraint the id to type τα
		    (vi,vl) = unzip v             -- get values
		    viℂ = S.fromList $ map π vi   -- compute elementary constraints
		    aℂs = S.map ((ℂπ n) :=:) viℂ  -- constraint the current name to viℂ
		lℂ ← τListR (nℂ ∪ aℂs) vi -- ⧺vl)
		(↣) $ liftΤℂ pc lℂ

instance TyConstr Terminator where
	-- τℂ ∷ → Terminator → ℂState
	τℂ tmn = do
		(fn,πς) ← δfn
		bb ← δbb
		let cλ c = ℂp (ℂλ πς c) anyRegAddr
		case tmn of
			Ret pc VoidRet → do
				let τα = T.TyPri T.TyVoid
				    fnℂ = ℂπ fn :=: cλ (ℂτ τα)
				    bbℂ = ℂπ bb :=: ℂτ τα
				(↣) $ liftΤℂ pc $ fnℂ ∘ (bbℂ ∘ ε)
			Ret pc (ValueRet v) → do
				τℂv ← τℂr v
				let πv = π v
				    fnℂ = ℂπ fn :=: cλ πv
				    bbℂ = ℂπ bb :=: πv
				(↣) $ liftΤℂ pc $ fnℂ ∘ (bbℂ ∘ τℂv)
			Unreachable pc → do
				let τα = T.TyUndef 
				    fnℂ = ℂπ fn :=: cλ (ℂτ τα)
				    bbℂ = ℂπ bb :=: ℂτ τα
				(↣) $ liftΤℂ pc $ fnℂ ∘ (bbℂ ∘ ε)
			Br pc c t f → do
				τℂv ← τListR ε [c]--,t,f]
				let (πc,πt,πf) = (π c,π t,π f)
				    cℂ = πc :=: (ℂτ $ T.TyPri $ T.TyInt 1)
				    tfℂ = πt :=: πf
				    fnℂ = ℂπ fn :=: cλ πt
				    bbℂ = ℂπ bb :=: πt
				(↣) $ liftΤℂ pc $ cℂ ∘ (tfℂ ∘ (fnℂ ∘ (bbℂ ∘ τℂv)))
			UBr pc d → do
				--τℂd ← τℂ d
				let πd = π d
				    fnℂ = ℂπ fn :=: cλ πd
				    bbℂ = ℂπ bb :=: πd
				(↣) $ liftΤℂ pc $ fnℂ ∘ (bbℂ ∘ ε) -- τℂd)
			_ → error $ show tmn ⧺ " not supported"

instance TyConstr Instruction where
	τℂ i = case i of
	-- Standard Binary Operations
	-- Integer Operations
 		Add  pc n τ α β → τℂbin pc TInt n τ α β 
 		Sub  pc n τ α β → τℂbin pc TInt n τ α β  
 		Mul  pc n τ α β → τℂbin pc TInt n τ α β  
 		UDiv pc n τ α β → τℂbin pc TInt n τ α β  
 		SDiv pc n τ α β → τℂbin pc TInt n τ α β  
 		URem pc n τ α β → τℂbin pc TInt n τ α β  
 		SRem pc n τ α β → τℂbin pc TInt n τ α β
 	-- Bitwise Binary Operations
		Shl  pc n τ α β → τℂbin pc TInt n τ α β 
		LShr pc n τ α β → τℂbin pc TInt n τ α β 
		AShr pc n τ α β → τℂbin pc TInt n τ α β 
		And  pc n τ α β → τℂbin pc TInt n τ α β 
		Or   pc n τ α β → τℂbin pc TInt n τ α β 
		Xor  pc n τ α β → τℂbin pc TInt n τ α β 
    -- Float Operations
 		FAdd pc n τ α β → τℂbin pc TFlt n τ α β
 		FSub pc n τ α β → τℂbin pc TFlt n τ α β
 		FMul pc n τ α β → τℂbin pc TFlt n τ α β
 		FDiv pc n τ α β → τℂbin pc TFlt n τ α β
 		FRem pc n τ α β → τℂbin pc TFlt n τ α β
    -- Cast Operations
		Trunc    pc n α τ → τℂcast pc n (α,TInt) (τ,TInt) (>:)  -- Truncate integers
		ZExt     pc n α τ → τℂcast pc n (α,TInt) (τ,TInt) (:<:) -- Zero extend integers
		SExt     pc n α τ → τℂcast pc n (α,TInt) (τ,TInt) (:<:) -- Sign extend integers
		FPTrunc  pc n α τ → τℂcast pc n (α,TFlt) (τ,TFlt) (>:)  -- Truncate floating point
		FPExt    pc n α τ → τℂcast pc n (α,TFlt) (τ,TFlt) (:≤:) -- Extend floating point
		FPToUI   pc n α τ → τℂnastyCast pc n (α,TFlt) (τ,TInt) -- floating point → UInt
		FPToSI   pc n α τ → τℂnastyCast pc n (α,TFlt) (τ,TInt) -- floating point → SInt
		UIToFP   pc n α τ → τℂnastyCast pc n (α,TInt) (τ,TFlt) -- UInt → floating point
		SIToFP   pc n α τ → τℂnastyCast pc n (α,TInt) (τ,TFlt) -- SInt → floating point
		PtrToInt pc n α τ → τℂnastyCast pc n (α,TPtr) (τ,TInt) -- Pointer → integer 
		IntToPtr pc n α τ → τℂnastyCast pc n (α,TInt) (τ,TPtr) -- integer → Pointer
		BitCast  pc n α τ → τℂcast pc n (α,T1NA) (τ,T1NA) (:≌:) -- 1stclass non agg → 1stclass non agg
    -- Comparison Operations
		ICmp pc n _ τ α β → τℂcmp pc TInt n τ α β
		FCmp pc n _ τ α β → τℂcmp pc TFlt n τ α β 
    -- Memory Operations
		Alloca pc n τ   _ → τℂalloca pc n τ
		Store  pc τ α β _ → τℂstore  pc τ α β
		Load   pc n α   _ → τℂload   pc n α
		GetElementPtr pc n τ α δs → τℂgep pc n τ α δs
    -- Atomic Operations
  		Cmpxchg   pc n α β η _ → τℂaop pc n α [β,η]
		AtomicRMW pc n α β _ _ → τℂaop pc n α [β]
    -- Call
		Call pc n τ c χ → τℂcall pc n τ c χ
		InlineAsm pc n τ β _ _ _ _ _ χ → 
			if β
			then error "error TypeInference InlineAsm: Could be lifted"
			else do
				χℂ ← τListR ε χ
				let nτ = ℂτ $ (↑)τ
				    nℂ = ℂπ n :=: nτ
				(↣) $ liftΤℂ pc $ nℂ ∘ χℂ
    -- Vector Operations
		Select       pc n α β η  → τselect  pc n α β η
		ExtractValue pc n τ α δs → τextract pc n τ α
		InsertValue  pc n α β δs → error "insert agg operations not supported"

-- Type Constraints for Binary Operations
τℂbin ∷ Int → TClass → Id → Τ → Value → Value → ℂState
τℂbin pc τc n τ α β = do
	τℂα ← τℂr α
	τℂβ ← τℂr β
	let cτρ = ℂτ $ (↑)τ
	    (πα,πβ) = (π α,π β)
	    αℂ = πα :=: cτρ  
	    βℂ = πβ :=: cτρ  
	    αβℂ = πα :=: πβ
	    cℂ = cτρ :=: ℂc τc
	    nℂ = ℂπ n :=: cτρ
	(↣) $ liftΤℂ pc $ nℂ ∘ (αℂ ∘ (βℂ ∘ (αβℂ ∘ (cℂ ∘ (τℂα ∪ τℂβ)))))

-- Type Constraints for Cast Operations
τℂcast ∷ Int → Id → (Value, TClass) → (Τ, TClass) → (ℂ → ℂ → Τℂ) → ℂState
τℂcast pc n (α,τcα) (τ,τcτ) (?:) = do
	τℂα ← τℂr α
	let cτρ = ℂτ $ (↑)τ
	    πα = π α
	    cℂα = πα :=: ℂc τcα
	    cℂτ = cτρ :=: ℂc τcτ
	    αℂ = πα ?: cτρ
	    nℂ = ℂπ n :=: cτρ
	(↣) $ liftΤℂ pc $ nℂ ∘ (αℂ ∘ (cℂτ ∘ (cℂα ∘ τℂα)))

τℂnastyCast ∷ Int → Id → (Value, TClass) → (Τ, TClass) → ℂState
τℂnastyCast pc n (α,τcα) (τ,τcτ) = do
	τℂα ← τℂr α
	let cτρ = ℂτ $ (↑)τ
	    πα = π α
	    cℂα = πα :=: ℂc τcα
	    cℂτ = cτρ :=: ℂc τcτ
	    nℂ = ℂπ n :=: cτρ
	(↣) $ liftΤℂ pc $ nℂ ∘ (cℂτ ∘ (cℂα ∘ τℂα))

-- Type Constraints for comparison operations
τℂcmp ∷ Int → TClass → Id → Τ → Value → Value → ℂState
τℂcmp pc τc n τ α β = do
	τℂα ← τℂr α
	τℂβ ← τℂr β
	let cτρ = ℂτ $ (↑)τ
	    (πα,πβ) = (π α,π β)
	    αβℂ = πα :=: πβ
	    --cℂ  = πα :=: cτρ
	    --cℂ  = πα :=: ℂc τc
	    cτn = ℂτ $ T.TyPri $ T.TyInt 1
	    nℂ = ℂπ n :=: cτn
	    --τℂ = cτρ :=: cτn
	(↣) $ liftΤℂ pc $ nℂ ∘ (αβℂ ∘ (τℂα ∪ τℂβ))
--	(↣) $ nℂ ∘ (τℂ ∘ (αβℂ ∘ (cℂ ∘ (τℂα ∪ τℂβ))))

τℂcall ∷ Int → Id → Τ → Id → Values → ℂState
τℂcall pc n τ c χ = do
	τℂχ ← τListR ε χ
	let (πn,πc) = (ℂπ n,ℂπ c)
	    cτρ = ℂτ $ (↑)τ       -- OK
	    πχ = map π χ
	    nℂ = πn :=: cτρ   -- OK
	    ς  = ℂp (ℂλ πχ πn) anyRegAddr -- ℂλ πχ cτρ
	    cℂ = πc :=: ς
	vfns ← δvfns
	if c ∈ vfns
	then (↣) $ liftΤℂ pc $ nℂ ∘ τℂχ
	else (↣) $ liftΤℂ pc $ nℂ ∘ (cℂ ∘ τℂχ)

{-
τncall ∷ Id → Maybe ℂ
τncall (Global "ioremap") τ = Just $ ℂτ $ TyDer $ TyPtr (i 8) TyIOAddr
τncall n τ = Nothing
-}

τselect ∷ Int → Id → Value → Value → Value → ℂState
τselect pc n α β η = do
	let (πα,πβ,πη) = (π α,π β,π η)
	    αcτ = ℂτ $ T.TyPri $ T.TyInt 1
	    αℂ = πα :=: αcτ
	    nℂ = ℂπ n :=: πβ
	    βηℂ = πβ :=: πη
	    βℂ = πβ :=: ℂc T1
	(↣) $ liftΤℂ pc $ αℂ ∘ (nℂ ∘ (βηℂ ∘ (βℂ ∘ ε))) 

τextract ∷ Int → Id → Τ → Value → ℂState
τextract pc n τ α = do
	αℂ ← τℂr α
	let (πn, πα) = (ℂπ n, π α)
	    nτ = ℂτ $ (↑)τ
	    nℂ = πn :=: nτ
	    cℂ = πα :=: ℂc TAgg
	(↣) $ liftΤℂ pc $ nℂ ∘ (cℂ ∘ αℂ)