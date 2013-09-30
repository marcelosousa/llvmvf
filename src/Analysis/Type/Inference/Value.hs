{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Value
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Constraints for Value and Constants
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Value where

import Language.LLVMIR
import Analysis.Type.Inference.Base
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.TyAnn as T
import Analysis.Type.Util

import qualified Data.Set as S
import Prelude.Unicode ((⧺),(≡))

import Debug.Trace

-- Type Constraints for values
instance TyConstrR Value where
	τℂr (Id n τ) = vτℂgen n τ 
	τℂr (Constant c) = τℂr c

instance TyConstrR Constant where
	τℂr c = case c of
		UndefValue      → (↣) ε
		SmpConst sc     → (↣) ε
		CmpConst cc     → (↣) ε
		GlobalValue gv  → τℂr gv
		ConstantExpr ce → (↣) ε
	  	_               → error "constant not supported"	

instance TyConstrR GlobalValue where
	τℂr v = case v of
	  FunctionValue  n τ → vτℂgen n τ
	  GlobalAlias    n τ → (↣) ε
	  GlobalVariable n τ → (↣) ε

vτℂgen ∷ Identifier → Type → ΕState (S.Set Τℂ)
vτℂgen n τ = do
	let τα = (↑)τ
	    nℂ = (ℂπ n) :=: (ℂτ τα) ∘ ε
	(↣) nℂ


instance Constr Value where
	π (Id n τ) = ℂπ n
	π (Constant c) = π c

instance Constr Constant where
	-- π ∷ Constant → ℂ
	π c = case c of
	  UndefValue      → ℂτ T.TyUndef 
	  SmpConst sc     → π sc
	  CmpConst cc     → π cc
	  GlobalValue gv  → π gv
	  ConstantExpr ce → π ce
	  _               → error "constant not supported"

-- Here we have three options:
-- 1. Either consider the erased term and then 
-- generate the minimum type that can contain the value
-- 2. Lift the annotated type w/ checks
-- 3. Lift the annotated type with checks
instance Constr SimpleConstant where
	π c = case c of
	  -- Here I could check if the value fits in that number of bits.
	  ConstantInt α τ → ℂτ $ (↑)τ
	  ConstantFP  α   → π α
	  ConstantPointerNull τ → ℂτ $ (↑)τ

instance Constr ConstantFP where
	π c = case c of
	  ConstantFPFloat  α τ → ℂτ $ (↑)τ
	  ConstantFPDouble α τ → ℂτ $ (↑)τ

instance Constr GlobalValue where
	π v = case v of
	  FunctionValue  n τ → ℂπ n
	  GlobalAlias    n τ → ℂπ n
	  GlobalVariable n τ → ℂπ n
  
instance Constr ComplexConstant where
	π c = case c of
	  ConstantAggregateZero  τ → ℂτ $ (↑)τ
	  ConstantDataSequential α → π α
	  ConstantStruct τ χ → ℂτ $ (↑)τ
	  ConstantArray  τ χ → ℂτ $ (↑)τ
	  ConstantVector τ χ → ℂτ $ (↑)τ

instance Constr ConstantDataSequential where
 	π c = case c of
 	  ConstantDataArray  τ α → ℂτ $ (↑)τ
 	  ConstantDataVector τ α → ℂτ $ (↑)τ

instance Constr ConstantExpr where
 	π e = case e of
 	  BinaryConstantExpr             → error "π of BinaryConstantExpr"
 	  CompareConstantExpr ce         → π ce
 	  ExtractElementConstantExpr     → error "π of ExtractElementConstantExpr"
 	  ExtractValueConstantExpr       → error "π of ExtractValueConstantExpr"
 	  GetElementPtrConstantExpr τ α δs → πgep α δs
 	  InsertElementConstantExpr      → error "π of InsertElementConstantExpr"
 	  InsertValueConstantExpr        → error "π of InsertValueConstantExpr"
 	  SelectConstantExpr             → error "π of SelectConstantExpr"
 	  ShuffleVectorConstantExpr      → error "π of ShuffleVectorConstantExpr"
 	  UnaryConstantExpr n op α τ     → ℂq ((↑)τ) $ π α

πgep ∷ Value → Values → ℂ
πgep α δs = let πα = π α
                δis = map getIntValue δs
	        in ℂι πα δis

instance Constr CompareConstantExpr where
	π e = case e of
	  ICmpExpr _ τ α β → ℂτ $ (↑)τ
	  FCmpExpr _ τ α β → ℂτ $ (↑)τ
