{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Worklist
-- Copyright :  (c) 2013 Marcelo Sousa
-- Worklist algorithm
-- Type equality must consider recursive types
-- That is going to complicate a bit more.
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Worklist where

import Language.LLVMIR hiding (Id)
import Analysis.Type.Inference.Base
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.TyAnn as T

import Analysis.Type.Inference.Value
import Control.Monad.State
import Analysis.Type.Util

import qualified Data.Set as S
import qualified Data.Map as M

type Γ = M.Map Id TyAnn

εΓ = M.empty

(⊨) ∷ S.Set Τℂ → Γ
(⊨) τℂ = solve εΓ τℂ
--(⊨) τℂ = S.fold (\τc γ → solve γ τℂ τc) εΓ τℂ

-- Solve 
-- Input : Env, Constraints left
solve ∷ Γ → S.Set Τℂ → Γ
solve γ τℂ = 
  if S.null τℂ
  then γ
  else let (c,τℂ') = extract τℂ
       in case c of
      	c1 :=: c2 → solveEq γ τℂ' c1 c2 
      	c1 :<: c2 → undefined
      	c1 :≤: c2 → undefined
      	c1 :≌: c2 → undefined

-- Solving :=:
solveEq ∷ Γ → S.Set Τℂ → ℂ → ℂ → Γ
solveEq γ τℂ (ℂτ τ1) (ℂτ τ2) =
	if τ1 == τ2
	then solve γ τℂ
	else error $ "τ error: solveEq (1)"
 


extract ∷ S.Set Τℂ → (Τℂ, S.Set Τℂ)
extract τℂ = case S.minView τℂ of
	Nothing → error "extract"
	Just v  → v

look ∷ S.Set Τℂ → Id → S.Set Τℂ
τℂ `look` n = S.filter (lhsτℂ n) τℂ

lhsτℂ ∷ Id → Τℂ → Bool
lhsτℂ n (ℂπ m :=: _) = n == m
lhsτℂ n (ℂπ m :<: _) = n == m
lhsτℂ n (ℂπ m :≤: _) = n == m
lhsτℂ n (ℂπ m :≌: _) = n == m
lhsτℂ n _            = False
