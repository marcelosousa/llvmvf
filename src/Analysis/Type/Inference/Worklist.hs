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

import Prelude.Unicode ((⧺))

import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace

type Ω = M.Map Id ℂ

εΩ = M.empty

optT ∷ S.Set Τℂ → Ω
optT = S.fold rewrite εΩ

rewrite ∷ Τℂ → Ω → Ω
rewrite τℂ γ = case τℂ of
  c1 :=: c2 → snd $ rewriteEq γ c1 c2 
  c1 :<: c2 → undefined
  c1 :≤: c2 → undefined
  c1 :≌: c2 → undefined

-- rewriteEq
rewriteEq ∷ Ω → ℂ → ℂ → (ℂ,Ω)
rewriteEq γ α@(ℂτ τ)     β = rwEqτ γ α β -- type
rewriteEq γ α@(ℂc cl)    β = rwEqc γ α β -- class
rewriteEq γ α@(ℂι c i)   β = undefined --rwEqι γ α β -- gep
rewriteEq γ α@(ℂp c τα)  β = undefined --rwEqp γ α β -- pointer
rewriteEq γ α@(ℂλ ca cr) β = rwEqλ γ α β -- function
rewriteEq γ α@(ℂπ n)     β = rwEqπ γ α β -- var

-- Type
rwEqτ ∷ Ω → ℂ → ℂ → (ℂ,Ω)
rwEqτ γ α@(ℂτ τ1) β =
  case β of
    ℂτ τ2 → if τ1 == τ2 
            then (α,γ)
            else error $ "rwEqτ (1)"
    ℂc cl → if classOf τ1 == cl 
            then (α,γ)
            else error $ "rwEqτ (2)"
    _ → rewriteEq γ β α                       
rwEqτ γ _ _ = error $ "rwEqτ: FATAL"

-- Type Class
rwEqc ∷ Ω → ℂ → ℂ → (ℂ,Ω)
rwEqc γ α@(ℂc cl1) β = 
  case β of 
    ℂc cl2 → if cl1 == cl2
             then (α,γ)
             else error $ "rwEqc (1)"
    _ → rewriteEq γ β α
rwEqc γ _ _ = error $ "rwEqc: FATAL"

-- Type Var
rwEqπ ∷ Ω → ℂ → ℂ → (ℂ,Ω)
rwEqπ γ α@(ℂπ n) β =  
  case M.lookup n γ of
    Nothing → (β, M.insert n β γ)
    Just ζ  → case ζ of
      ℂπ m → if n == m
             then (β, M.insert n β γ)
             else case β of
                ℂπ o → if n == o || o == m
                       then (β, γ)
                       else (β, M.insert m β γ)
                _ → (β,M.insert n β γ)
      _    → let (c,γ') = rewriteEq γ β ζ
             in (c,M.insert n c γ')
rwEqπ γ _ _ = error $ "rwEqπ: FATAL"


-- Type Function
rwEqλ ∷ Ω → ℂ → ℂ → (ℂ,Ω)
rwEqλ γ α@(ℂλ ca1 cr1) β =
  case β of 
    ℂλ ca2 cr2 → 
      let (ca,γ') = foldr fλ ([],γ) $ zip ca1 ca2
          fλ (a1,a2) (lca, g) = 
            let (c,g') = rewriteEq g a1 a2
            in (c:lca,g')
          (cr,γ'') = rewriteEq γ' cr1 cr2
      in (ℂλ ca cr,γ'')
    ℂc c → undefined  
    _    → rewriteEq γ β α
rwEqλ γ _ _ = error $ "rwEqλ: FATAL"

{-
-- Type gep
rwEqι ∷ Ω → ℂ → ℂ → (ℂ,Ω)
rwEqι γ (ℂι c1 i1) (ℂι c2 i2)
rwEqι γ (ℂι c1 i1) (ℂc c)
rwEqι γ (ℂι c1 i1) (ℂλ ca cr) = error "rewriteEq: cant constraint gep with function"
rwEqι γ (ℂι c1 i1) (ℂp c ταρ) = error "rewriteEq: cant constraint gep with pointer"
rwEqι γ (ℂι c1 i1) c = rewriteEq γ c (ℂι c1 i1) 

-- Type pointer
rewriteEq γ (ℂp c1 τα1) (ℂp c2 τα2)
rewriteEq γ (ℂp c1 τα1) (ℂc c)
rewriteEq γ (ℂp c1 τα1) (ℂλ ca cr) = error "rewriteEq: cant constraint pointer with function"
rewriteEq γ (ℂp c1 τα1) c = rewriteEq γ c (ℂp c1 τα1)

-}

------------------------------------------




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
solveEq = undefined
{-
-- τ :=: τ
solveEq γ τℂ (ℂτ τ1) (ℂτ τ2) =
	if τ1 == τ2
	then solve γ τℂ
	else error $ "τ error: solveEq (1)"
-- τ :=: c
solveEq γ τℂ (ℂτ τ1) (ℂc c) = 
  if classOf τ1 == c
  then solve γ τℂ
  else error $ "τ error: solveEq (2)"
-- n :=: τ  
solveEq γ τℂ (ℂπ n) (ℂτ τ1) = 
  let γ' = case M.lookup n γ of
    Nothing → let γ' = M.insert n τ1 γ
              in solve γ' τℂ
    Just τ2 → let τ = τ1 `unify` τ2
                  M.update (const $ Just τ) γ
  in solve γ' τℂ
solveEq γ τℂ (ℂπ n) (ℂπ x) = undefined
-}

unify ∷ Τ → Τ → Τ
unify = undefined
 
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
