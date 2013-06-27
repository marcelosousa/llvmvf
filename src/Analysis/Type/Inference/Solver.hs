{-# LANGUAGE UnicodeSyntax, FlexibleInstances, RecordWildCards, DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Solver
-- Copyright :  (c) 2013 Marcelo Sousa
-- Worklist algorithm
-- Type equality must consider recursive types
-- That is going to complicate a bit more.

-------------------------------------------------------------------------------

module Analysis.Type.Inference.Solver where

import Language.LLVMIR hiding (Id, NamedTypes)
import Analysis.Type.Inference.Base
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.TyAnn as T
import Language.LLVMIR.Util
import Analysis.Type.Inference.Value
import Control.Monad.State
import Analysis.Type.Util

import Prelude.Unicode ((⧺),(≡))
import Data.List.Unicode ((∈))

import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad
import Control.Monad.State

import qualified Debug.Trace as Trace
import UU.PPrint 

trace s f = f
--trace = Trace.trace

data Ω = Ω 
  { 
    rc  ∷ S.Set Τℂ  -- remaining constraints
  , mic ∷ M.Map Id ℂ -- current map
  , nmdtys ∷ NamedTypes
  , gmap ∷ Γ -- global map
  }

instance Eq Ω where
  (Ω rc1 mic1 _ _) == (Ω rc2 mic2 _ _) = rc1 ≡ rc2 && mic1 ≡ mic2 

νΤℂ ∷ Τℂ → ΩState ()
νΤℂ τℂ = do γ@Ω{..} ← get
            let rc' = τℂ ∘ rc
            put γ{rc=rc'}

νIℂ ∷ Id → ℂ → ΩState ()
νIℂ i c = trace ("Adding to map: " ⧺ show i ⧺ " " ⧺ show c) $ 
  do γ@Ω{..} ← get
     let mic' = M.insert i c mic
     put γ{mic=mic'}

δNτ ∷ ΩState NamedTypes
δNτ = do γ@Ω{..} ← get
         (↣) nmdtys

type ΩState α = State Ω α

iΩ c = Ω c M.empty

μrewriteEq ∷ ΩState ()
μrewriteEq = trace ("fixRewriteEq -----") $ do
  γ ← get
  let τℂs = S.toList $ rc γ
  put γ{rc = ε}
  mapM_ rewriteEq τℂs 
  γ' ← get
  if γ ≡ γ'
  then (↣) ()
  else μrewriteEq

rewriteEq ∷ Τℂ → ΩState ()
rewriteEq τℂ =trace ("rewriteEq " ⧺ show τℂ) $ 
  case τℂ of
    c1 :=: c2 → do 
      rwEq c1 c2
      (↣) ()
    _  → νΤℂ τℂ

-- rwEq
rwEq ∷ ℂ → ℂ → ΩState ℂ
rwEq α β = trace ("rwEq " ⧺ show α ⧺ " " ⧺ show β) $ 
  case α of
    ℂτ τ     → rwEqτ α β -- type
    ℂc cl    → rwEqc α β -- class
    ℂι c i   → rwEqι α β -- gep
    ℂp c τα  → rwEqp α β -- pointer
    ℂλ ca cr → rwEqλ α β -- function
    ℂπ n     → rwEqπ α β -- var

-- Type
rwEqτ ∷ ℂ → ℂ → ΩState ℂ
rwEqτ α@(ℂτ τ1) β = trace ("rwEqTy " ⧺ show α ⧺ " " ⧺ show β) $ do
  nτs ← δNτ
  case β of
    ℂτ τ2 → case (≅) nτs τ1 τ2 of
              Nothing → error $ "rwEqTy (1) " ⧺ show α ⧺ "\n" ⧺ show β
              Just c  → (↣) $ ℂτ c
    ℂc cl → if τ1 `classOf` cl 
            then (↣) α
            else error $ "rwEqTy " ⧺ show τ1 ⧺ " not class of " ⧺ show cl
    _ → rwEq β α                       
rwEqτ _ _ = error $ "rwEqTy: FATAL"

-- Type Class
rwEqc ∷ ℂ → ℂ → ΩState ℂ
rwEqc α@(ℂc cl1) β = trace ("rwEqc " ⧺ show α ⧺ " " ⧺ show β) $  do
  nτs ← δNτ
  case β of 
    ℂc cl2 → case (≅) nτs cl1 cl2 of
              Nothing → error $ "rwEqc (1) " ⧺ show α ⧺ " " ⧺ show β
              Just cl → (↣) $ ℂc cl
    _ → rwEq β α
rwEqc _ _ = error $ "rwEqc: FATAL"

-- Type Var
rwEqπ ∷ ℂ → ℂ → ΩState ℂ
rwEqπ α@(ℂπ n) β = trace ("rwEqv (start) " ⧺ show α ⧺ " " ⧺ show β) $ do
  γ@Ω{..} ← get
  case M.lookup n mic of
    Nothing → trace ("rwEqv (not in map) " ⧺ show α ⧺ " " ⧺ show β) $ do
      νIℂ n β
      (↣) β
    Just ζ  → case ζ of
      ℂπ m → if n ≡ m
             then trace ("rwEqv (in map, Cv equal): " ⧺ show (pretty n) ⧺ " " ⧺ show (pretty m)) $ do 
              νIℂ n β
              (↣) β
             else case β of
                ℂπ o → if n ≡ o || o ≡ m
                       then trace ("rwEqv (in map, Cv diff but messy): " ⧺ show (pretty n) ⧺ " " ⧺ show (pretty m) ⧺ " " ⧺ show (pretty o)) $ (↣) β
                       else trace ("rwEqv (in map, Cv diff but messy (2)): " ⧺ show (pretty n) ⧺ " " ⧺ show (pretty m) ⧺ " " ⧺ show (pretty o)) $ do 
                        νIℂ m β
                        (↣) β
                _ → do 
                  if mutual m [n] mic
                  then trace ("rwEqv (in map, Cv dif and not Cv but mutual): " ⧺ show (pretty n) ⧺ " " ⧺ show (pretty m) ⧺ " " ⧺ show β) $ do 
                    νIℂ n β
                    νIℂ m β
                    (↣) β
                  else trace ("rwEqv (in map, Cv dif, not Cv, not mutual): " ⧺ show (pretty n) ⧺ " " ⧺ show (pretty m) ⧺ " " ⧺ show β) $ do 
                    c ← rwEq ζ β
                    νIℂ n c
                    (↣) c
      _    → do c ← trace ("rwEqv (in map but not Cv): calling rwEq " ⧺ show (pretty n) ⧺ " " ⧺ show β ⧺ " " ⧺ show ζ) $ rwEq β ζ
                νIℂ n c
                (↣) c
rwEqπ _ _ = error $ "rwEqπ: FATAL"

mutual ∷ Id → [Id] → M.Map Id ℂ → Bool
mutual n l γ | n ∈ l = True
             | otherwise = 
  case M.lookup n γ of
    Nothing → False
    Just (ℂπ m) → mutual m (n:l) γ
    Just _ → False

-- Type Function
rwEqλ ∷ ℂ → ℂ → ΩState ℂ
rwEqλ α@(ℂλ ca1 cr1) β = trace ("rwEqFn " ⧺ show α ⧺ " " ⧺ show β) $ 
  case β of 
    ℂλ ca2 cr2 → do
      ca ← mapM (uncurry rwEq) $ zip ca1 ca2
      cr ← rwEq cr1 cr2
      (↣) $ ℂλ ca cr
    ℂc c → error $ "rwEqλ: class constraint"
    _    → rwEq β α
rwEqλ _ _ = error $ "rwEqλ: FATAL"

-- Type gep
rwEqι ∷ ℂ → ℂ → ΩState ℂ
rwEqι α@(ℂι cin idxn) β = trace ("rwEqGep " ⧺ show α ⧺ " " ⧺ show β) $  do
  γ@Ω{..} ← get
  let τα = solveEq nmdtys gmap mic [] α
  rwEq β (ℂτ τα)

-- Type pointer
-- Missing ℂc
rwEqp ∷ ℂ → ℂ → ΩState ℂ
rwEqp α@(ℂp c1 τα1) β = trace ("rwEqp " ⧺ show α ⧺ " " ⧺ show β) $ 
  case β of
    ℂτ τ → case τ of
      TyDer (TyPtr τ1 τα2) → 
        case τα1 ≌ τα2 of
          Just τα → do c ← rwEq c1 (ℂτ τ1)
                       case c of
                        ℂτ τ' → (↣) $ ℂτ $ TyDer $ TyPtr τ' τα
                        _     → error $ "rwEqp error: impossible case? " ⧺ show c
          Nothing → error $ "rwEqp error: " ⧺ show α ⧺ " " ⧺ show β
      _ → error $ "rwEqp: types dont match " ⧺ show α ⧺ " " ⧺ show β
    ℂp c2 τα2 → 
      case τα1 ≌ τα2 of
        Just τα → do c ← rwEq c1 c2
                     (↣) $ ℂp c τα
        Nothing → error $ "rwEqp error: " ⧺ show α ⧺ " " ⧺ show β
    ℂλ ca cr → error "rwEqp: cant constraint pointer with function" 
    _ → rwEq β α
rwEqp _ _ = error $ "rwEqp: FATAL"

------------------------------------------

type Γ = M.Map Id Τα

(⊨) ∷ NamedTypes → Γ → S.Set Τℂ → Γ
(⊨) nτ e τℂ = let γ@Ω{..} = trace "rewriteEq" $ execState μrewriteEq (iΩ τℂ nτ e)
                  γ' = trace ("solveEq\n" ⧺ traceSolveEq mic) $ M.mapWithKey (\n c → solveEq nτ e mic [n] c) mic
              in S.fold (solveCast nτ) γ' rc

traceSolveEq ∷ M.Map Id ℂ → String
traceSolveEq = M.foldrWithKey (\k v r → show (pretty k) ⧺ " , " ⧺ show v ⧺ "\n" ⧺ r ) ""

-- Solve 
-- Input : Env, Constraints left
solveEq ∷ NamedTypes → Γ → M.Map Id ℂ → [Id] → ℂ → Τα
solveEq nτ e γ n τℂ = trace ("solveEq " ⧺ show n) $ case τℂ of
  ℂτ τ → τ
  ℂπ m → look nτ e γ n m
  ℂc cl → error $ "solve does not expect a class " ⧺ show n ⧺ show τℂ
  ℂι c is → let cτ = solveEq nτ e γ n c
            in TyDer $ TyPtr (gepτs nτ cτ is) TyAny
  ℂp c a → let cτ = solveEq nτ e γ n c 
           in TyDer $ TyPtr cτ a  
  ℂλ ca cr → let caτ = map (solveEq nτ e γ n) ca
                 crτ = solveEq nτ e γ n cr
             in TyDer $ TyFun caτ crτ False

look ∷ NamedTypes → Γ → M.Map Id ℂ → [Id] → Id → Τα 
look nτ e γ l m | m ∈ l = error "look: same identifiers"
                | otherwise = case M.lookup m γ of
                     Nothing → case M.lookup m e of
                       Nothing → error $ "look: not in maps " ⧺ show m ⧺ show l ⧺ show γ ⧺ show e
                       Just τ  → τ
                     Just c  → solveEq nτ e γ (m:l) c

solveCast ∷ NamedTypes → Τℂ → Γ → Γ
solveCast nτ τℂ γ = case τℂ of
  c1 :=: c2 → error "solveCast :=: impossible"
  c1 :<: c2 → solveBit (<) nτ γ c1 c2
  c1 :≤: c2 → solveBit (<=) nτ γ c1 c2
  c1 :≌: c2 → solveBit (≡) nτ γ c1 c2

solveBit ∷ (Int → Int → Bool) → NamedTypes → Γ → ℂ → ℂ → Γ
solveBit op nτ γ α@(ℂπ n) β =
  let ατ = safeLookup n γ
  in case β of
    ℂτ βτ → if sizeOf ατ `op` sizeOf βτ
            then γ
            else error $ "solveBit: bitsize mismatch " ⧺ show α ⧺ " " ⧺ show β
    ℂπ m  → error "solveBit: both are Cv"
    _ → error "solveBit: beta is not supported"
solveBit op nτ γ α β@(ℂπ n) =
  let βτ = safeLookup n γ
  in case α of
    ℂτ ατ → if sizeOf ατ `op` sizeOf βτ
            then γ
            else error $ "solveBit: bitsize mismatch " ⧺ show α ⧺ " " ⧺ show β
    ℂπ m  → error "solveBit: both are Cv"
    _ → error "solveBit: beta is not supported"
solveBit op nτ γ α β = error $ "solveBit: not Cpi (" ⧺ show α ⧺ ", " ⧺ show β ⧺ ")"

safeLookup ∷ Id → Γ → Τα
safeLookup n γ = 
  case M.lookup n γ of
    Nothing → error $ "safeLookup failed with id=" ⧺ show n
    Just τ  → τ 

instance AEq TClass where
  (≅) nτs α β = case α of
    TInt → case β of
      T1NA → Just TInt
      T1   → Just TInt
      _    → Nothing
    TFlt → case β of
      T1NA → Just TFlt 
      T1   → Just TFlt
      _    → Nothing
    TPtr → case β of
      T1NA → Just TPtr 
      T1   → Just TPtr
      _    → Nothing
    T1NA → case β of
      TAgg → Nothing
      _    → (≅) nτs β T1NA
    TAgg → (≅) nτs β TAgg
    T1   → Just β   

gepτs ∷ NamedTypes → Τα → [Int] → Τα
gepτs nτ τ [] = error "geps: no idxs"
gepτs nτ τ (i:j) = trace ("gettys: " ⧺ show τ ⧺ "\n" ⧺ show (i:j) ⧺ "\n") $ case τ of 
  TyDer (TyPtr τα τann) → 
    let τβ = foldl (gepτ nτ) τα j
    in  τβ
  _ →  error $ "gepτs: wrong type " ⧺ show τ

gepτ ∷ NamedTypes → Τα → Int → Τα
gepτ nτ τ idx =  trace ("getty: " ⧺ show τ ⧺ "\n" ⧺ show idx ⧺ "\n") $  case τ of
  TyDer (TyVec c τβ) → 
    if c > idx
    then τβ
    else error $ "gepTy: idx > c: " ⧺ show τ ⧺ " " ⧺ show idx 
  TyDer (TyAgg τα)  → case τα of
    TyArr   c τβ → τβ {-if c > idx
                   then τβ
                   else error $ "gepTy: idx > c: " ⧺ show τ ⧺ " " ⧺ show idx -}
    TyStr n c τβ → if length τβ > idx
                   then τβ !! idx
                   else case M.lookup n nτ of
                    Nothing → error $ "gepTy: " ⧺ show τ ⧺ "\n" ⧺ show τα ⧺ show idx
                    Just τη → case τη of
                      TyDer (TyAgg (TyStr n' c' τβ')) →
                        if n' ≡ n
                        then if length τβ' > idx
                             then τβ' !! idx
                             else error $ "gepTy(2): " ⧺ show τ ⧺ " " ⧺ show τη ⧺ " " ⧺ show idx
                        else gepτ nτ τη idx
  _ → error $ "gepTy: wrong type " ⧺ show τ