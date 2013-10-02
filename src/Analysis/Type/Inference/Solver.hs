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
--import Analysis.Type.Inference.Initial

import Prelude.Unicode ((⧺),(≡))
import Data.List.Unicode ((∈))

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Maybe as MB

import Control.Applicative
import Control.Monad
import Control.Monad.State

import qualified Debug.Trace as Trace
import UU.PPrint hiding ((<$>))

trace s f = f
--trace = Trace.trace

data Ω = Ω 
  { 
    rc  ∷ S.Set Τℂ'  -- remaining constraints
  , mic ∷ M.Map Id ℂ -- current map
  , gepMap ∷ M.Map Id [ℂ] -- map for the gep instructions
  , gMap ∷ M.Map Id [ℂ]
  , nmdtys ∷ NamedTypes
  , gmap ∷ Γ -- global map
  }

instance Eq Ω where
  (Ω rc1 mic1 gm1 ggm1 _ _) == (Ω rc2 mic2 gm2 ggm2 _ _) = 
    rc1 ≡ rc2 && mic1 ≡ mic2 && gm1 ≡ gm2 && ggm1 ≡ ggm2 

νΤℂ ∷ Τℂ' → ΩState ()
νΤℂ τℂ = do γ@Ω{..} ← get
            let rc' = τℂ ∘ rc
            put γ{rc=rc'}

νIℂ ∷ Id → ℂ → ΩState ()
νIℂ i c = --trace ("Adding to map: " ⧺ show i ⧺ " " ⧺ show c) $ 
  do γ@Ω{..} ← get
     let mic' = M.insert i c mic
     put γ{mic=mic'}

νGℂ ∷ Id → ℂ → ΩState ()
νGℂ i c = --trace ("Adding to map: " ⧺ show i ⧺ " " ⧺ show c) $ 
  do γ@Ω{..} ← get
     let gepMap' = M.insertWith (⧺) i [c] gepMap
     put γ{gepMap=gepMap'}

νGVℂ ∷ Id → ℂ → ΩState ()
νGVℂ i c = --trace ("Adding to map: " ⧺ show i ⧺ " " ⧺ show c) $ 
  do γ@Ω{..} ← get
     let gMap' = M.insertWith (⧺) i [c] gMap
     put γ{gMap=gMap'}

δNτ ∷ ΩState NamedTypes
δNτ = do γ@Ω{..} ← get
         (↣) nmdtys

type ΩState α = State Ω α

iΩ c = Ω c M.empty M.empty M.empty

μrewriteEq ∷ ΩState ()
μrewriteEq = do --trace ("fixRewriteEq -----") $ 
  γ ← get
  let τℂs = S.toList $ rc γ
  put γ{rc = ε}
  mapM_ rewriteEq τℂs 
  γ' ← get
  if γ ≡ γ'
  then (↣) ()
  else μrewriteEq

rewriteEq ∷ Τℂ' → ΩState ()
rewriteEq (τℂ, pc) = --trace ("rewriteEq " ⧺ show τℂ) $ 
  case τℂ of
    c1 :=: c2 → do 
      rwEq pc c1 c2
      (↣) ()
    _  → νΤℂ (τℂ,pc)

-- rwEq
rwEq ∷ Int → ℂ → ℂ → ΩState ℂ
rwEq pc α β = trace ("rwEq " ⧺ show α ⧺ " " ⧺ show β) $ 
  case α of
    ℂτ τ     → rwEqτ pc α β -- type
    ℂι c i   → rwEqι pc α β -- gep
    ℂp c τα  → rwEqp pc α β -- pointer
    ℂλ ca cr → rwEqλ pc α β -- function
    ℂπ n     → rwEqπ pc α β -- var
    ℂq τ c   → rwEqq pc α β -- type tau is qualified by the qualifier in c 

traceString ∷ String → NamedTypes → ℂ → ℂ → String
traceString s nt α β = s ⧺ " " ⧺ showType nt α ⧺ " " ⧺ showType nt β

-- Type
rwEqτ ∷ Int → ℂ → ℂ → ΩState ℂ
rwEqτ pc α@(ℂτ τ1) β = do
  nτs ← δNτ
  trace (traceString "rwEqTy" nτs α β) $ case β of
    ℂτ τ2 → case (≅) nτs τ1 τ2 of
              Nothing → error $ "Type Unification failed in pc=: " ⧺ show pc ⧺ "\n" ⧺ showType nτs α ⧺ "\n" ⧺ showType nτs β
              Just c  → (↣) $ ℂτ c
    ℂc cl → if τ1 `classOf` cl 
            then (↣) α
            else error $ "rwEqTy " ⧺ showType nτs τ1 ⧺ " not class of " ⧺ show cl
    _ → rwEq pc β α                       
rwEqτ _ _ _ = error $ "rwEqTy: FATAL"

rwEqq ∷ Int → ℂ → ℂ → ΩState ℂ
rwEqq pc α@(ℂq τ c) β = do
  νΤℂ (α :=: β,pc)
  (↣) α

-- Type Var
rwEqπ ∷ Int → ℂ → ℂ → ΩState ℂ
rwEqπ pc α@(ℂπ n) β@(ℂι x idxn) = do
  νGℂ n β
  (↣) α
rwEqπ pc α@(ℂπ n) β = do   
  initProcessing n β
  nτs ← δNτ
  γ@Ω{..} ← get
  trace (traceString "rwEqv (start)" nτs α β) $ case M.lookup n mic of
    Nothing → trace (traceString "rwEqv (not in map)" nτs α β) $ do
      νIℂ n β
      (↣) β
    Just ζ  → case ζ of
      ℂπ m → if n ≡ m
             then trace ("rwEqv (in map, Cv equal): " ⧺ show (pretty n) ⧺ " " ⧺ show (pretty m)) $ do 
              νIℂ n β
              (↣) $ general ζ β
             else case β of
                ℂπ o → if n ≡ o || o ≡ m
                       then trace ("rwEqv (in map, Cv diff but messy): " ⧺ show (pretty n) ⧺ " " ⧺ show (pretty m) ⧺ " " ⧺ show (pretty o)) $ (↣) β
                       else trace ("rwEqv (in map, Cv diff but messy (2)): " ⧺ show (pretty n) ⧺ " " ⧺ show (pretty m) ⧺ " " ⧺ show (pretty o)) $ do 
                        νIℂ m β
                        (↣) β
                _ → do 
                  if mutual m [n] mic
                  then trace ("rwEqv (in map, Cv dif and not Cv but mutual): " ⧺ show (pretty n) ⧺ " " ⧺ show (pretty m) ⧺ " " ⧺ showType nτs β) $ do 
                    νIℂ n β
                    νIℂ m β
                    (↣) β
                  else trace ("rwEqv (in map, Cv dif, not Cv, not mutual): " ⧺ show (pretty n) ⧺ " " ⧺ show (pretty m) ⧺ " " ⧺ showType nτs β) $ do 
                    c ← rwEq pc ζ β
                    νIℂ n c
                    (↣) c
      _    → do c ← trace (traceString ("rwEqv (in map but not Cv): calling rwEq " ⧺ show (pretty n)) nτs β ζ) $ rwEq pc β ζ 
                νIℂ n c
                (↣) $ general ζ β
rwEqπ _ _ _ = error $ "rwEqπ: FATAL"

general ∷ ℂ → ℂ → ℂ 
general α@(ℂπ n) β@(ℂτ τ1) = α
general β@(ℂτ τ1) α@(ℂπ n) = α
general α β = α

initProcessing ∷ Id → ℂ → ΩState ()
initProcessing n@(Local _) β = (↣) ()
initProcessing n@(Global _) β = νGVℂ n β
 -- if not (n `elem` iIds)
 -- then νGVℂ n β
 -- else (↣) ()

-- Resolve GEP
rwGEP ∷ String → ℂ → ℂ → ΩState ℂ
rwGEP x α@(ℂι (ℂπ n) idxn) β = Trace.trace ("rwGEP unsound " ++ show α ++ " " ++ show β) $ return β 

mutual ∷ Id → [Id] → M.Map Id ℂ → Bool
mutual n l γ | n ∈ l = True
             | otherwise = 
  case M.lookup n γ of
    Nothing → False
    Just (ℂπ m) → mutual m (n:l) γ
    Just _ → False

-- Type Function
rwEqλ ∷ Int → ℂ → ℂ → ΩState ℂ
rwEqλ pc α@(ℂλ ca1 cr1) β = do
  nτs ← δNτ
  trace (traceString "rwEqFn" nτs α β) $ case β of
    ℂλ ca2 cr2 → do
      ca ← mapM (uncurry (rwEq pc)) $ zip ca1 ca2
      cr ← rwEq pc cr1 cr2
      (↣) $ ℂλ ca cr
    ℂc c → error $ "rwEqλ: class constraint"
    ℂτ τ → case τ of
      TyDer (TyFun τa τr _) →
        if length ca1 ≡ length τa
        then do
          mapM (\(a,b) → rwEq pc a b) $ zip ca1 (map ℂτ τa)
          rwEq pc cr1 (ℂτ τr)
          (↣) $ β
        else error $ "rwEqλ: function type has different arity"          
      _ → error $ "rwEqλ: type given is not a function type"
    _    → rwEq pc β α
rwEqλ _ _ _ = error $ "rwEqλ: FATAL"

-- Type gep
rwEqι ∷ Int → ℂ → ℂ → ΩState ℂ
rwEqι pc α@(ℂι (ℂπ n) idxn) β = Trace.trace ("rwEqGEP unsound " ++ show α ++ " " ++ show β) $ return β 

-- Type pointer
-- Missing ℂc
rwEqp ∷ Int → ℂ → ℂ → ΩState ℂ
rwEqp pc α@(ℂp c1 τα1) β = do
  nτs ← δNτ
  trace (traceString "rwEqp" nτs α β) $ case β of
    ℂτ τ → case τ of
      TyDer (TyPtr τ1 τα2) → 
        case τα1 ≌ τα2 of
          Just τα → do c ← rwEq pc c1 (ℂτ τ1)
                       case c of
                        ℂτ τ' → (↣) $ ℂτ $ TyDer $ TyPtr τ' τα
                        _     → Trace.trace ("rwEqp warning: impossible case? " ⧺ showType nτs c) $ (↣) $ c
          Nothing → error $ "(rwEqp error) Type qualifier mismatch " ⧺ show pc ⧺ "\n" ⧺ showType nτs α ⧺ "\n" ⧺ showType nτs β
      _ → error $ traceString "rwEqp: types dont match" nτs α β
    ℂp c2 τα2 → 
      case τα1 ≌ τα2 of
        Just τα → do c ← rwEq pc c1 c2
                     (↣) $ ℂp c τα
        Nothing → error $ traceString "rwEqp error:" nτs α β
    ℂλ ca cr → error "rwEqp: cant constraint pointer with function"
    ℂc cc → case cc of
      T1 → (↣) $ α
      TPtr → (↣) $ α
      _ → error "rwEqp: type class given is not compatible with pointer"
    _ → rwEq pc β α
rwEqp _ _ _ = error $ "rwEqp: FATAL"

------------------------------------------

type Γ = M.Map Id Τα

(⊨) ∷ NamedTypes → Γ → S.Set Τℂ' → Γ
(⊨) nτ e τℂ = let γ@Ω{..} = execState μrewriteEq (iΩ τℂ nτ e)
                  -- Solve simple first NamedTypes → Γ → Id → ℂ → (Γ, [ℂ])
                  (y1,mic') = M.foldWithKey (\n c (r,y) → solveEqualSimple nτ (r,y) n c) (e,M.empty) mic
                  y2 = trace (show gepMap) $ M.foldWithKey (solveGeps nτ) y1 gepMap
                  y3 = trace (show y1) $ S.fold (solveCast nτ) y2 rc                   
                  y4 = trace (show mic') $ M.foldWithKey (\n c y → solveEqual nτ n y mic' c) y3 mic'                
              in y4 --Trace.trace (show y4) M.foldWithKey (solveGlobal nτ) y4 gMap 
      
traceSolveEq ∷ NamedTypes → M.Map Id ℂ → String
traceSolveEq nτ = M.foldrWithKey (\k v r → show (pretty k) ⧺ " , " ⧺ showType nτ v ⧺ "\n" ⧺ r ) ""

solveEqual ∷ NamedTypes → Id → Γ → M.Map Id ℂ → ℂ → Γ
solveEqual nt n y mic c = trace ("solveEqual " ++ show n ++ " " ++ show c) $ 
  let ty1 = solveEq nt y mic [n] c
  in trace ("solveEqual possible candidate " ++ show ty1) $ case M.lookup n y of
    Nothing → M.insert n ty1 y
    Just ty2 → case (≅) M.empty ty1 ty2 of
        Nothing  → error $ "Type Unification failed in solveEqual\n" ++ showType nt ty1 ++"\n" ++ showType nt ty2
        Just ty → trace ("solveEqual inserting " ++ show ty) $ M.insert n ty y

--SolveGlobal
solveGlobal ∷ NamedTypes → Id → [ℂ] → Γ → Γ
solveGlobal nt n lc y =
  let lty = map (solveEq nt y M.empty []) lc
      ty  = foldr unifyGlobal (head lty) $ tail lty
  in M.insert n ty y

unifyGlobal t1 t2 = case (≅) M.empty t1 t2 of
        Nothing  → error $ "Type Unification failed in solveGlobal\n" ++ showType M.empty t1 ++"\n" ++ showType M.empty t2
        Just nty → nty

-- SolveGeps
solveGeps ∷ NamedTypes → Id → [ℂ] → Γ → Γ
solveGeps nt n lc y = foldr (solveGep nt n) y lc

-- Cv(%tmp1) :=: Cgep(Cv(%x),[0,0])
solveGep ∷ NamedTypes → Id → ℂ → Γ → Γ
solveGep nt n α@(ℂι (ℂπ x) []) γ = error "empty indices list"
solveGep nt n α@(ℂι (ℂπ x) idxn) γ = trace ("solveGep " ++ show n ++ " " ++ show x ++ show idxn)$  
  let unWrap = \m x → MB.fromMaybe (error $ "solveGep " ++ show m ++ " " ++ show n ++ " " ++ show α) x   
      (tn,tx) = (unWrap n (M.lookup n γ), unWrap x (M.lookup x γ)) -- lookup type of n and x
  in case tn of
    TyDer (TyPtr tni _) → trace ("solveGep2 " ++ show tn) $
      let wholeTypeX = getType nt tx
          updateTypeX = updateStructType nt wholeTypeX (tail idxn) tni
      in M.insert x updateTypeX γ
    _ → error "solveGep"
solveGep nt n α γ = error "solveGep"

getType ∷ NamedTypes → Τα → Τα
getType nt ty@(TyDer (TyPtr agg ann)) = 
  case agg of 
    TyDer (TyAgg (TyStr sn i [])) ->
      case M.lookup sn nt of
        Nothing → error "getType failed"
        Just t  → TyDer (TyPtr t ann)
    TyDer (TyAgg (TyStr sn i l)) -> ty
    _ -> ty 
getType nt ty = error $ "getType " ++ showType nt ty

updateStructType ∷ NamedTypes -> Τα → [Int] → Τα → Τα
updateStructType nt (TyDer (TyPtr ty1@(TyPri _) ann)) [] ty = 
  case (≅) nt ty ty1 of
    Nothing  → error $ "Type Unification failed in updateStructType\n" ++ showType nt ty ++"\n" ++ showType nt ty1
    Just nty → TyDer $ TyPtr nty ann
updateStructType nt tya@(TyDer (TyPtr agg ann)) []   ty = trace ("updateStructType 2 " ++ show tya ++ " " ++ show ty) $ 
  case (≅) nt agg ty of
    Nothing  → error $ "Type Unification failed in updateStructType\n" ++ showType nt agg ++"\n" ++ showType nt ty
    Just nty → TyDer $ TyPtr nty ann
updateStructType nt tya@(TyDer (TyPtr agg ann)) idxs ty = trace ("updateStructType " ++ show agg ++ show idxs ++ show ty) $
  case agg of
    TyDer (TyAgg (TyStr sn i [])) -> error "updateStructType: getType is wrong"
    TyDer (TyAgg (TyStr sn i l)) -> let agg' = unifyStructType nt agg idxs ty
                                    in TyDer $ TyPtr agg' ann
    _ -> tya
updateStructType _  _ _ _ = error "updateStructType: bad arguments"

unifyStructType ∷ NamedTypes -> Τα → [Int] → Τα → Τα
unifyStructType nt agg idxs ty = case agg of
    TyDer (TyAgg (TyStr sn i [])) -> 
      case M.lookup sn nt of
        Nothing -> error "unifyStructType"
        Just (TyDer (TyAgg (TyStr _ _ []))) -> error "unifyStructType 2"
        Just wty -> unifyStructType nt wty idxs ty
    TyDer (TyAgg (TyStr sn i l)) -> trace ("unifyStructType: " ++ show agg ++ show idxs) $
      case idxs of
        [] → error "empty indices list"
        [idx] → let ty' = l!!idx
                in case (≅) M.empty ty ty' of
                    Nothing  → error $ "Type Unification failed in unifyStructType\n" ++ showType M.empty ty ++"\n" ++ showType M.empty ty'
                    Just nty → TyDer $ TyAgg $ TyStr sn i $ replace idx nty l
        idx:idxs → let ty' = l!!idx
                       nty = unifyStructType nt ty' idxs ty
                   in TyDer $ TyAgg $ TyStr sn i $ replace idx nty l
    _ -> trace ("unifyStructType incomplete " ++ show agg) $ agg
 
replace ∷ Int → a → [a] → [a]
replace i a l = if length l <= i
                then error "cannot replace"
                else replace' i a l
    where replace' ∷ Int → a → [a] → [a]
          replace' 0 t (x:xs) = t:xs
          replace' n t (x:xs) = x:(replace (n-1) t xs)

-- Solve 
-- Goal
-- %tmp :: i8* IOAddr
-- %tmp1 :: (i8* IOAddr)* AnyAddr
-- %x :: struct.Device={i8* IOAddr}* AnyAddr
-- Input : Env, Constraints left
solveEqualSimple ∷ NamedTypes → (Γ, M.Map Id ℂ) → Id → ℂ → (Γ, M.Map Id ℂ)
solveEqualSimple nt (γ,cs) n c = 
  case solveEqSimple nt γ n c of
    Nothing → (γ,M.insert n c cs)
    Just t  → case M.lookup n γ of
      Nothing → (M.insert n t γ, cs)
      Just t1 → case (≅) M.empty t t1 of
                    Nothing  → error $ "Type Unification failed in unifyStructType\n" ++ showType nt t ++"\n" ++ showType nt t1
                    Just nty → (M.insert n nty γ, cs)

solveEqSimple ∷ NamedTypes → Γ → Id → ℂ → Maybe Τα
solveEqSimple nτ e n τℂ = trace ("solveEq " ⧺ show n) $ case τℂ of
  ℂτ τ → Just τ
  ℂπ m → Nothing
  ℂq τ c → Nothing
  ℂc cl → error $ "solve does not expect a class " ⧺ show n ⧺ showType nτ τℂ
  ℂι c is → M.lookup n $ solveGep nτ n τℂ e 
  ℂp c a → (\cτ → TyDer $ TyPtr cτ a) <$> solveEqSimple nτ e n c 
  ℂλ ca cr → do caτ ← mapM (solveEqSimple nτ e n) ca
                crτ ← solveEqSimple nτ e n cr
                (↣) $ TyDer $ TyFun caτ crτ False

solveEq ∷ NamedTypes → Γ → M.Map Id ℂ → [Id] → ℂ → Τα
solveEq nτ e γ n τℂ = trace ("solveEq " ⧺ show n) $ case τℂ of
  ℂτ τ → τ
  ℂπ m → look nτ e γ n m
  ℂc cl → error $ "solve does not expect a class " ⧺ show n ⧺ showType nτ τℂ
  ℂι c is → let cτ = solveEq nτ e γ n c
            in TyDer $ TyPtr (gepτs nτ cτ is) AnyAddr
  ℂp c a → let cτ = solveEq nτ e γ n c 
           in TyDer $ TyPtr cτ a  
  ℂλ ca cr → let caτ = map (solveEq nτ e γ n) ca
                 crτ = solveEq nτ e γ n cr
             in TyDer $ TyFun caτ crτ False
  ℂq τ c → let cτ = solveEq nτ e γ n c
           in case (τ,cτ) of
            (TyDer (TyPtr t1 _),TyDer (TyPtr _ a)) → TyDer (TyPtr t1 a)

look ∷ NamedTypes → Γ → M.Map Id ℂ → [Id] → Id → Τα 
look nτ e γ l m | m ∈ l = case M.lookup m e of 
                     Nothing → error "look: same identifiers and not in maps"
                     Just τ  → τ
                | otherwise = case M.lookup m γ of
                     Nothing → case M.lookup m e of
                       Nothing → error $ "look: not in maps " ⧺ show m ⧺ show l -- ⧺ show γ ⧺ show e
                       Just τ  → τ
                     Just c  → solveEq nτ e γ (m:l) c

solveCast ∷ NamedTypes → Τℂ' → Γ → Γ
solveCast nτ τℂ γ = case fst τℂ of
  (ℂq (TyDer (TyPtr t1 t1a)) (ℂπ x)) :=: (ℂτ t2) → 
    let (TyDer (TyPtr _ t1b)) = safeLookup x γ
    in case t1a ≌ t1b of
      Nothing → error $ "Type Unification failed solveCast\n" ++ show t1a ++"\n" ++ show t1b
      Just t1a' → case (≅) nτ (TyDer (TyPtr t1 t1a')) t2 of
        Nothing → error $ "Type Unification failed solveCast\n" ++ showType nτ (TyDer (TyPtr t1 t1a')) ++"\n" ++ showType nτ t2
        Just k  → M.insert x k γ
  c1 :=: c2 → Trace.trace ("solveCast unsound " ++ showType nτ τℂ) $ γ
  c1 :<: c2 → trace ("solveBit 1 " ++ showType nτ τℂ) $ solveBit (<) nτ γ c1 c2
  c1 :≤: c2 → trace ("solveBit 2 " ++ showType nτ τℂ) $ solveBit (<=) nτ γ c1 c2
  c1 :≌: c2 → trace "solveBit 3" $ solveBit (≡) nτ γ c1 c2

solveBit ∷ (Int → Int → Bool) → NamedTypes → Γ → ℂ → ℂ → Γ
solveBit op nτ γ α@(ℂπ n) β =
  let ατ = safeLookup n γ
  in case β of
    ℂτ βτ → if sizeOf ατ `op` sizeOf βτ
            then γ
            else error $ traceString "solveBit: bitsize mismatch" nτ α β
    ℂπ m  → let βτ = safeLookup m γ
            in if sizeOf ατ `op` sizeOf βτ
               then case (ατ, βτ) of 
                  (TyDer (TyPtr at ata), TyDer (TyPtr bt bta)) → 
                      case ata ≌ bta of
                      Nothing  → error $ "Type Unification failed in solveBit\n" ++ showType nτ ατ ++"\n" ++ showType nτ βτ
                      Just nta → M.insert n (TyDer (TyPtr at nta)) γ
                  (TyPri _, TyPri _) → γ 
                  _ → error $ "solveBit: TODO " ++ showType nτ ατ ++ " " ++ showType nτ βτ
               else error $ "solveBit: invalid cast\n"  ++ showType nτ α ++ " " ++ showType nτ ατ ++ "\n" ++ showType nτ β ++ " " ++ showType nτ βτ                
    _ → error "solveBit: beta is not supported"
solveBit op nτ γ α β@(ℂπ n) =
  let βτ = safeLookup n γ
  in case α of
    ℂτ ατ → if sizeOf ατ `op` sizeOf βτ
            then γ
            else error $ traceString "solveBit: bitsize mismatch" nτ α β
    _ → error "solveBit: beta is not supported"
solveBit op nτ γ α β = error $ "solveBit: not Cpi (" ⧺ showType nτ α ⧺ ", " ⧺ showType nτ β ⧺ ")"

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
gepτs nτ τ (i:j) = trace ("gettys: " ⧺ showType nτ τ ⧺ "\n" ⧺ show (i:j) ⧺ "\n") $ case τ of 
  TyDer (TyPtr τα τann) → 
    let τβ = foldl (gepτ nτ) τα j
    in  τβ
  _ →  error $ "gepτs: wrong type " ⧺ showType nτ τ

gepτ ∷ NamedTypes → Τα → Int → Τα
gepτ nτ τ idx =  trace ("getty: " ⧺ showType nτ τ ⧺ "\n" ⧺ show idx ⧺ "\n") $  case τ of
  TyDer (TyVec c τβ) → 
    if c > idx
    then τβ
    else error $ "gepTy: idx > c: " ⧺ showType nτ τ ⧺ " " ⧺ show idx 
  TyDer (TyAgg τα)  → case τα of
    TyArr   c τβ → τβ {-if c > idx
                   then τβ
                   else error $ "gepTy: idx > c: " ⧺ show τ ⧺ " " ⧺ show idx -}
    TyStr n c τβ → if length τβ > idx
                   then τβ !! idx
                   else case M.lookup n nτ of
                    Nothing → error $ "gepTy: " ⧺ showType nτ τ ⧺ "\n" ⧺ showType nτ τα ⧺ show idx
                    Just τη → case τη of
                      TyDer (TyAgg (TyStr n' c' τβ')) →
                        if n' ≡ n
                        then if length τβ' > idx
                             then τβ' !! idx
                             else error $ "gepTy(2): " ⧺ showType nτ τ ⧺ " " ⧺ showType nτ τη ⧺ " " ⧺ show idx
                        else gepτ nτ τη idx
  _ → error $ "gepTy: wrong type " ⧺ showType nτ τ

