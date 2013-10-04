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
import Analysis.Type.Util
--import Analysis.Type.Inference.Initial

import Prelude.Unicode ((⧺),(≡))
import Data.List.Unicode ((∈))

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Maybe as MB
import Data.List

import Control.Applicative
import Control.Monad hiding (join)
import Control.Monad.State hiding (join)

import qualified Debug.Trace as Trace
import UU.PPrint hiding ((<$>))

trace s f = f
--trace = Trace.trace

cToString ∷ NamedTypes → S.Set Τℂ' → String
cToString nt ts = foldr (\co r → showType nt co ++ "\n" ++ r) "" (S.toList ts) 

mcToString ∷ NamedTypes → ConstraintMap → String
mcToString nt ts = M.foldWithKey 
  (\k co r → show (pretty k) ++ ":~> " ++ (M.foldWithKey (\c _ r → showType nt c ++ " || " ++ r) "" co) ++ "\n" ++ r) "" ts

scToString ∷ NamedTypes → M.Map Identifier (S.Set ℂ) → String
scToString nt ts = M.foldWithKey 
  (\k co r → show (pretty k) ++ ":~> " ++ (S.fold (\c r → showType nt c ++ " || " ++ r) "" co) ++ "\n" ++ r) "" ts

envToString ∷ M.Map String Ταρ → String
envToString ts = M.foldWithKey 
  (\k co r → k ++ ":-> " ++ show co ++ "\n" ++ r) "" ts

------------------------------------------

references ∷ M.Map Identifier (S.Set ℂ) → M.Map Identifier Int
references m = M.fold countReference M.empty m

countReference ∷ S.Set ℂ → M.Map Identifier Int → M.Map Identifier Int
countReference cs m = S.fold (\c r → M.unionWith (+) (countReferences c) r) m cs

countReferences ∷ ℂ →  M.Map Identifier Int
countReferences c = case c of
  ℂτ _ → M.empty
  ℂπ n → M.singleton n 1
  ℂι c _ → countReferences c
  ℂp c _ → countReferences c
  ℂλ cs c → foldr (\a b → M.unionWith (+) (countReferences a) b) (countReferences c) cs
  ℂq c → countReferences c

type Γ = M.Map Id Τα

type ConstraintMap = M.Map Identifier (M.Map ℂ (S.Set Int))

tyEq ∷ Int → NamedTypes → Τα → Τα → Τα
tyEq pc nt ty1 ty2 =
  case (≅) nt ty1 ty2 of
    Nothing → error $ "Type Unification failed in pc=: " ⧺ show pc ⧺ "\n" ⧺ showType nt ty1 ⧺ "\n" ⧺ showType nt ty2
    Just ty → ty

erasePC = M.map (S.fromList . M.keys)

solve ∷ NamedTypes → Γ → S.Set Τℂ' → Γ 
solve nt y cs = Trace.trace ("solve init\n---------------\n" ++ cToString nt cs ++ "---------------\n") $
  let cs' = S.fold (solveEqualType nt) S.empty cs -- type with type
      (oc,ocs) = Trace.trace ("after solveEqualType\n---------------\n" ++ cToString nt cs' ++ "---------------\n") $ 
                 prep nt cs'
      (y',rc)  = Trace.trace ("after prep\n---------------\n" ++ scToString nt (erasePC oc) ++ "---------------\n") $ 
                 collect nt oc y
      nrc      = Trace.trace ("after collect\n---------------\n" ++ scToString nt (erasePC rc) ++ "---------------\n") $ 
                 fixrewrite nt rc
      (grc,nocs,tyq) = Trace.trace ("after fixrewrite\n---------------\n" ++ scToString nt (erasePC rc) ++ "---------------\n") $ 
                   generalize nrc ocs
      rrc = Trace.trace ("after generalize\n---------------\n" ++ scToString nt (erasePC grc) ++ "---------------\n") $
            replace grc
      (orc,ny) = Trace.trace (   "after replace\n---------------\n" ++ scToString nt (erasePC rrc) ++ "---------------\n") $
                 liftPrimitive rrc y'
  in Trace.trace ("after lift\n---------------\n" ++ scToString nt (erasePC orc) 
                  ++ "---------------\n" ++ envToString tyq ++ cToString nt nocs) $ ny

-- Step 1 - Remove trivial constraints
solveEqualType ∷ NamedTypes → Τℂ' → S.Set Τℂ' → S.Set Τℂ'
solveEqualType nt (ℂτ ty1 :=: ℂτ ty2, pc) r = tyEq pc nt ty1 ty2 `seq` r
solveEqualType nt c r = S.insert c r

-- Step 2 - Hash and simplify type
prep ∷ NamedTypes → S.Set Τℂ' → (ConstraintMap, S.Set Τℂ')
prep nt cs = 
  let (rcs, ecs) = S.partition (\(c1 :=: c2, _) → isComplexConstr c1 || isComplexConstr c2) cs
  in (S.foldr (hash nt) M.empty ecs, rcs)

hash ∷ NamedTypes → Τℂ' → ConstraintMap → ConstraintMap
hash nt (lhs@(ℂπ n) :=: rhs, pc) r = 
  case rhs of
    ℂτ t → addConstr nt n (rhs,pc) r 
    ℂπ m → if reachable n m r
           then r
           else addConstr nt n (rhs,pc) r 
    _ → case simplify nt rhs of
      Nothing → addConstr nt n (rhs,pc) r
      Just ty → addConstr nt n (ℂτ ty,pc) r
hash nt c _ = error $ "gather: should not happen " ++ show c 

-- Is n reachable from m given r?
reachable ∷ Id → Id → ConstraintMap → Bool
reachable n m r = 
  if n == m 
  then True
  else case M.lookup m r of
    Nothing → False
    Just mc → 
      let ids = nub $ concatMap vars $ M.keys mc
      in any (\o → reachable n o r) ids

addConstr ∷ NamedTypes → Identifier → (ℂ,Int) → ConstraintMap → ConstraintMap
addConstr nt n (c,pc) res = 
  case M.lookup n res of
    Nothing → M.insert n (M.singleton c $ S.singleton pc) res
    Just m  → case c of 
      ℂτ ty → let m' = M.fromList $ simplifyType nt (c,pc) $ M.assocs m
              in M.insert n m' res
      _ → let m' = M.insertWith S.union c (S.singleton pc) m
          in M.insert n m' res

simplifyType ∷ NamedTypes → (ℂ, Int) → [(ℂ,(S.Set Int))] → [(ℂ,(S.Set Int))]
simplifyType nt (c,i) [] = [(c,S.singleton i)] 
simplifyType nt (ℂτ ty1,i) ((ℂτ ty2,si):xs) = (ℂτ $ tyEq i nt ty1 ty2, S.insert i si):xs
simplifyType nt (ℂτ ty1,i) (x:xs) = x:(simplifyType nt (ℂτ ty1,i) xs) 

-- Step 3 - Process trivial 
collect ∷ NamedTypes → ConstraintMap → Γ → (Γ, ConstraintMap)
collect nt m y = 
  let (toProcess, rest) = M.partitionWithKey partitionCondition m
  in (M.foldrWithKey (\k im yy → process nt k (M.assocs im) yy) y toProcess,rest)
    where process ∷ NamedTypes → Identifier → [(ℂ,S.Set Int)] → Γ → Γ
          process nt k [(ℂτ ty, pcs)] y = case M.lookup k y of 
            Nothing → M.insert k ty y
            Just ty' → M.insert k (tyEq (head $ S.toList pcs) nt ty ty') y
          process nt k _ y = error "process: bad arguments"
          partitionCondition k m = isGlobalId k && M.size m == 1 &&  length (filter isℂτ (M.keys m)) == 1

-- Step 4 - Fix point and solve 
-- The termination condition is the cardinality of the all the elements in the table to be one
fixrewrite ∷ NamedTypes → ConstraintMap → ConstraintMap
fixrewrite nt cm = 
  let cm' = M.foldrWithKey (steprewrite nt) M.empty cm
  in if cm == cm'
     then cm
     else fixrewrite nt cm'

steprewrite ∷ NamedTypes → Identifier → M.Map ℂ (S.Set Int) → ConstraintMap → ConstraintMap
steprewrite nt n cn cm =
  if M.size cn <= 1
  then M.insert n cn cm
  else let cns = M.assocs cn
           ((c,pcs), ncm) = foldr (rewrite nt n) (head cns, cm) $ tail cns
       in M.insert n (M.singleton c pcs) ncm

rewrite ∷ NamedTypes → Identifier → (ℂ,S.Set Int) → ((ℂ,S.Set Int), ConstraintMap) → ((ℂ,S.Set Int), ConstraintMap)
rewrite nt n (c1,pc1) ((c2,pc2),cm) = 
  let (c,icm) = fuse nt cm n c1 c2
  in ((c, S.union pc1 pc2), icm)

join ∷ ConstraintMap → ConstraintMap → ConstraintMap
join cm1 cm2 = M.unionWith (M.unionWith (S.union)) cm1 cm2

update ∷ Identifier → ℂ → ConstraintMap → ConstraintMap
update n c mc = 
  case M.lookup n mc of
    Nothing → M.insert n (M.singleton c S.empty) mc
    Just cs → M.insert n (M.insertWith S.union c S.empty cs) mc

-- Important fun
fuse ∷ NamedTypes → ConstraintMap → Identifier → ℂ → ℂ → (ℂ, ConstraintMap)
fuse nt cm n lhs rhs = 
  case lhs of
    ℂτ t → fuseWithType nt cm n t rhs
    ℂπ m → fuseWithVar  nt cm n m rhs 
    ℂp c a → fuseWithPtr nt cm n c a rhs
    ℂλ a r → fuseWithFun nt cm n a r rhs
    _ → error "fuse error: unsupported constraint"

fuseWithType ∷ NamedTypes → ConstraintMap → Identifier → Τα → ℂ → (ℂ, ConstraintMap)
fuseWithType nt cm n ty rhs =
  case rhs of
    ℂτ tyr → (ℂτ $ tyEq (-1) nt ty tyr, cm)
    ℂπ m   → (rhs, update m (ℂτ ty) cm)
    ℂp c a → case ty of
        TyDer (TyPtr typ ann) → case (≌) a ann of
              Nothing → error $ "Unification failed in fuseWithType " ++ show a ++ " " ++ show ann
              Just na → let (nc, ncm) = fuseWithType nt cm n typ c
                        in (ℂp nc na, ncm)
        _ → error "fuseWithType: type is not pointer"
    ℂλ a r → case ty of
        TyDer (TyFun ta tr _) → 
          if length ta /= length a
          then error "fuseWithType: functions have different argument list length"
          else let as = zip ta a
                   (na, mcs) = fuseLambdaParameters nt cm n as
                   (nr, rmc) = fuseWithType nt mcs n tr r
               in (ℂλ na nr, rmc)
        _ → error "fuseWithType: type is not a function"
    _ → error "fuseWithType error: unsupported constraint"   
    where fuseLambdaParameters ∷ NamedTypes → ConstraintMap → Identifier → [(Τα, ℂ)] → ([ℂ],ConstraintMap)
          fuseLambdaParameters nt cm n [] = ([],cm)
          fuseLambdaParameters nt cm n xs = 
            let (ta,c) = last xs
                (c',cm') = fuseWithType nt cm n ta c 
            in foldr (\(ta,ca) (cas,icm) → 
                let (c',cm') = fuseWithType nt icm n ta ca
                in (c':cas,cm')) ([c'],cm') (init xs)

fuseWithVar ∷ NamedTypes → ConstraintMap → Identifier → Identifier → ℂ → (ℂ, ConstraintMap)
fuseWithVar nt cm n m rhs = 
  case rhs of
    ℂτ tyr → fuseWithType nt cm n tyr (ℂπ m)
    ℂπ o   → if reachable o m cm
             then (ℂπ m, cm)
             else if reachable m o cm
                  then (rhs, cm)
                  else (rhs, update m (ℂπ o) cm)
    ℂp c a → (ℂπ m, update m rhs cm)
    ℂλ a r → (ℂπ m, update m rhs cm)
    _ → error "fuseWithVar error: unsupported constraint"   

fuseWithPtr ∷ NamedTypes → ConstraintMap → Identifier → ℂ → Ταρ → ℂ → (ℂ, ConstraintMap)
fuseWithPtr nt cm n c ann rhs = 
  case rhs of
    ℂτ tyr → fuseWithType nt cm n tyr (ℂp c ann)
    ℂπ m   → fuseWithVar  nt cm n m   (ℂp c ann)
    ℂp cr a → let (ncr, ncm) = fuse nt cm n c cr 
              in case (≌) a ann of
                Nothing → error $ "Unification failed in fuseWithPtr " ++ show a ++ " " ++ show ann
                Just na → (ℂp ncr na, ncm) 
    ℂλ a r → error "fuseWithPtr error: CPtr with CFn"   
    _ → error "fuseWithPtr error: unsupported constraint"   

fuseWithFun ∷ NamedTypes → ConstraintMap → Identifier → [ℂ] → ℂ → ℂ → (ℂ, ConstraintMap)
fuseWithFun nt cm n ca cr rhs =
  case rhs of
    ℂτ tyr → fuseWithType nt cm n tyr (ℂλ ca cr)
    ℂπ m   → fuseWithVar  nt cm n m   (ℂλ ca cr)
    ℂp cr a → fuseWithPtr nt cm n cr a (ℂλ ca cr)
    ℂλ a r → 
      if length ca /= length a
      then error "fuseWithType: functions have different argument list length"
      else let as = zip ca a
               (na, mcs) = fuseLambdaParameters nt cm n as
               (nr, rmc) = fuse nt mcs n cr r
           in (ℂλ na nr, rmc)
    _ → error "fuseWithFun error: unsupported constraint"   
    where fuseLambdaParameters ∷ NamedTypes → ConstraintMap → Identifier → [(ℂ, ℂ)] → ([ℂ],ConstraintMap)
          fuseLambdaParameters nt cm n [] = ([],cm)
          fuseLambdaParameters nt cm n xs = 
            let (a,c) = last xs
                (c',cm') = fuse nt cm n a c 
            in foldr (\(ta,ca) (cas,icm) → 
                let (c',cm') = fuse nt icm n ta ca
                in (c':cas,cm')) ([c'],cm') (init xs)

-- Step 5 - Generate the type qualifier variables
generalize ∷ ConstraintMap → S.Set Τℂ' → (ConstraintMap, S.Set Τℂ', M.Map String Ταρ)
generalize mc oc = let (c, nmc,env) = M.foldWithKey generalizeConstraints (0,M.empty,M.empty)  mc
                       (_, goc,env') = S.fold generalizeTyConst (c,S.empty,env) oc
                   in (nmc, goc, env')

generalizeConstraints ∷ Identifier → M.Map ℂ (S.Set Int) → (Int, ConstraintMap,M.Map String Ταρ) → (Int, ConstraintMap,M.Map String Ταρ)
generalizeConstraints n cs (counter,mc,env) = 
  let (ncounter, ncs,env') = M.foldrWithKey generalizeConstraint (counter,M.empty,env) cs 
  in (ncounter, M.insert n ncs mc,env')

generalizeConstraint ∷ ℂ → S.Set Int → (Int, M.Map ℂ (S.Set Int),M.Map String Ταρ) → (Int, M.Map ℂ (S.Set Int),M.Map String Ταρ)
generalizeConstraint c pcs (counter, res,env) = 
  let (ncounter, nc,env') = generalizeCons counter c env
  in (ncounter, M.insert nc pcs res,env')

-- Step 6 - Rewrite again
replace ∷ ConstraintMap → ConstraintMap
replace cm = M.map (replaceConstraints cm) cm

replaceConstraints ∷ ConstraintMap → M.Map ℂ (S.Set Int) → M.Map ℂ (S.Set Int)
replaceConstraints ocm = M.mapKeys (replaceConstraint ocm)

replaceConstraint ∷ ConstraintMap → ℂ → ℂ
replaceConstraint cm rhs = 
  case rhs of
    ℂτ tyr → rhs
    ℂπ m   → 
      case M.lookup m cm of
        Nothing → error "replaceConstraint: can't find it!" 
        Just nc → if M.size nc == 1 
                  then replaceConstraint cm $ head $ M.keys nc
                  else error "replaceConstraint: more than one candidate"
    ℂp cr a → ℂp (replaceConstraint cm cr) a 
    ℂλ a r → ℂλ (map (replaceConstraint cm) a) (replaceConstraint cm r)
    _ → error "fuseWithPtr error: unsupported constraint"   

-- Step 7 -- Lift all primitive types
liftPrimitive ∷ ConstraintMap → Γ → (ConstraintMap, Γ)
liftPrimitive cm y = 
  M.foldWithKey liftPrimitiveConstraints (M.empty, y) cm 

liftPrimitiveConstraints ∷ Identifier → M.Map ℂ (S.Set Int) → (ConstraintMap,Γ) → (ConstraintMap,Γ)
liftPrimitiveConstraints n cs (mc,y) = 
  let (ncs,y') = M.foldWithKey (liftPrimitiveConstraint n) (M.empty,y) cs
  in if M.null ncs 
     then (mc,y')
     else (M.insert n ncs mc, y')

liftPrimitiveConstraint ∷ Identifier → ℂ → S.Set Int → (M.Map ℂ (S.Set Int), Γ) → (M.Map ℂ (S.Set Int), Γ)
liftPrimitiveConstraint n rhs pcs (cs, y) = 
  case rhs of
    ℂτ tyr → case tyr of
      TyDer _ → (M.insert rhs pcs cs, y)
      _ → (cs, M.insert n tyr y)
    _ → (M.insert rhs pcs cs, y)


(⊨) ∷ NamedTypes → Γ → S.Set Τℂ' → Γ
(⊨) = undefined
