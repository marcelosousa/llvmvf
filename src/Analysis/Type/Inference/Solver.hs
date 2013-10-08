{-# LANGUAGE UnicodeSyntax, FlexibleInstances, RecordWildCards, DoAndIfThenElse, NoMonomorphismRestriction #-}
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

cToString ∷ S.Set Τℂ' → String
cToString ts = foldr (\co r → show co ++ "\n" ++ r) "" (S.toList ts) 

mcToString ∷ ConstraintMap → String
mcToString ts = M.foldWithKey 
  (\k co r → show (pretty k) ++ ":~> " ++ (M.foldWithKey (\c _ r → show c ++ " || " ++ r) "" co) ++ "\n" ++ r) "" ts

scToString ∷ M.Map Identifier (S.Set ℂ) → String
scToString ts = M.foldWithKey 
  (\k co r → show (pretty k) ++ ":~> " ++ (S.fold (\c r → show c ++ " || " ++ r) "" co) ++ "\n" ++ r) "" ts

envToString ∷ Env → String
envToString ts = M.foldWithKey 
  (\k co r → k ++ ":-> " ++ show co ++ "\n" ++ r) "" ts

gammaToString ∷ Γ → String
gammaToString ts = M.foldWithKey 
  (\k co r → show (pretty k) ++ "= " ++ show co ++ "\n" ++ r) "" ts

------------------------------------------

type Γ = M.Map Id Τα
type Env = M.Map String [Ταρ]

type ConstraintMap = M.Map Identifier (M.Map ℂ (S.Set Int))

tyEq ∷ Int → NamedTypes → Τα → Τα → Τα
tyEq pc nt ty1 ty2 = Trace.trace ("tyEq " ++ show ty1 ++ " " ++ show ty2) $ 
  case (≅) nt ty1 ty2 of
    Nothing → error $ "Type Unification failed in pc=: " ⧺ show pc ⧺ "\n" ⧺ show ty1 ⧺ "\n" ⧺ show ty2
    Just ty → ty

erasePC = M.map (S.fromList . M.keys)

solve ∷ NamedTypes → Γ → S.Set Τℂ' → Γ 
solve nt y cs = Trace.trace ("solve init\n---------------\n" ++ cToString cs ++ "---------------\n") $
  let cs' = S.fold (solveEqualType nt) S.empty cs -- type with type
      (counter,ncs) = liftCa cs'
      (oc,ocs) = Trace.trace ("after solveEqualType & liftCa\n---------------\n" ++ cToString ncs ++ "---------------\n") $ 
                 prep nt ncs
      (y',rc)  = Trace.trace ("after prep\n---------------\n" ++ scToString (erasePC oc) ++ "---------------\n") $ 
                 collect nt oc y
      nrc      = Trace.trace ("after collect\n---------------\n" ++ scToString (erasePC rc) ++ "---------------\n") $ 
                 fixrewrite nt rc
      (nc,grc,nocs,tyq) = Trace.trace ("after fixrewrite\n---------------\n" ++ scToString (erasePC rc) ++ "---------------\n") $ 
                   generalize nrc ocs counter
      rrc = Trace.trace ("after generalize\n---------------\n" ++ scToString (erasePC grc) ++ "---------------\n") $
            replace grc
      (orc,ny) = Trace.trace ("after replace\n---------------\n" ++ scToString (erasePC rrc) ++ "---------------\n") $
                 liftPrimitive rrc y'
  in if not $ M.null orc
     then error "solve: something went wrong"
     else let (nc',ny',ntyq) = Trace.trace ("after lift\n---------------\n" ++ gammaToString ny ++ "\n" ++ envToString tyq ++ "---------------\n" ++ cToString nocs ++ "---------------\n") $ 
                               merge nt nc ny nocs tyq
              finaly = Trace.trace ("after merge\n---------------\n" ++ gammaToString ny' ++ "\n" ++ envToString ntyq ++ "---------------\n") $ 
                       typify ntyq ny'
          in  Trace.trace ("finaly\n---------------\n") $ finaly

-- Step 1 - Remove trivial constraints
solveEqualType ∷ NamedTypes → Τℂ' → S.Set Τℂ' → S.Set Τℂ'
solveEqualType nt (ℂτ ty1 :=: ℂτ ty2, pc) r = tyEq pc nt ty1 ty2 `seq` r
solveEqualType nt c r = S.insert c r

-- Step 2a - Remove ℂa constraints
liftCa ∷ S.Set Τℂ' → (Int, S.Set Τℂ')
liftCa = S.fold liftCaAux (0,S.empty) 

liftCaAux ∷ Τℂ' → (Int, S.Set Τℂ') → (Int, S.Set Τℂ')
liftCaAux (lhs :=: rhs, pc) (c,rest) = 
  let (clhs, nlhs, cslhs) = liftCAux c lhs pc 
      (crhs, nrhs, csrhs) = liftCAux clhs rhs pc
  in (crhs, S.insert (nlhs :=: nrhs, pc) $ S.union cslhs $ S.union csrhs rest)

liftCAux ∷ Int → ℂ → Int → (Int, ℂ, S.Set Τℂ')
liftCAux counter cn pc = case cn of
  ℂτ t → (counter, cn, S.empty)
  ℂπ n → (counter, cn, S.empty)
  ℂι c idxs a → let (ncounter, nc, r) = liftCAux counter c pc
                in (ncounter, ℂι nc idxs a, r)
  ℂp c a → let (ncounter, nc, r) = liftCAux counter c pc
           in (ncounter, ℂp nc a, r)
  ℂλ a r → let aux = \c (cn,la,rest) → let (ncn,nc,rc) = liftCAux cn c pc
                                       in (ncn,nc:la, S.union rc rest)
               (ncounter, na, csa) = foldr aux (counter,[],S.empty) a
               (ncounter', nr, csr) = liftCAux ncounter r pc
           in (ncounter', ℂλ na nr, S.union csr csa)
  ℂq c → let (ncounter, nc, r) = liftCAux counter c pc
         in (ncounter, ℂq nc, r)
  ℂa t c → let (ncounter,nt,_) = generalizeType counter t M.empty 
           in (ncounter, ℂτ nt, S.singleton (ℂq (ℂτ nt) :=: ℂq c, pc))                

-- Step 2b - Hash and simplify type
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
fixrewrite nt cm = Trace.trace "entering fixrewrite" $ 
  let cm' = M.foldrWithKey (steprewrite nt) M.empty cm
  in if cm == cm'
     then cm
     else fixrewrite nt cm'

steprewrite ∷ NamedTypes → Identifier → M.Map ℂ (S.Set Int) → ConstraintMap → ConstraintMap
steprewrite nt n cn cm = Trace.trace "entering steprewrite" $ 
  if M.size cn <= 1
  then M.insert n cn cm
  else let cns = M.assocs cn
           ((c,pcs), ncm) = foldr (rewrite nt n) (head cns, cm) $ tail cns
       in M.insert n (M.singleton c pcs) ncm

rewrite ∷ NamedTypes → Identifier → (ℂ,S.Set Int) → ((ℂ,S.Set Int), ConstraintMap) → ((ℂ,S.Set Int), ConstraintMap)
rewrite nt n (c1,pc1) ((c2,pc2),cm) = Trace.trace "entering rewrite" $ 
  let (c,icm) = fuse nt cm n c1 c2
  in ((c, S.union pc1 pc2), icm)

join ∷ ConstraintMap → ConstraintMap → ConstraintMap
join cm1 cm2 = M.unionWith (M.unionWith (S.union)) cm1 cm2

update ∷ Identifier → ℂ → ConstraintMap → ConstraintMap
update n c mc = Trace.trace "update" $ 
  case M.lookup n mc of
    Nothing → M.insert n (M.singleton c S.empty) mc
    Just cs → M.insert n (M.insertWith S.union c S.empty cs) mc

-- Important fun
fuse ∷ NamedTypes → ConstraintMap → Identifier → ℂ → ℂ → (ℂ, ConstraintMap)
fuse nt cm n lhs rhs = Trace.trace "entering fuse" $ 
  case lhs of
    ℂτ t → fuseWithType nt cm n t rhs
    ℂπ m → fuseWithVar  nt cm n m rhs 
    ℂp c a → fuseWithPtr nt cm n c a rhs
    ℂλ a r → fuseWithFun nt cm n a r rhs
    _ → error "fuse error: unsupported constraint"

fuseWithType ∷ NamedTypes → ConstraintMap → Identifier → Τα → ℂ → (ℂ, ConstraintMap)
fuseWithType nt cm n ty rhs = Trace.trace ("entering fuseWithType " ++ show rhs ++ " " ++ show ty) $ 
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
          fuseLambdaParameters nt cm n xs = Trace.trace "entering fuseLambdaParameters" $ 
            let (ta,c) = last xs
                (c',cm') = fuseWithType nt cm n ta c 
            in foldr (\(ta,ca) (cas,icm) → 
                let (c',cm') = fuseWithType nt icm n ta ca
                in (c':cas,cm')) ([c'],cm') (init xs)

fuseWithVar ∷ NamedTypes → ConstraintMap → Identifier → Identifier → ℂ → (ℂ, ConstraintMap)
fuseWithVar nt cm n m rhs = Trace.trace "entering fuseWithVar" $ 
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
fuseWithPtr nt cm n c ann rhs = Trace.trace "entering fuseWithPtr" $ 
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
fuseWithFun nt cm n ca cr rhs = Trace.trace "entering fuseWithFun" $ 
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
generalize ∷ ConstraintMap → S.Set Τℂ' → Int → (Int, ConstraintMap, S.Set Τℂ', Env)
generalize mc oc n = let (c, nmc,env) = M.foldWithKey generalizeConstraints (n,M.empty,M.empty)  mc
                         (nc, goc,env') = S.fold generalizeTyConst (c,S.empty,env) oc
                     in Trace.trace "generalize" $ (nc, nmc, goc, env')

generalizeConstraints ∷ Identifier → M.Map ℂ (S.Set Int) → (Int, ConstraintMap,Env) → (Int, ConstraintMap,Env)
generalizeConstraints n cs (counter,mc,env) = 
  let (ncounter, ncs,env') = M.foldrWithKey generalizeConstraint (counter,M.empty,env) cs 
  in (ncounter, M.insert n ncs mc,env')

generalizeConstraint ∷ ℂ → S.Set Int → (Int, M.Map ℂ (S.Set Int),Env) → (Int, M.Map ℂ (S.Set Int),Env)
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
-- At this point, all constraints should be Tτ
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
    ℂτ tyr → (cs, M.insert n tyr y)
    ℂp c ann → case simplify M.empty c of
      Nothing → (M.insert rhs pcs cs, y)
      Just ty → let tyr = TyDer $ TyPtr ty ann
                in (cs, M.insert n tyr y)
    _ → (M.insert rhs pcs cs, y)

-- Step 8 -- Incorporate other constraints and rewrite
merge ∷ NamedTypes → Int → Γ → S.Set Τℂ' → Env → (Int,Γ,Env)
merge nt counter cm cs env = 
  let (nc,y,env') = S.fold (mergeConstraint nt) (counter,cm,env) cs
  in Trace.trace (show env') $ (nc,y,env')

mergeConstraint ∷ NamedTypes → Τℂ' → (Int, Γ, Env) → (Int, Γ, Env)
mergeConstraint nt (lhs :=: rhs, pc) (counter,cm,env) = Trace.trace ("merge " ++ show lhs ++ " " ++ show rhs) $
  case lhs of
    ℂπ c → case rhs of
      ℂι ca idxs ann → 
        let (nc,tya,ncm) = solveGep nt counter cm ca idxs ann 
        in case M.lookup c cm of
          Nothing → (nc, M.insert c tya ncm, env)
          Just csm → let (nenv, nty) = mergeGepTypes env tya csm
                     in (nc, M.insert c nty ncm, nenv)
    ℂq cl → case rhs of
      ℂq cr → let TyVar var = Trace.trace ("getTyQual result = " ++ show (getTyQual cm cl) ++ " " ++ show cl) $ getTyQual cm cl 
                  vcr = getTyQual cm cr
              in (counter, cm, M.insertWith (++) var [vcr] env)

-- Computes the type of the gep constraint; generates the appropriate unfolded type
solveGep ∷ NamedTypes → Int → Γ → ℂ → [Int] → Ταρ → (Int, Τα, Γ)
solveGep nt counter env rhs idxs ann =  
  case rhs of
    ℂτ t → let (nc, ty, gepty) = expandType nt counter t idxs
           in (nc, TyDer (TyPtr gepty ann), env)
    ℂπ m → case M.lookup m env of
              Nothing → error $ "solveGep: " ++ show m ++ " is not in *env*"
              Just t  → let (nc, ty, gepty) = expandType nt counter t idxs
                            nty = TyDer (TyPtr gepty ann)
                            nenv = M.insert m ty env
                        in (nc, nty, nenv) 
    _ → error $ "solveGep: unsupported constraint " ++ show rhs

mergeGepTypes ∷ Env → Τα → Τα → (Env, Τα)
mergeGepTypes env (TyDer (TyPtr lhs lann)) (TyDer (TyPtr rhs rann)) = 
  let (enva,ann) = addTyConstr env lann rann
      (nenv,ty)  = mergeTypes enva lhs rhs
  in (nenv, TyDer $ TyPtr ty ann)
mergeGepTypes env lhs rhs = error $ "mergeGepTypes: Unification error\n" ++ show lhs ++ "\n" ++ show rhs

mergeTypes ∷ Env → Τα → Τα → (Env, Τα)
mergeTypes env lhs rhs = 
  case (lhs,rhs) of
    (TyBot, TyBot) → (env,lhs)
    (TyUndef, TyUndef) → (env,lhs) 
    (TyPri plhs, TyPri prhs) → 
      let (nenv, ty) = mergePriTypes env plhs prhs
      in (nenv, TyPri ty)
    (TyDer dlhs, TyDer drhs) →
      let (nenv, ty) = mergeDerTypes env dlhs drhs
      in (nenv, TyDer ty)
    _ → error $ "mergeTypes: Unification error\n" ++ show lhs ++ "\n" ++ show rhs

mergePriTypes ∷ Env → TyPri → TyPri → (Env, TyPri)
mergePriTypes env lhs rhs = 
  if lhs ≡ rhs
  then (env, lhs)
  else error $ "mergePriTypes: Unification error\n" ++ show lhs ++ "\n" ++ show rhs

mergeDerTypes ∷ Env → TyDer → TyDer → (Env, TyDer)
mergeDerTypes env lhs rhs = 
  case (lhs,rhs) of
    (TyAgg alhs, TyAgg arhs) → 
      let (nenv, ty) = mergeAggTypes env alhs arhs
      in (nenv, TyAgg ty)
    (TyVec n vlhs, TyVec m vrhs) → 
      let (nenv, ty) = mergeTypes env vlhs vrhs
      in if n == m 
         then (nenv, TyVec n ty)
         else error $ "mergeDerTypes: Unification error\n" ++ show lhs ++ "\n" ++ show rhs
    (TyFun alhs rlhs vlhs, TyFun arhs rrhs vrhs) → 
      if vlhs 
      then (env, lhs)
      else if vrhs
           then (env, rhs)
           else if length alhs == length arhs
                then let args = zip alhs arhs
                         (nenv, argtys) = foldr (\(l,r) (e,lty) → 
                                          let (ne,ty) = mergeTypes e l r 
                                          in (ne,ty:lty)) (env,[]) args 
                         (nenv', rty) = mergeTypes nenv rlhs rrhs
                      in (nenv', TyFun argtys rty vlhs) 
                else error $ "mergeDerTypes: Unification error\n" ++ show lhs ++ "\n" ++ show rhs
    (TyPtr plhs annlhs, TyPtr prhs annrhs) →
      let (nenv, ty) = mergeTypes env plhs prhs
          (nenv', tyann) = addTyConstr nenv annlhs annrhs
      in (nenv', TyPtr ty tyann) 
    _ → error $ "mergeDerTypes: Unification error\n" ++ show lhs ++ "\n" ++ show rhs

mergeAggTypes ∷ Env → TyAgg → TyAgg → (Env, TyAgg)
mergeAggTypes env lhs rhs = 
  case (lhs,rhs) of
    (TyArr n tlhs, TyArr m trhs) → 
      if n == m 
      then let (nenv, ty) = mergeTypes env tlhs trhs
           in (nenv, TyArr n ty)
      else error $ "mergeAggTypes: Unification error\n" ++ show lhs ++ "\n" ++ show rhs
    (TyStr snlhs n tslhs, TyStr snrhs m tsrhs) → 
      if length tslhs == length tsrhs
      then let nn = length tslhs
               nsn = mergeNames snlhs snrhs
               args = zip tslhs tsrhs
               (nenv, argtys) = foldr (\(l,r) (e,lty) → 
                                let (ne,ty) = mergeTypes e l r 
                                in (ne,ty:lty)) (env,[]) args
           in (nenv, TyStr nsn nn argtys)
      else error $ "mergeAggTypes: Unification error\n" ++ show lhs ++ "\n" ++ show rhs


mergeNames ∷ String → String → String
mergeNames "" rhs = rhs
mergeNames lhs "" = lhs
mergeNames lhs rhs = 
  if lhs == rhs
  then lhs
  else error $ "mergeNames: Unification error\n" ++ show lhs ++ "\n" ++ show rhs

addTyConstr ∷ Env → Ταρ → Ταρ → (Env,Ταρ)
addTyConstr env lhs@(AnyAddr) rhs = (env,rhs)
addTyConstr env lhs rhs@(AnyAddr) = (env,lhs)
addTyConstr env lhs@(TyVar s) rhs = (M.insertWith (++) s [rhs] env, lhs)
addTyConstr env lhs rhs@(TyVar s) = (M.insertWith (++) s [lhs] env, rhs)
addTyConstr env lhs rhs = 
  case lhs ≌ rhs of
    Nothing → error $ "addTyConstr: Unification error\n" ++ show lhs ++ "\n" ++ show rhs
    Just ann → (env,ann)


getTyQual ∷ Γ → ℂ → Ταρ
getTyQual cm c = case c of
    ℂτ t → getTypeQual t
    ℂπ n → case M.lookup n cm of
        Nothing → error $ "getTyQual: " ++ show n ++ " is not in *env*"
        Just t → getTypeQual t
    ℂp cp ta → ta
    ℂι ca idxs ann → error "getTyQual: TODO GEP"
    _ → error "getTyQual: unsupported"

-- Step 9
typify ∷ Env → Γ → Γ 
typify env types = M.map (resolve env) types

(⊨) ∷ NamedTypes → Γ → S.Set Τℂ' → Γ
(⊨) = undefined

