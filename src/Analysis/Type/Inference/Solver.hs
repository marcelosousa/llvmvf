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
import Analysis.Type.Memory.TyAnn as T hiding (trace)
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
tyEq pc nt ty1 ty2 = trace ("tyEq " ++ show ty1 ++ " " ++ show ty2) $ 
  case (≅) nt ty1 ty2 of
    Nothing → error $ "Type Unification failed in pc=: " ⧺ show pc ⧺ "\n" ⧺ show ty1 ⧺ "\n" ⧺ show ty2
    Just ty → ty

erasePC = M.map (S.fromList . M.keys)

solve ∷ NamedTypes → S.Set Τℂ' → Γ 
solve nt cs = trace ("solve init\n---------------\n" ++ cToString cs ++ "---------------\n") $
  let scs = S.fold (solveEqualType nt) S.empty cs -- type with type
      (counter, ncs) = liftCa scs                 -- remove ℂa 
      (table, ecs) = trace ("after solveEqualType & liftCa\n---------------\n" ++ cToString ncs ++ "---------------\n") $ 
                     buildHash ncs
      (ncounter, gtable, gecs, env) = trace ("after buildHash\n" ++ scToString (erasePC table) ++ "---------------\n" ++ cToString ecs ++ "---------------\n") $
                                      generalize table ecs counter
      (ncounter', ntable, necs, nenv) = trace ("after generalize\n---------------\n" ++ scToString (erasePC gtable) ++ "---------------\n" ++ cToString gecs ++ "---------------\n" ++ envToString env ++ "---------------\n") $
                                        solveGeps nt (ncounter, gtable, gecs, env)
      (renv, rtable) = trace ("after solveGeps\n---------------\n" ++ scToString (erasePC ntable) ++ "---------------\n" ++ envToString nenv ++ "---------------\n" ++ cToString necs ++ "---------------\n") $
                       rewrite nenv ntable 
      (itable, ienv) = Trace.trace ("after 1st rewrite\n---------------\n" ++ scToString (erasePC rtable) ++ "---------------\n" ++ cToString necs ++ "---------------\n" ++ envToString renv ++ "---------------\n") $
                       incorporate rtable renv necs
      (fenv, ftable) = trace ("after incorporate\n---------------\n" ++ scToString (erasePC itable) ++ "---------------\n" ++ envToString ienv ++ "---------------\n") $
                       rewrite ienv itable 
  in trace ("after 2nd rewrite\n---------------\n" ++ scToString (erasePC ftable) ++ "---------------\n" ++ envToString fenv ++ "---------------\n") $
     typify ftable fenv


-- Step 1 - Remove trivial constraints
solveEqualType ∷ NamedTypes → Τℂ' → S.Set Τℂ' → S.Set Τℂ'
solveEqualType nt (ℂτ ty1 :=: ℂτ ty2, pc) r = tyEq pc nt ty1 ty2 `seq` r
solveEqualType nt c r = S.insert c r

-- Step 2 - Remove ℂa constraints
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

-- Step 3 - Build an hash table 
-- Build huge hash table and separate
-- type ConstraintMap = M.Map Identifier (M.Map ℂ (S.Set Int))
buildHash ∷ S.Set Τℂ' → (ConstraintMap, S.Set Τℂ')
buildHash = S.foldr buildHashElement (M.empty, S.empty) 

buildHashElement ∷ Τℂ' → (ConstraintMap, S.Set Τℂ') → (ConstraintMap, S.Set Τℂ')
buildHashElement cn@(lhs :=: rhs, pc) (table, rest) = 
  case lhs of 
    ℂτ tl → case rhs of      
      ℂπ n  → (addConstraint n (lhs,pc) table, rest)
      ℂp c ta → case tl of
        TyDer (TyPtr ty tta) → 
          let (ntable, nrest) = buildHashElement (c :=: (ℂτ ty),pc) (table, rest)
          in (ntable, S.insert cn rest)
        _ → error $ "buildHashElement: Type Unification failed :" ++ show cn 
      ℂλ ca cr → case tl of
        TyDer (TyFun ta tr _) → 
          if length ca == length ta
          then let (ntable, nrest) = foldr (\(c,t) res → buildHashElement (c :=: (ℂτ t),pc) res) (table,rest) $ zip ca ta
                   (ntable', nrest') = buildHashElement (cr :=: (ℂτ tr), pc) (ntable, nrest)
               in (ntable', S.insert cn nrest')
          else error $ "buildHashElement: Argument list mismatch"
      _     → (table, S.insert cn rest)
    ℂπ n → case rhs of
      ℂπ m → let ntable = addConstraint n (rhs,pc) table
                 ntable' = addConstraint m (lhs,pc) ntable
             in (ntable', rest)
      ℂι c i a → (table, S.insert cn rest)
      _ → (addConstraint n (rhs,pc) table, rest)
    ℂι c i a → case rhs of
      ℂι d j b → let (ntable, nrest) = buildHashElement (c :=: d, pc) (table, rest)
                 in (ntable, S.insert cn nrest)
      ℂp d b → (table, S.insert cn rest)
      ℂλ a r → error $ "buildHashElement: Type Unification failed :" ++ show cn 
      ℂq d → (table, S.insert cn rest)
      _ → buildHashElement (rhs :=: lhs, pc) (table, rest)
    ℂp c a → case rhs of
      ℂp d b → let (ntable, nrest) = buildHashElement (c :=: d, pc) (table, rest)
               in (ntable, S.insert cn nrest)
      ℂλ a r → error $ "buildHashElement: Type Unification failed :" ++ show cn 
      ℂq d → (table, S.insert cn rest)
      _ → buildHashElement (rhs :=: lhs, pc) (table, rest)
    ℂλ a r → case rhs of
      ℂλ b s → 
        if length a == length b
        then let (ntable, nrest) = foldr (\(c,t) res → buildHashElement (c :=: t,pc) res) (table,rest) $ zip a b
                 (ntable', nrest') = buildHashElement (r :=: s, pc) (ntable, nrest)
             in (ntable', S.insert cn nrest')
        else error $ "buildHashElement: Argument list mismatch" 
      _ → buildHashElement (rhs :=: lhs, pc) (table, rest)
    ℂq c → (table, S.insert cn rest)
      
addConstraint ∷ Identifier → (ℂ,Int) → ConstraintMap → ConstraintMap
addConstraint n (c,pc) res = 
  case M.lookup n res of
    Nothing → M.insert n (M.singleton c $ S.singleton pc) res
    Just m  → let m' = M.insertWith S.union c (S.singleton pc) m
              in M.insert n m' res

-- Step 4 - Generate the type qualifier variables
generalize ∷ ConstraintMap → S.Set Τℂ' → Int → (Int, ConstraintMap, S.Set Τℂ', Env)
generalize mc oc n = let (c, nmc,env) = M.foldWithKey generalizeConstraints (n,M.empty,M.empty)  mc
                         (nc, goc,env') = S.fold generalizeTyConst (c,S.empty,env) oc
                     in trace "generalize" $ (nc, nmc, goc, env')

generalizeConstraints ∷ Identifier → M.Map ℂ (S.Set Int) → (Int, ConstraintMap,Env) → (Int, ConstraintMap,Env)
generalizeConstraints n cs (counter,mc,env) = 
  let (ncounter, ncs,env') = M.foldrWithKey generalizeConstraint (counter,M.empty,env) cs 
  in (ncounter, M.insert n ncs mc,env')

generalizeConstraint ∷ ℂ → S.Set Int → (Int, M.Map ℂ (S.Set Int),Env) → (Int, M.Map ℂ (S.Set Int),Env)
generalizeConstraint c pcs (counter, res,env) = 
  let (ncounter, nc,env') = generalizeCons counter c env
  in (ncounter, M.insert nc pcs res,env')

-- Step 5 - Solve geps
solveGeps ∷ NamedTypes → (Int, ConstraintMap, S.Set Τℂ', Env) → (Int, ConstraintMap, S.Set Τℂ', Env)
solveGeps nt (counter,table,cs,env) = S.foldr (nsolveGep nt) (counter, table, S.empty, env) cs

nsolveGep ∷ NamedTypes → Τℂ' → (Int, ConstraintMap, S.Set Τℂ', Env) → (Int, ConstraintMap, S.Set Τℂ', Env)
nsolveGep nt cn@(lhs :=: rhs,pc) (counter, table, rest, env) = --trace ("nsolveGep " ++ show cn) $ 
  let (nlhs,ncounter,ntable,nrest,nenv) = solveGepConstraint nt counter lhs table rest env
      (nrhs,ncounter',ntable',nrest',nenv') = solveGepConstraint nt ncounter rhs ntable nrest nenv
      ncn = (nlhs :=: nrhs,pc)
  in (ncounter',ntable', S.insert ncn nrest', nenv')

solveGepConstraint ∷ NamedTypes → Int → ℂ → ConstraintMap → S.Set Τℂ' → Env → (ℂ, Int, ConstraintMap, S.Set Τℂ', Env)
solveGepConstraint nt counter cn table rest env = 
  case cn of
    ℂτ ty → (cn,counter,table,rest,env)
    ℂπ n  → (cn,counter,table,rest,env)
    ℂι c idxs ca → 
      case solveGepType nt counter [] c idxs ca table rest env of
        (Nothing,_,_,_,_) → error $ "solveGepConstraint: couldn't solve " ++ show cn
        (Just (_,tyl),ncounter,ntable,nrest,nenv) → trace ("solved " ++ show tyl) $ (ℂτ tyl,ncounter,ntable,nrest,nenv)
    ℂp c ca → 
      let (nc,ncounter,ntable,nrest,nenv) = solveGepConstraint nt counter c table rest env
      in (ℂp nc ca,ncounter,ntable,nrest,nenv)
    ℂλ ca cr → 
      let foldrAux = \c (rest,counter,table,rest',env) → let (nc,ncounter,ntable,nrest,nenv) = solveGepConstraint nt counter c table rest' env
                                                         in (nc:rest,ncounter,ntable,nrest,nenv)
          (nca,ncounter,ntable,nrest,nenv) = foldr foldrAux ([],counter,table,rest,env) ca
          (ncr,ncounter',ntable',nrest',nenv') = solveGepConstraint nt ncounter cr ntable nrest nenv
      in (ℂλ nca ncr,ncounter',ntable',nrest',nenv')
    ℂq c → 
      let (nc,ncounter,ntable,nrest,nenv) = solveGepConstraint nt counter c table rest env
      in (ℂq nc,ncounter,ntable,nrest,nenv)

solveGepType ∷ NamedTypes → Int → [Id] → ℂ → [Int] → Ταρ → ConstraintMap → S.Set Τℂ' → Env → (Maybe (Τα,Τα), Int, ConstraintMap, S.Set Τℂ', Env)
solveGepType nt counter hist cn idxs ann table rest env = trace ("solveGepType " ++ show cn ++ " " ++ show idxs) $ 
  case cn of
    ℂτ ty → 
      let (ncounter,ety,gty) = trace ("expanding " ++ show ty ++ " " ++ show idxs) $ expandType nt counter ty idxs 
          nty = TyDer $ TyPtr gty ann
      in (Just (ety,nty), ncounter, table, rest, env)      
    ℂp c cann →
      case ann ≌ cann of
        Nothing → error $ "solveGepType: Type Unification error"
        Just nann → solveGepType nt counter hist c idxs nann table rest env
    ℂπ n → if n `elem` hist
           then (Nothing,counter,table,rest,env)
           else case M.lookup n table of
              Nothing → findCandidate [] (nt, counter, M.assocs table, table, rest, env) n idxs ann -- 
              Just cs →
                let acs = M.assocs cs
                in case mapSolveGep nt counter (n:hist) acs idxs ann table rest env of
                      (Nothing,ncounter,ntable,nrest,nenv) → (Nothing,ncounter,ntable,nrest,nenv)
                      (Just ((ety,ty),pc),ncounter,ntable,nrest,nenv) → 
                        let ntable' = addConstraint n (ℂτ ety,pc) ntable
                        in (Just (ety,ty),ncounter,ntable',nrest,nenv)
    ℂι c didxs da → 
          case solveGepType nt counter hist c didxs da table rest env of
            (Nothing,_,_,_,_) → error $ "solveGepType: couldn't solve " ++ show cn
            (Just (_,tyl),ncounter,ntable,nrest,nenv) → 
              let (ncounter',ety,gty) = expandType nt ncounter tyl idxs 
                  fty = TyDer $ TyPtr gty ann
                  nrest' = S.insert (ℂτ tyl :=: ℂτ ety,(-10)) nrest
              in (Just (ety,fty), ncounter', ntable, nrest',nenv) -- Missing adding ℂτ tyl :=: ℂτ ety
    ℂλ ca cr → error $ "solveGepType: Not allowed"
    ℂq c → error $ "solveGepType: Not allowed"

mapSolveGep ∷ NamedTypes → Int → [Id] → [(ℂ,S.Set Int)] → [Int] → Ταρ → ConstraintMap → S.Set Τℂ' → Env → (Maybe ((Τα,Τα),Int),Int,ConstraintMap,S.Set Τℂ',Env)
mapSolveGep nt counter hist [] idxs ann table rest env = (Nothing,counter,table,rest,env)
mapSolveGep nt counter hist ((c,pcs):xs) idxs ann table rest env = 
  case solveGepType nt counter hist c idxs ann table rest env of
    (Nothing,ncounter,ntable,nrest,nenv) → mapSolveGep nt ncounter hist xs idxs ann ntable nrest nenv
    (Just (ety,ty),ncounter,ntable,nrest,nenv) → (Just ((ety,ty),head $ S.toList pcs), ncounter, ntable, nrest,nenv)

--findCandidate ∷ [(Id,M.Map ℂ (S.Set Int))] → Id → Maybe Τα
findCandidate log (nt, counter, vals, table, rest, env) n idxs ann = 
  if n `elem` log
  then (Nothing,counter,table,rest,env)
  else let ncs = filter (hasN n) vals 
       in if null ncs
          then (Nothing,counter,table,rest,env)
          else case selectC log n ncs of
            Nothing → (Nothing,counter,table,rest,env)
            Just cs' → case cs' of
              ℂτ ty → 
                let (ncounter,ety,gty) = trace ("expanding " ++ show ty ++ " " ++ show idxs) $ expandType nt counter ty idxs 
                    nty = TyDer $ TyPtr gty ann
                    nrest = S.insert (ℂπ n :=: ℂτ ety, (-1)) rest
                in (Just (ety,nty), ncounter, table, nrest, env) 
              ℂπ m → findCandidate (n:log) (nt, counter, vals, table, rest, env) m idxs ann
 
hasN ∷ Id → (Id, M.Map ℂ (S.Set Int)) → Bool
hasN n (_,cs) = 
  let ncs = M.keys cs
  in any ((==) (ℂπ n)) ncs

selectC ∷ [Id] → Id → [(Id, M.Map ℂ (S.Set Int))] → Maybe ℂ
selectC log n [] = Nothing
selectC log n ((_,cs):ys) = 
  if n `elem` log
  then Nothing
  else case selectCAux (n:log) (M.keys cs) of
    Nothing → selectC log n ys
    Just c  → Just c

selectCAux ∷ [Id] → [ℂ] → Maybe ℂ
selectCAux log [] = Nothing
selectCAux log (c:cs) = 
  case c of 
    ℂτ _ → Just c
    ℂπ m → if m `elem` log
           then selectCAux log cs
           else Just c
    _ → selectCAux log cs

-- Step 4a
incorporate ∷ ConstraintMap → Env → S.Set Τℂ' → (ConstraintMap, Env)
incorporate table env cs = S.fold incorporateConstraint (table,env) cs

incorporateConstraint ∷ Τℂ' → (ConstraintMap, Env) → (ConstraintMap, Env)
incorporateConstraint cn@(ℂπ n :=: rhs, pc) (table, env) =
  let ntable = addConstraint n (rhs, pc) table
  in (ntable, env)
incorporateConstraint (ℂq cl :=: rhs,pc) (table, env) =
  case rhs of
      ℂq cr → let vcl = getTyQual table cl 
                  vcr = getTyQual table cr
              in case vcl of 
                Nothing → case vcr of 
                  Nothing → (table, env)
                  Just _ → error "mergeConstraint: probably invalid cast"
                Just (TyVar var) → case vcr of
                  Nothing → error "mergeConstraint: probably invalid cast"
                  Just rvcr  → (table, M.insertWith (++) var [rvcr] env)
incorporateConstraint (lhs@(ℂτ ty) :=: rhs@(ℂp _ _),pc) (table, env) = 
  let (_,nenv,ntable) = fuse table env table lhs rhs
  in (ntable,nenv)
incorporateConstraint cn _ =  error ("incorporateConstraint " ++ show cn) 

-- Step 4b - Fix point and solve 
-- The termination condition is the cardinality of the all the elements in the table to be one
rewrite ∷ Env → ConstraintMap → (Env, ConstraintMap)
rewrite env table = M.foldWithKey (steprewrite table) (env, M.empty) table

steprewrite ∷ ConstraintMap → Identifier → M.Map ℂ (S.Set Int) → (Env, ConstraintMap) → (Env, ConstraintMap)
steprewrite table n cn (env, gamma) = trace ("entering steprewrite " ++ show n ++ " " ++ show cn) $
  if M.size cn <= 1
  then trace ("steprewrite inserting") $  (env, M.insert n cn gamma)
  else let (cns,pcs) = unzip $ M.assocs cn
           (c, nenv,ngamma) = foldr (\c1 (c2,env,gamma) → fuse table env gamma c1 c2) (head cns, env, gamma) $ tail cns
       in (nenv, M.insert n (M.singleton c (S.unions pcs)) ngamma)

-- Important fun
fuse ∷ ConstraintMap → Env → ConstraintMap → ℂ → ℂ → (ℂ, Env, ConstraintMap)
fuse table env gamma lhs rhs = trace ("entering fuse " ++ show lhs ++ " " ++ show rhs) $
  case lhs of
    ℂτ t   → fuseWithType table env gamma t rhs
    ℂπ m   → case grabNonVar m gamma of 
        Nothing → case grabNonVar m table of 
          Nothing → (lhs, env, M.insert m (M.singleton rhs S.empty) gamma)
          Just ct → fuse table env gamma ct rhs
        Just ct → fuse table env gamma ct rhs
    ℂp c a → fuseWithPtr  table env gamma c a rhs
    ℂλ a r → fuseWithFun  table env gamma a r rhs
    _ → error "fuse error: unsupported constraint"

fuseWithType ∷ ConstraintMap → Env → ConstraintMap → Τα → ℂ → (ℂ, Env, ConstraintMap)
fuseWithType table env gamma ty rhs = trace ("entering fuseWithType " ++ show rhs ++ " " ++ show ty) $
  case rhs of
    ℂτ tyr → let (nenv, nty) = mergeTypes env ty tyr
             in (ℂτ nty, nenv, gamma)
    ℂπ m   → fuse table env gamma rhs (ℂτ ty)
    ℂp c a → case ty of
        TyDer (TyPtr typ ann) → 
          let (na, nenv) = addToEnv a ann env
              (nc, nenv',gamma') = fuseWithType table nenv gamma typ c
          in (ℂp c na, nenv', gamma')
        _ → error "fuseWithType: type is not pointer"
    ℂλ a r → case ty of
        TyDer (TyFun ta tr _) → 
          if length ta /= length a
          then error "fuseWithType: functions have different argument list length"
          else let as = zip ta a
                   (na, nenv,ngamma) = fuseLambdaParameters table env gamma as
                   (nr, nenv',ngamma') = fuseWithType table nenv ngamma tr r
               in (ℂλ na nr, nenv',ngamma')
        _ → error "fuseWithType: type is not a function"
    _ → error "fuseWithType error: unsupported constraint"   
    where fuseLambdaParameters ∷ ConstraintMap → Env → ConstraintMap → [(Τα, ℂ)] → ([ℂ],Env,ConstraintMap)          
          fuseLambdaParameters table env gamma xs = trace "entering fuseLambdaParameters" $
            foldr (\(ta,ca) (cas,icm,igamma) → 
                let (c',cm',gamma') = fuseWithType table icm igamma ta ca
                in (c':cas,cm',gamma')) ([],env,gamma) xs

fuseWithPtr ∷ ConstraintMap → Env → ConstraintMap → ℂ → Ταρ → ℂ → (ℂ, Env, ConstraintMap)
fuseWithPtr table env gamma c ann rhs = trace "entering fuseWithPtr" $ 
  case rhs of
    ℂτ tyr → fuseWithType table env gamma tyr (ℂp c ann)
    ℂπ m   → fuse table env gamma rhs (ℂp c ann) 
    ℂp cr a → let (ncr, nenv,ngamma) = fuse table env gamma c cr 
                  (na, nenv') = addToEnv a ann nenv
              in (ℂp ncr na, nenv',ngamma) 
    ℂλ a r → error "fuseWithPtr error: CPtr with CFn"   
    _ → error "fuseWithPtr error: unsupported constraint"   

fuseWithFun ∷ ConstraintMap → Env → ConstraintMap → [ℂ] → ℂ → ℂ → (ℂ, Env, ConstraintMap)
fuseWithFun table env gamma ca cr rhs = trace "entering fuseWithFun" $
  case rhs of
    ℂτ tyr → fuseWithType table env gamma tyr (ℂλ ca cr)
    ℂπ m   → fuse table env gamma rhs (ℂλ ca cr)
    ℂp cr a → error "fuseWithFun error: CFn with CPtr"   
    ℂλ a r → 
      if length ca /= length a
      then error "fuseWithType: functions have different argument list length"
      else let as = zip ca a
               (na, nenv,ngamma) = fuseLambdaParameters table env gamma as
               (nr, nenv',ngamma') = fuse table nenv ngamma cr r
           in (ℂλ na nr, nenv',ngamma')
    _ → error "fuseWithFun error: unsupported constraint"   
    where fuseLambdaParameters ∷ ConstraintMap → Env → ConstraintMap → [(ℂ, ℂ)] → ([ℂ],Env,ConstraintMap)
          fuseLambdaParameters table env gamma xs = trace "entering fuseLambdaParameters2" $
            foldr (\(ta,ca) (cas,icm,igamma) → 
                let (c',cm',gamma') = fuse table icm igamma ta ca
                in (c':cas,cm',gamma')) ([],env,gamma) xs

addToEnv ∷ Ταρ → Ταρ → Env → (Ταρ,Env)
addToEnv AnyAddr       rhs        env = (rhs, env)
addToEnv lhs@(TyVar x) AnyAddr    env = (lhs, env)
addToEnv lhs@(TyVar x) rhs        env = (lhs, M.insertWith (++) x [rhs] env)
addToEnv UserAddr      UserAddr   env = (UserAddr, env)
addToEnv UserAddr      KernelAddr env = error $ "addToEnv: Unification error: User vs Kernel"
addToEnv KernelAddr    KernelAddr env = (KernelAddr, env)
addToEnv a             b          env = addToEnv b a env 

grabNonVar ∷ Id → ConstraintMap → Maybe ℂ
grabNonVar m table = grabNonVarAux [] m table 

grabNonVarAux ∷ [Id] → Id → ConstraintMap → Maybe ℂ
grabNonVarAux log m table = 
  if m `elem` log
  then Nothing
  else case M.lookup m table of
        Nothing → Nothing
        Just cm → 
          let (a,b) = foldr checkNonVar ([],[]) $ M.keys cm
          in if null a
             then foldr (\(ℂπ n) x → checkGrab (grabNonVarAux (m:log) n table) x) Nothing b
             else Just $ head a 

checkNonVar ∷ ℂ → ([ℂ],[ℂ]) → ([ℂ],[ℂ])
checkNonVar lhs@(ℂπ _) (a,b) = (a,lhs:b)
checkNonVar lhs        (a,b) = (lhs:a,b)

checkGrab ∷ Maybe a → Maybe a → Maybe a
checkGrab lhs@(Just _) rhs = lhs
checkGrab Nothing      rhs = rhs

checkCTy ∷ ℂ → ([Τα],[ℂ]) → ([Τα],[ℂ])
checkCTy lhs@(ℂτ ty) (a,b) = (ty:a,b)
checkCTy lhs         (a,b) = (a,lhs:b)

-- Retrieve a possible type of the table given an identifier
grabCTy ∷ [Id] → Id → ConstraintMap → Maybe Τα
grabCTy log n table = 
  if n `elem` log
  then Nothing
  else case M.lookup n table of
    Nothing → Nothing
    Just cm → case foldr checkCTy ([],[]) (M.keys cm) of
      ([],ys) → grabCTyAux table log ys
      (xs,ys) → Just $ head xs

grabCTyAux table log []     = Nothing
grabCTyAux table log (x:xs) = 
  case x of
    ℂπ n →
      case grabCTy log n table of
        Nothing → grabCTyAux table (n:log) xs
        Just ty → Just ty
    _ → case toType x of 
      Nothing → grabCTyAux table log xs
      Just t  → Just t

toType ∷ ℂ → Maybe Τα
toType (ℂτ ty) = Just ty
toType (ℂp c ann) = do  
  ty ← toType c 
  return $ TyDer $ TyPtr ty ann
toType (ℂλ ca cr) = do 
  tya ← mapM toType ca
  tyr ← toType cr
  return $ TyDer $ TyFun tya tyr False
toType _ = Nothing

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
      else if snlhs == snrhs 
           then if length tslhs > length tsrhs 
                then (env, lhs)
                else (env, rhs)
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

-- This is wrong: It should be [Ταρ]
getTyQual ∷ ConstraintMap → ℂ → Maybe Ταρ
getTyQual cm c = case c of
    ℂτ t → getTypeQual t
    ℂπ n → case grabCTy [] n cm of
        Nothing → error $ "getTyQual: " ++ show n ++ " is not in *env*"
        Just t → getTypeQual t
    ℂp cp ta → Just ta
    ℂι ca idxs ann → error "getTyQual: TODO GEP"
    _ → error "getTyQual: unsupported"

-- Step 9
typify ∷ ConstraintMap → Env → Γ 
typify table env =  M.mapWithKey (unfoldAndResolve table env) table

unfoldAndResolve ∷ ConstraintMap → Env → Id → M.Map ℂ (S.Set Int) → Τα
unfoldAndResolve table env n cn = 
  if M.size cn == 1
  then let c = head $ M.keys cn
       in resolveType table env n c  
  else error "unfoldAndResolve error" 

resolveType ∷ ConstraintMap → Env → Id → ℂ → Τα
resolveType table env n cn = trace ("resolveType " ++ show cn) $ 
  case cn of 
    ℂτ t → resolve env t
    ℂπ m → case M.lookup m table of
      Nothing → error "resolveType"
      Just cns → unfoldAndResolve table env m cns
    ℂp c ann → let ty = resolveType table env n c 
               in resolve env $ TyDer $ TyPtr ty ann
    ℂλ ca cr → let tyas = foldr (\a r → (resolveType table env n a):r) [] ca
                   tycr = resolveType table env n cr
                   ty = TyDer $ TyFun tyas tycr False
               in resolve env ty
    _ → error "resolveType: not supported"

(⊨) ∷ NamedTypes → Γ → S.Set Τℂ' → Γ
(⊨) = undefined

