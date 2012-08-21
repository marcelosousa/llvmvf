{-#LANGUAGE RecordWildCards, DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.ESEncoder
-- Copyright :  (c) 2012 Marcelo Sousa
-- Explicit State Model Checking
-------------------------------------------------------------------------------

module Concurrent.Model.ESEncoder (esencode) where

import Concurrent.Model                    hiding (State)

import Concurrent.Model.ESEncoder.Model (encType, encGlobalVars, encTransitions, sAnd, sOr, sFn)

import Concurrent.Model.Analysis.ControlFlow (ControlFlow(..))

import Language.LLVMIR

import Language.SMTLib2.Base
import Language.SMTLib2.Builder

import qualified Data.IntMap as IM
import qualified Data.Map as Map

import Data.Maybe
import Data.List (nub)

import Debug.Trace (trace)

import Control.Monad.State
 
-- In general, the problem of verifying two-threaded programs
-- (with unbounded stacks) is undecidable.
-- Constraint the scheduler by guard stregthening

-- To guarantee correctness, the scheduler must allow context-switch
-- that are conflicting, i.e., accesses whose relative execution
-- order can produce different global system states. 
-- 1. Persistent/ample set computations.
-- 2. Lock-set and/or lock-acquisition history analysis
-- 3. Conditional dependency

-- | Wrapper for main encode function. Builds the initial global state and wraps encModel in a State monad.
esencode :: (SCModel t) => Model t -> SMod
esencode m@Model{..} = let ccfg@ControlFlow{..} = controlflow m
                           cme = fromMaybe (error "encode")                  $ Map.lookup "main" cte  -- ^ Set the current PC to the initial pc of main
                           tvs = Map.map (\pci -> ThreadState pci Map.empty) $ Map.delete "main" cte  -- ^ Set the initial PC for each thread
                           s0  = GlobalState Map.empty cme Map.empty tvs                              -- ^ Initial state
                           (smod, sf) = runState (encModel m) s0                             
                       in smod --trace (show s0 ++ show sf) $ smod

-- | Main encode function.
encModel :: (SCModel t) => Model t -> State GlobalState SMod
encModel m = do tyenc <- encNmdTys  m   -- ^ Encode Named Types
                gvenc <- encGlobals m   -- ^ Encode Global Variables
              --  el <- encLocals  m 
                menc  <- encCon     m   -- ^ Encode 
                return $ preamble ++ tyenc ++ gvenc ++ menc ++ final

-- | Initial part of an smt module
preamble :: SExpressions
preamble = [ setlogic QF_AUFBV           -- ^ Closed quantifier-free formulas over the theory of bitvectors and bitvector arrays extended with free sort and function symbols.
           , setoption "produce-models"  -- ^ To be able to get values if satisfiable
           , declsort  "Pair" 2          -- ^ Declare the sort Pair
           , declsort  "Pointer" 1       -- ^ Declare the sort Pointer
           ]

-- | Final part of an smt module
final :: SExpressions
final = [ checksat 
        , exit 
        ]

-- | Useful sorts
usefulsorts :: [((Type,String), SExpression)]
usefulsorts = [ ((TyInt 8, "I8")  , defsorti  8)                 -- ^ Define I8 as _ BitVector 8
              , ((TyInt 32, "I32"), defsorti  32)                -- ^ Define I32 as _ BitVector 32
              , ((TyInt 64, "I64"), defsorti  64)                -- ^ Define I64 as _ BitVector 64
              ]

-- | Encode Named Types
encNmdTys :: (SCModel t) => Model t -> State GlobalState SExpressions
encNmdTys m@Model{..} = do gs@GlobalState{..} <- get
                           let (sts, sexprs) = unzip usefulsorts
                               defsorts' = Map.union defsorts $ Map.fromList sts
                               gs'       = gs { defsorts = defsorts' }
                           put gs'
                           sexprs0 <- forM (Map.toList nmdtys) encNmdTy
                           return $ sexprs ++ concat sexprs0

-- | Encode one named type 
encNmdTy :: (Id,Type) -> State GlobalState SExpressions
encNmdTy (i,ty) = do gs@GlobalState{..} <- get
                     let (defsorts',sexprs, sexpr) = encType ty (Just i) defsorts
                     put $ gs {defsorts = defsorts'}
                     return sexprs

-- | Encode Global Variables
encGlobals :: (SCModel t) => Model t -> State GlobalState SExpressions
encGlobals m@Model{..} = do gs <- get
                            let (gs', sexprs) = encGlobalVars gvars gs
                            put gs'
                            return sexprs

-- The transitions are calculated as in a sequential encoding.
-- The scheduling information is the one who then generates the real
-- formula.

type ModelT = Map.Map String Transitions

-- | Get the transitions
transitions :: (SCModel t) => Model t -> ModelT
transitions m@Model{..} = let fs = getFs mainf procs 
                              ccfg = controlflow m
                              cdfg = dataflow    m
                          in encTransitions fs ccfg cdfg decls 

-- | Encode 
encCon :: (SCModel t) => Model t -> State GlobalState SExpressions
encCon m = do let ts  = transitions m                              -- compute the transitions 
                  mts = case Map.lookup "main" ts of               -- get the transitions of main
                             Nothing -> error "encMain"      
                             Just mt -> mt                         
                  tts = Map.delete "main" ts           -- get thread transitions
              (msexprs, mexpr) <- trace (show $ length mts) $ applySeq mts
              gs@GlobalState{..} <- get
              let gs' = gs { currentpc = -1 }
              put gs'
--                  exprs = [ assert $ fromISExpr mexpr ]
--              trace (show gs') $ return $ msexprs ++ exprs
              (tsexprs, texpr) <- trace (show $ length $ Map.elems tts) $ applyCn 1 tts
              let exprs = [ assert $ fromISExpr mexpr, assert $ fromISExpr texpr ] 
              trace (show gs') $ return $ msexprs ++ tsexprs ++ exprs 

-- | Apply sequential transitions
applySeq :: Transitions -> State GlobalState (SExpressions, ISExpr)
applySeq ts = do s <- get
                 let e = enabledSeq s ts
                 if (length e == 0)
                 then return ([], ISEmpty)
                 else fireSeq e ts 

-- | Get enabled transitions
enabledSeq :: GlobalState -> Transitions -> Transitions
enabledSeq g@GlobalState{..} = filter (\(pci, g, _, _) -> currentpc == pci && g)  

fireSeq :: Transitions -> Transitions -> State GlobalState (SExpressions, ISExpr)
fireSeq [(_,_,gsf,pce)] ts = do s <- get
                                let (s',sexprs, sexpr) = gsf s
                                put s'
                                (sexprs', sexpr') <- applySeq ts
                                return( sexprs ++ sexprs', fuse sexpr sexpr')
fireSeq l               ts = do s <- get
                                let (gs,bbs) = unzip $ map (\t -> branch t s ts) l -- [SExpressions, ISExpr]
                                put $ stateFusion gs
                                let (sex, six) = unzip bbs
                                return (concat sex, branchFusion six)        
 
branch :: Transition -> GlobalState -> Transitions -> (GlobalState, (SExpressions, ISExpr))
branch (_,_,gsf,pce) s ts = let (s',exprs,expr) = gsf s
                                ((exprs',expr'), gs)  = runState (applySeq ts) s' 
                            in (gs,( exprs ++ exprs', fuse expr expr' ))

stateFusion :: [GlobalState] -> GlobalState
stateFusion gss = let agvals = map gvals gss
                      gvals' = Map.unions agvals
                      one    = head gss
                  in one { gvals = gvals' } 

fuse :: ISExpr -> ISExpr -> ISExpr
fuse (ISFunction f) x = f x
fuse x              y = sAnd x y   

branchFusion :: [ISExpr] -> ISExpr
branchFusion [] = error "branchFusion"
branchFusion [x] = x
branchFusion (x:xs) = sAnd x $ branchFusion xs

-- | Apply thread transitions
applyCn :: Int -> ModelT -> State GlobalState (SExpressions, ISExpr)
applyCn i ts = do s <- get
                  let e = enabledCn s ts
                      se = Map.size e
                  case trace (show $ Map.keys e) $ se of
                       0  -> return ([], ISEmpty)      -- no more transitions
                       l  -> do let mone = fireCn s e  -- give a map of with pci (si, sexprsi, sexpri) for each thread i
                                return $ explode i mone ts 

-- | Get enabled thread transitions 
enabledCn :: GlobalState -> Map.Map String Transitions -> Map.Map String Transitions
enabledCn g@GlobalState{..} mst = Map.filter (\t -> length t /= 0 ) $ Map.intersectionWith enabledTh ti mst  

enabledTh :: ThreadState -> Transitions -> Transitions
enabledTh th@ThreadState{..} = filter (\(pci, g, _, _) -> trace (show tipc ++ " " ++ show pci) $ tipc == pci && g)

type T = Map.Map Int [(GlobalState, SExpressions, ISExpr)]

-- | Fire the enable transitions
fireCn :: GlobalState -> Map.Map String Transitions -> T
fireCn gs mst = Map.foldrWithKey (\s ts -> fireTh gs s ts) Map.empty mst

-- | Fire the transitions of a thread
fireTh :: GlobalState -> String -> Transitions -> T -> T 
fireTh gs s [] t = t
fireTh gs s ts t = let ths = fromJust $  Map.lookup s $ ti gs
                       pci = tipc $ ths
                       gs' = map (fireOne gs) ts
                   in Map.insert pci gs' t
 
-- | Fire one transition of a thread
fireOne :: GlobalState -> Transition -> (GlobalState, SExpressions, ISExpr)
fireOne gs (_,_,f,_) = f gs

-- | Explode - For each enabled thread
explode :: Int -> T -> Map.Map String Transitions -> (SExpressions, ISExpr)
explode i mone ts = let schsym = SimpleSym $ "sel" ++ show i
                        schisexp = ISExpr $ IdentExpr $ SymIdent schsym 
                        sch = declfun schsym (SymSort "I32") 
                        mse = Map.mapWithKey (explodeOne ts i schisexp) mone
                        (se,ise) = unzip $ Map.elems mse
                    in (nub $ sch:(concat se), branchThread ise)

-- Generate the final ISExpr for the thread
explodeOne :: ModelT -> Int -> ISExpr -> Int -> [(GlobalState, SExpressions, ISExpr)] -> (SExpressions, ISExpr)
explodeOne ts si sch pci vals = let pcisym  = SimpleSym $ "bv" ++ show pci
                                    pcisexp = ISExpr $ IdentExpr $ IdxIdent pcisym [32]
                                    schisexp = sFn "=" sch pcisexp 
                                    (exprs1, expr1) = unzip $ map (\(_,a,b) -> (a,b)) vals
                                    (exprs2, expr2) = unzip $ map (\(gs,_,_) -> runApplyCn gs si ts) vals 
                                    sexprs = concat $ exprs1 ++ exprs2
                                    isexpr = branchFusion $ schisexp:(expr1 ++ expr2)
                                in (sexprs, isexpr)  

runApplyCn :: GlobalState -> Int -> ModelT -> (SExpressions, ISExpr) 
runApplyCn gs si ts = evalState (applyCn (si+1) ts) gs

branchThread :: [ISExpr] -> ISExpr
branchThread [] = error "branchThread"
branchThread [x] = x
branchThread (x:xs) = sOr x $ branchFusion xs
