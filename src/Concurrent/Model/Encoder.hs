{-#LANGUAGE RecordWildCards, DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Encoder (encode) where

import Concurrent.Model                    hiding (State)

import Concurrent.Model.Encoder.Model (encType, encGlobalVars, preEncoder, wrap, sAnd, sOr, sFn, encodeMain)
import Concurrent.Model.Encoder.Threads (encodeThreads)
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
encode :: (SCModel t) => Model t -> Int -> SMod
encode m@Model{..} k = let ccfg@ControlFlow{..} = controlflow m
                           cme = fromMaybe (error "encode")                  $ Map.lookup "main" cte  -- ^ Set the current PC to the initial pc of main
                           tvs = Map.map (\pci -> ThreadState pci Map.empty) $ Map.delete "main" cte  -- ^ Set the initial PC for each thread
                           s0  = GlobalState Map.empty cme Map.empty tvs                              -- ^ Initial state
                           (smod, sf) = runState (encModel m k) s0                             
                       in smod --trace (show s0 ++ show sf) $ smod

-- | Main encode function.
encModel :: (SCModel t) => Model t -> Int -> State GlobalState SMod
encModel m k = do tyenc <- encNmdTys  m   -- ^ Encode Named Types
                  gvenc <- encGlobals m   -- ^ Encode Global Variables
                  menc  <- encFunctions m k  -- ^ Encode Functions
                  return $ nub $ preamble ++ tyenc ++ gvenc ++ menc ++ final

-- | Initial part of an smt module
preamble :: SExpressions
preamble = [ setlogic QF_AUFBV           -- ^ Closed quantifier-free formulas over the theory of bitvectors and bitvector arrays extended with free sort and function symbols.
           , setoption "produce-models"  -- ^ To be able to get values if satisfiable
           , declsort  "Pair" 2          -- ^ Declare the sort Pair
           ]

-- | Final part of an smt module
final :: SExpressions
final = [ checksat 
        , exit 
        ]

-- | Useful sorts
usefulsorts :: [((Type,(SSortExpr, SSort)), SExpression)]
usefulsorts = [ ((TyInt 8,  (SymSort "I8", "I8"))  , defsorti  8)                 -- ^ Define I8 as _ BitVector 8
              , ((TyInt 32, (SymSort "I32", "I32")), defsorti  32)                -- ^ Define I32 as _ BitVector 32
              , ((TyInt 64, (SymSort "I64", "I64")), defsorti  64)                -- ^ Define I64 as _ BitVector 64
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

-- | Encode Main
encFunctions :: (SCModel t) => Model t -> Bound -> State GlobalState SExpressions
encFunctions m@Model{..} k = do gs@GlobalState{..} <- get
                                let ccfg@ControlFlow {..} = controlflow m 
                                    fs = getFs mainf procs 
                                    (s,p)  = preEncoder fs defsorts decls
                                    se = preEncode p
                                    (l, pcs, sexprs) = encodeMain    (unProc mainf) p decls
                                    (cpcs, csexprs)  = encodeThreads (toFunctions procs)  k p l (Map.delete "main" cte) $ Map.delete "main" cfg 
                                trace ("----\n" ++ show p ++ "--- ---\n" ++ show l) $ return $ s ++ se ++ pcs ++ [ assert $ wrap sAnd sexprs ] ++ cpcs ++ [ assert csexprs ] 

preEncode :: PreEncoder -> SExpressions
preEncode p@PreEncoder{..} = Map.foldrWithKey (\i (t, pcs) se -> (concatMap (\(_,c) -> (declSVar (i ++ show c) t sortEnv):[]) (zip pcs [0..])) ++ se ) [] fStore 

-- | declSVar declare a new fresh variable 
declSVar :: Id -> Type -> TypeEnv -> SExpression
declSVar s ty mt = let ss = SimpleSym s
                       sexpr = case Map.lookup ty mt of
                                    Nothing     -> error $ "encodeVar:\n" ++ show ty ++ "\n" ++ show mt
                                    Just (se,_) -> se
                   in declfun ss sexpr


 
