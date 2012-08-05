{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Encoder (encode) where

import Concurrent.Model.Analysis.ControlFlow (ControlFlow(..))
import Concurrent.Model.Encoder.Model 
import Concurrent.Model hiding (State)

import Language.LLVMIR

import Language.SMTLib2.Base
import Language.SMTLib2.Builder

import qualified Data.IntMap as IM
import qualified Data.Map as Map

import Data.Maybe

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
--class (SCModel t) => Encode t where

type GlobalState = (Map.Map String (PC, Map.Map Id Value), Map.Map Id Value, PC)

encode :: (SCModel t) => Model t -> SMod
encode m@Model{..} = let ccfg@ControlFlow{..} = controlflow m
                         cme = fromMaybe (error "encode") $ Map.lookup "main" cte
                         tvs = Map.map (\pci -> (pci, Map.empty)) $ Map.delete "main" cte
                         s0  = (tvs, Map.empty, cme) :: GlobalState
                         (smod, sf) = runState (encModel m ccfg) s0
                     in trace (show s0 ++ show sf) $ smod

encModel :: (SCModel t) => Model t -> ControlFlow -> State GlobalState SMod
encModel m ccfg = do eg <- encGlobals m
                     em <- encMain    m ccfg
                     et <- encProcs   m ccfg
                     return $ preamble ++ eg ++ em ++ et ++ final

preamble :: [SExpression]
preamble = [ setlogic QF_AUFBV
           , setoption "produce-models"
           ]

final :: [SExpression]
final = [ checksat , exit ]

encGlobals :: (SCModel t) => Model t -> State GlobalState [SExpression]
encGlobals m@Model{..} = let gsexpr = genc_Syn_Globals $ wrap_Globals (sem_Globals gvars) $ Inh_Globals { }
                         in  return gsexpr  

encMain :: (SCModel t) => Model t -> ControlFlow -> State GlobalState [SExpression]
encMain m@Model{..} ccfg@ControlFlow{..} =
   do let syn_fun =  wrap_Function (sem_Function $ unProc mainf) $ Inh_Function { tys_Inh_Function = nmdtys, vars_Inh_Function = gvars }
      return $ trace (show $ locals_Syn_Function syn_fun) $ menc_Syn_Function syn_fun

encProcs :: (SCModel t) => Model t -> ControlFlow -> State GlobalState [SExpression]
encProcs m@Model{..} ccfg@ControlFlow{..} = return []

