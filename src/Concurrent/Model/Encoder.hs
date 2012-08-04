{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Encoder (encode) where

import Concurrent.Model.Encoder.Model 
import Concurrent.Model

import Language.LLVMIR

import Language.SMTLib2.Base
import Language.SMTLib2.Builder

import qualified Data.IntMap as IM
import qualified Data.Map as Map

import Debug.Trace (trace)
 
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

type GlobalState = (IM.IntMap (PC, Map.Map Id Value), Map.Map Id Value, PC)

encode :: (SCModel t) => Model t -> SMod
encode m@Model{..} = preamble 
{- 
   let s0 = (IM.empty, Map.empty, iCm m)
       ccfg@ControlFlow{..} =
   in  preamble
    ++ encodeGlobals vars
     encodeMain tys vars m  
                                   ++ encodeThreads tys vars ts 
                                   ++ final
-}
preamble :: [SExpression]
preamble = [ setlogic QF_AUFBV
           , setoption "produce-models"
           ]

encGlobals :: Globals -> [SExpression]
encGlobals gs = let gsexpr = genc_Syn_Globals $ wrap_Globals (sem_Globals gs) $ Inh_Globals { }
                in (setlogic QF_AUFBV):gsexpr  

final :: [SExpression]
final = [ checksat , exit ]

encodeInit :: NamedTypes -> Globals -> Process -> [SExpression]
encodeInit tys vars (Process i f) = let syn_fun =  wrap_Function (sem_Function f) $ Inh_Function { tys_Inh_Function = tys, vars_Inh_Function = vars  }
                                    in  trace (show $ locals_Syn_Function syn_fun) $ menc_Syn_Function syn_fun

-- Having a IntMap PC to check 
encodeThreads :: NamedTypes -> Globals -> Processes -> [SExpression]
encodeThreads tys vars m = []

