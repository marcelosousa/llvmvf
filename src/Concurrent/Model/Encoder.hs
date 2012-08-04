-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Encoder (encode) where

import Concurrent.Model.Encoder.Model 
import Concurrent.Model

import Language.LLVMIR
import Language.SMTLib2.Base

import qualified Data.IntMap as IM

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
encode :: Model t -> SMod
encode (Model tys vars m ts decls) =  preamble vars
                                   ++ encodeInit tys vars m  
--                                 ++ encodeThreads tys vars ts 
                                   ++ final

preamble :: Globals -> [SExpression]
preamble gs = let gsexpr = genc_Syn_Globals $ wrap_Globals (sem_Globals gs) $ Inh_Globals { }
              in (setlogic QF_AUFBV):gsexpr  

final :: [SExpression]
final = [ checksat , exit ]

encodeInit :: NamedTypes -> Globals -> Process -> [SExpression]
encodeInit tys vars (Process i f) = let syn_fun =  wrap_Function (sem_Function f) $ Inh_Function { tys_Inh_Function = tys, vars_Inh_Function = vars  }
                                    in  trace (show $ locals_Syn_Function syn_fun) $ menc_Syn_Function syn_fun
encodeThreads = undefined

--encode :: SCModel t => Model t -> SMod
--encode (Model tys vars m ts) = preamble ++ encode m 

--encode :: Module -> SMod
--encode mdl = enc_Syn_Module $ wrap_Module (sem_Module mdl) $ Inh_Module {}
