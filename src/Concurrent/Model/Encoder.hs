-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Encoder(encode) where

import Concurrent.Model.Encoder.Module 
import Concurrent.Model

import Language.LLVMIR
import Language.SMTLib2

-- In general, the problem of verifying two-threaded programs
-- (with unbounded stacks) is undecidable.
-- Constraint the scheduler by guard stregthening

-- To guarantee correctness, the scheduler must allow context-switch
-- that are conflicting, i.e., accesses whose relative execution
-- order can produce different global system states. 
-- 1. Persistent/ample set computations.
-- 2. Lock-set and/or lock-acquisition history analysis
-- 3. Conditional dependency
encode :: SCModel t => Model t -> SMod
encode = undefined 

--encode :: Module -> SMod
--encode mdl = enc_Syn_Module $ wrap_Module (sem_Module mdl) $ Inh_Module {}
