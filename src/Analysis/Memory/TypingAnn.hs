-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.TypingAnn
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Judgement and Type Environment 
-------------------------------------------------------------------------------

module Analysis.Memory.TypingAnn where

import Analysis.Memory.TyAnn
import Data.Map
import Language.LLVMIR (Identifier)

type TyAnnEnv = Map Identifier TyAnn

--type TyAnnRel = (TyAnnEnv, Module, TyAnn)
  
