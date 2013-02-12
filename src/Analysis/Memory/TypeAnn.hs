-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.TypeAnn
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Memory.TypeAnn where

import Language.LLVMIR

type TyMem = (Type, TyAnn)

data TyAnn = TyReg
           | TyIOAddr
           | TyRegAddr
           | TyAny
  deriving (Show, Eq, Ord)

