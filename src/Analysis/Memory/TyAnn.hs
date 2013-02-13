-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.TyAnn
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Memory.TyAnn where

type TysAnn = [TyAnn]

-- First Class Type are the ones produced by instructions
data TyAnn = TyVoid
           | TyUndefined
           | TyInt Int
           | TyFloatPoint -- Just a tag
           | TyVector Int TyAnn
           | TyArray  Int TyAnn         -- Derived + Aggregate  
           | TyStruct String Int TysAnn -- Derived + Aggregate
           | TyFun TysAnn TyAnn
           | TyPtr TyAnn  TyAnnotation
  deriving (Show, Eq, Ord)

data TyAnnotation = TyIOAddr
                  | TyRegAddr
                  | TyAny
  deriving (Show, Eq, Ord)

