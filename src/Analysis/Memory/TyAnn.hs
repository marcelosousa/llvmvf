-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.TyAnn
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Memory.TyAnn where

type TysAnn = [TyAnn]

-- First Class Type are the ones produced by instructions
data TyAnn = TyBot
           | TyPri TyPri
           | TyDer TyDer
  deriving (Show, Eq, Ord)

data TyPri = TyVoid
           | TyInt Int
           | TyFloatPoint -- Just a tag
  deriving (Show, Eq, Ord)

data TyDer = TyAgg TyAgg
           | TyVector Int TyAnn
           | TyFun TysAnn TyAnn
           | TyLab TysAnn TyAnn
           | TyPtr TyAnn  TyAnnot
           | TyOpa String
  deriving (Show, Eq, Ord)

data TyAgg = TyArray  Int TyAnn         -- Derived + Aggregate  
           | TyStruct String Int TysAnn -- Derived + Aggregate
  deriving (Show, Eq, Ord)

data TyAnnot = TyIOAddr
             | TyRegAddr
             | TyAny
  deriving (Show, Eq, Ord)

