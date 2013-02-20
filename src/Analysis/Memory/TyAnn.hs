-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.TyAnn
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Memory.TyAnn where

import Data.Map
import Language.LLVMIR (Identifier)

type TysAnn = [TyAnn]

data TyAnn = TyBot
           | TyUndef
           | TyPri TyPri
           | TyDer TyDer
  deriving (Show, Eq, Ord)

data TyPri = TyVoid
           | TyInt Int
           | TyFloat -- Just a tag
  deriving (Show, Eq, Ord)

data TyDer = TyAgg TyAgg
           | TyVec Int TyAnn
           | TyFun TysAnn TysAnn
           | TyLab TysAnn TyAnn
           | TyPtr TyAnn  TyAnnot
           | TyOpa String
  deriving (Show, Eq, Ord)

data TyAgg = TyArr  Int TyAnn         -- Derived + Aggregate  
           | TyStr String Int TysAnn -- Derived + Aggregate
  deriving (Show, Eq, Ord)

data TyAnnot = TyIOAddr
             | TyRegAddr
             | TyAny
  deriving (Show, Eq, Ord)


type TyAnnEnv = Map Identifier TyAnn