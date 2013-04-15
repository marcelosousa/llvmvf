-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.TyAnn
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Memory.TyAnn where

import Language.LLVMIR (Identifier)

type TysAnn = [TyAnn]

data TyAnn = TyBot
           | TyUndef
           | TyPri TyPri
           | TyDer TyDer
  deriving (Eq, Ord)

data TyPri = TyVoid
           | TyInt Int
           | TyFloat -- Just a tag
           | TyMetadata
           | TyLabel
  deriving (Eq, Ord)

data TyDer = TyAgg TyAgg
           | TyVec Int TyAnn
           | TyFun TysAnn TyAnn Bool
           | TyPtr TyAnn  TyAnnot
--           | TyLab TysAnn TyAnn
--           | TyOpa String
  deriving (Eq, Ord)

data TyAgg = TyArr  Int TyAnn         -- Derived + Aggregate  
           | TyStr String Int TysAnn -- Derived + Aggregate
  deriving (Eq, Ord)

data TyAnnot = TyIOAddr
             | TyRegAddr
             | TyAny
  deriving (Eq, Ord)

instance Show TyAnn where
  show TyBot = "_|_"
  show TyUndef = "undef"
  show (TyPri t) = show t
  show (TyDer t) = show t

instance Show TyPri where
  show TyVoid     = "void"
  show TyFloat    = "float"
  show TyLabel    = "label"
  show TyMetadata = "metadata"
  show (TyInt n)  = "i"++show n

instance Show TyDer where
  show (TyAgg t) = show t
  show (TyVec n t) = "<" ++ show n ++ " x " ++ show t ++ ">"
  show (TyFun tys t _) = "fn :: " ++ (foldr (\x s -> show x ++ " -> " ++ show s) "" tys) ++ show " -> " ++ show t
  show (TyPtr ty tya) = "* (" ++ show ty ++ ")" ++ show tya
--  show (TyOpa s) = "{" ++ s ++ "}"

instance Show TyAgg where
  show (TyArr n t) = "[" ++ show n ++ " x " ++ show t ++ "]"
  show (TyStr nm n t) = "{" ++ (foldr (\x s -> show x ++ ", " ++ show s) (show $ last t) (init t)) ++ "}"

instance Show TyAnnot where
  show TyIOAddr = "IOAddr"
  show TyRegAddr = "RegAddr"
  show TyAny = "AnyAddr"