-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.TyAnn
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Memory.TyAnn where

#if __GLASGOW_HASKELL__ >= 704
import Data.Map hiding (foldr)
#else
import Data.Map 
#endif
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
  deriving (Eq, Ord)

data TyDer = TyAgg TyAgg
           | TyVec Int TyAnn
           | TyFun TysAnn TysAnn
           | TyLab TysAnn TyAnn
           | TyPtr TyAnn  TyAnnot
           | TyOpa String
  deriving (Eq, Ord)

data TyAgg = TyArr  Int TyAnn         -- Derived + Aggregate  
           | TyStr String Int TysAnn -- Derived + Aggregate
  deriving (Eq, Ord)

data TyAnnot = TyIOAddr
             | TyRegAddr
             | TyAny
  deriving (Eq, Ord)


type TyAnnEnv = Map Identifier TyAnn

instance Show TyAnn where
  show TyBot = "_|_"
  show TyUndef = "undef"
  show (TyPri t) = show t
  show (TyDer t) = show t

instance Show TyPri where
  show TyVoid = "void"
  show (TyInt n) = "i"++show n
  show TyFloat = "float"

instance Show TyDer where
  show (TyAgg t) = show t
  show (TyVec n t) = "<" ++ show n ++ " x " ++ show t ++ ">"
  show (TyFun tys t) = "fn :: " ++ (foldr (\x s -> show x ++ " -> " ++ show s) "" tys) ++ show " -> " ++ show t
  show (TyLab tys t) = "l :: " ++ (foldr (\x s -> show x ++ " -> " ++ show s) "" tys) ++ show " -> " ++ show t
  show (TyPtr ty tya) = "* (" ++ show ty ++ ")" ++ show tya
  show (TyOpa s) = "{" ++ s ++ "}"

instance Show TyAgg where
  show (TyArr n t) = "[" ++ show n ++ " x " ++ show t ++ "]"
  show (TyStr nm n t) = "{" ++ (foldr (\x s -> show x ++ ", " ++ show s) (show $ last t) (init t)) ++ "}"

instance Show TyAnnot where
  show TyIOAddr = "IOAddr"
  show TyRegAddr = "RegAddr"
  show TyAny = "AnyAddr"