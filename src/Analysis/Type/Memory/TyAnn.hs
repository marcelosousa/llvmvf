{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.TyAnn
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Memory.TyAnn where

import Language.LLVMIR (Identifier, Identifiers)

import Control.Applicative
import Prelude.Unicode ((≡))

type TysAnn = [TyAnn]

data TyAnn = TyBot
           | TyUndef
           | TyPri TyPri
           | TyDer TyDer
           | TyJumpTo Identifiers
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

data TyAgg = TyArr  Int TyAnn        -- Derived + Aggregate  
           | TyStr String Int TysAnn -- Derived + Aggregate
  deriving (Eq, Ord)

data TyAnnot = TyIOAddr
             | TyRegAddr
             | TyAny
  deriving (Eq, Ord)

i ∷ Int → TyAnn
i n = TyPri $ TyInt n

instance Show TyAnn where
  show TyBot = "⊥"
  show TyUndef = "undef"
  show (TyPri t) = show t
  show (TyDer t) = show t
  show (TyJumpTo ids) = "{#" ++ show ids ++ "}"

instance Show TyPri where
  show TyVoid     = "void"
  show TyFloat    = "float"
  show TyLabel    = "label"
  show TyMetadata = "metadata"
  show (TyInt n)  = "i"++show n

instance Show TyDer where
  show (TyAgg t) = show t
  show (TyVec n t) = "<" ++ show n ++ " x " ++ show t ++ ">"
  show (TyFun [] t _) = "fn :: " ++ show t
  show (TyFun tys t _) = "fn :: " ++ (foldr (\x s -> show x ++ " -> " ++ s) (show t) tys)
  show (TyPtr ty tya) = "*(" ++ show ty ++ ") " ++ show tya
--  show (TyOpa s) = "{" ++ s ++ "}"

instance Show TyAgg where
  show (TyArr n t) = "[" ++ show n ++ " x " ++ show t ++ "]"
  show (TyStr nm n []) = "{" ++ nm ++ "}"
  show (TyStr nm n t) = "{" ++ (foldr (\x s -> show x ++ ", " ++ show s) (show $ last t) (init t)) ++ "}"

instance Show TyAnnot where
  show TyIOAddr = "IOAddr"
  show TyRegAddr = "RegAddr"
  show TyAny = "AnyAddr"

class AEq α where
  (≅) ∷ α → α → Maybe α

instance AEq TyAnn where
  (TyDer τ1) ≅ (TyDer τ2) = TyDer <$> τ1 ≅ τ2
  τ1 ≅ τ2 = if τ1 ≡ τ2
            then Just τ1
            else Nothing

instance AEq TyDer where
  (TyAgg τ1) ≅ (TyAgg τ2) = TyAgg <$> τ1 ≅ τ2
  (TyVec n τ1) ≅ (TyVec m τ2) =
    if n ≡ m
    then TyVec n <$> τ1 ≅ τ2
    else Nothing
  (TyFun a1 r1 b1) ≅ (TyFun a2 r2 b2) =
    if b1 ≡ b2
    then (\a r → TyFun a r b1) <$> cmpList a1 a2 <*> r1 ≅ r2
    else Nothing  
  (TyPtr τ1 a1) ≅ (TyPtr τ2 a2) = TyPtr <$> τ1 ≅ τ2 <*> a1 ≅ a2
  τ1 ≅ τ2 = Nothing

instance AEq TyAgg where
  (TyArr n τ1) ≅ (TyArr m τ2) = 
    if n ≡ m
    then TyArr n <$> τ1 ≅ τ2
    else Nothing
  (TyStr n i τ1) ≅ (TyStr m j τ2) =
    if n ≡ m && i ≡ j
    then TyStr n i <$> cmpList τ1 τ2
    else Nothing
  τ1 ≅ τ2 = Nothing

instance AEq TyAnnot where
  TyIOAddr ≅ TyRegAddr = Nothing
  TyIOAddr ≅ _         = Just TyIOAddr
  TyRegAddr ≅ TyIOAddr = Nothing
  TyRegAddr ≅ _        = Just TyRegAddr
  TyAny ≅ TyAny = Just TyAny
  TyAny ≅ α     = Just α

cmpList ∷ TysAnn → TysAnn → Maybe TysAnn
cmpList a1 a2 = sequence $ map (uncurry (≅)) $ zip a1 a2