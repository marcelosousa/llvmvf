{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.TypeQual
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.TypeQual where

import Language.LLVMIR (Identifier, Identifiers, Type)

import Control.Applicative
import Prelude.Unicode ((≡),(⧺))

import qualified Data.Map as M

import Debug.Trace 

type NamedTypes α = M.Map String (TypeQual α)
type TypesQual α = [TypeQual α]

data TypeQual α = 
    TyBot
  | TyPri TyPri
  | TyDer (TyDer α)
  deriving (Eq, Ord)

data TyPri = TyVoid
           | TyInt Int
           | TyFloat -- Just a tag
           | TyMetadata
           | TyLabel
  deriving (Eq, Ord)

data TyDer α = 
    TyAgg (TyAgg α)
  | TyVec Int (TypeQual α)
  | TyFun (TypesQual α) (TypeQual α) Bool
  | TyPtr (TypeQual α)  α
  deriving (Eq, Ord)

data TyAgg α = 
    TyArr Int        (TypeQual  α) -- Derived + Aggregate  
  | TyStr String Int (TypesQual α) -- Derived + Aggregate
  deriving (Eq, Ord)

i ∷ Int → TypeQual α
i n = TyPri $ TyInt n

-- Poset class
class PartialOrder α where
  (⋚) ∷ NamedTypes α → α → α → Maybe α
{-
instance (Eq α, AEq α, IEq α) ⇒ AEq (TypeQual α) where
  (≅) nτ (TyDer τ1) (TyDer τ2) = TyDer <$> (≅) nτ τ1 τ2
  (≅) nτ τ1 τ2 = if τ1 ≡ τ2 
                 then Just τ1
                 else Nothing

instance (Eq α, AEq α, IEq α) ⇒ AEq (TyDer α) where
  (≅) nτ (TyAgg τ1)   (TyAgg τ2) = TyAgg <$> (≅) nτ τ1 τ2
  (≅) nτ (TyVec n τ1) (TyVec m τ2) =
    if n ≡ m
    then TyVec n <$> (≅) nτ τ1 τ2
    else Nothing
  (≅) nτ (TyFun a1 r1 b1) (TyFun a2 r2 b2) =
    if b1 ≡ b2
    then (\a r → TyFun a r b1) <$> cmpList nτ a1 a2 <*> (≅) nτ r1 r2
    else Nothing  
  (≅) nτ (TyPtr τ1 a1) (TyPtr τ2 a2) = TyPtr <$> (≅) nτ τ1 τ2 <*> (≌) a1 a2
  (≅) nτ τ1 τ2 = Nothing

instance (Eq α, AEq α, IEq α) ⇒ AEq (TyAgg α) where
  (≅) nτ (TyArr n τ1) (TyArr m τ2) = 
    if n ≡ m
    then TyArr n <$> (≅) nτ τ1 τ2
    else Nothing
  (≅) nτ (TyStr n i τ1) (TyStr m j τ2) = -- Source of all troubles
    eqTyStr nτ (n,i,τ1) (m,j,τ2)
  (≅) nτ τ1 τ2 = Nothing

cmpList ∷ (Eq α, AEq α, IEq α) ⇒ NamedTypes α → TypesQual α → TypesQual α → Maybe (TypesQual α)
cmpList nτ a1 a2 = sequence $ map (uncurry ((≅) nτ)) $ zip a1 a2

eqTyStr ∷ (Eq α, AEq α, IEq α) ⇒ NamedTypes α -> (String,Int,TypesQual α) -> (String,Int, TypesQual α) -> Maybe (TyAgg α) 
eqTyStr nτ α β =
  case α of 
  ("",n,ατ) → case β of
    ("",m,βτ) → if n ≡ m && length ατ ≡ length βτ
                then TyStr "" n <$> cmpList nτ ατ βτ
                else Nothing
    (nβ,m,βτ) → case M.lookup nβ nτ of
      Nothing → if n ≡ m && length ατ ≡ length βτ
                then TyStr "" n <$> cmpList nτ ατ βτ
                else Nothing
      Just η@(TyDer (TyAgg (TyStr nη o ητ))) →
        if nβ ≡ nη
        then if n ≡ o
             then TyStr "" n <$> cmpList nτ ατ ητ
             else Nothing
        else eqTyStr nτ α (nη,o,ητ)
      Just _ → Nothing      
  (nα,n,ατ) → case β of
    ("",m,βτ) → eqTyStr nτ β α
    (nβ,m,βτ) → 
      if nα ≡ nβ
      then Just $ TyStr nα n ατ 
      else Nothing  
-}
