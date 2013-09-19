{-# LANGUAGE TypeOperators, UnicodeSyntax, KindSignatures, GADTs #-}
-------------------------------------------------------------------------------
-- Module    :  Language.FeatherIR.Base.Type
-- Copyright :  (c) 2013 Marcelo Sousa
-- Feather-weight Intermediate Representation 
-- Type System
-------------------------------------------------------------------------------

module Language.FeatherIR.Base.Type where


type Type = TypeQual ()

type TypesQual α = [TypeQual α]

data TypeQual α = 
    TyPri TyPri
  | TyDer (TyDer α)
  deriving (Eq, Ord)

data TyPri = 
	TyVoid
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