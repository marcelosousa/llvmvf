{-# LANGUAGE TypeOperators, UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Language.FeatherIR.Base.Syntax
-- Copyright :  (c) 2013 Marcelo Sousa
-- Feather-weight Intermediate Representation Base
-------------------------------------------------------------------------------

module Language.FeatherIR.Base.Syntax where

import Language.FeatherIR.Base.Type
import Data.Map

type Id = String

data P = 
	P { namedTypes      ∷ Map Id Τ 
	  , globalVariables ∷ Map Id G 
	  , functions       ∷ Map Id F
	}

data G = G Id Τ C

data F = F Id Σ [BB]

data Σ = Σ [(Id,Τ)] Τ

-- Basic Block
data BB = BB Id [Φ] [I] T

-- φ-instruction
data Φ = Φ Id Τ [(V,Id)]

-- instruction
data I =
	  BinOp Id Τ BinI V V
	| BitOp Id Τ BitI V V
    | DerOp DerI
    | Cast  Id Τ V
    | Call  Id Τ Id [V]
    | Cmp   Id Τ V V
    | MemOp MemI

data BinI =
	Add | Sub | Mul | Div | Rem

data BitI = 
	Shl | LShr | AShr | And | Or | Xor

data DerI = 
	  Extract Id Τ V [V]
	| Insert  Id Τ V V [V]

data MemI = 
	  Alloca Id Τ V

-- terminator
data T = T

-- constant
data C = Constant

-- value
data V = 
	  VI Id Τ
	| VC C

