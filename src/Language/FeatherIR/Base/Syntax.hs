{-# LANGUAGE TypeOperators #-}
-------------------------------------------------------------------------------
-- Module    :  Language.FeatherIR.Base.Syntax
-- Copyright :  (c) 2013 Marcelo Sousa
-- Feather-weight Intermediate Representation Base
-------------------------------------------------------------------------------

module Language.FeatherIR.Base.Syntax where

import Language.FeatherIR.Base.Type
import Data.Map

type Id = String
type (:⇀) = Map

data Σ = Σ [(Id,Τ)] Τ

data P = P (Id:⇀Τ) (Id:⇀G) (Id:⇀F)

data G = G Id Τ C

data F = F Id Σ [BB]

-- Basic Block
data BB = BB Id [Φ] [I] T

-- φ-instruction
data Φ = Φ Id Τ [(V,Id)]

-- instruction
data I = I

-- terminator
data T = T

-- constant
data C = Constant

-- value
data V = VI Id | VC C

