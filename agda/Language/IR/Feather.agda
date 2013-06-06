{-# OPTIONS --type-in-type #-}

module Language.IR.Feather where

open import Data.Unit
open import Data.Sum
open import Data.Product
open import Coinduction
open import Category.Monad.Partiality

-- Code for Regular
data Code : Set where
  U : Code
  K : Set → Code
  I : Code
  _⊕_ : Code → Code → Code
  _⊗_ : Code → Code → Code

-- Interpretation function for Regular Code
⟦_⟧_ : Code → Set → Set
⟦ U ⟧       r = ⊤
⟦ K a ⟧     r = a
⟦ I ⟧       r = r
⟦ c1 ⊕ c2 ⟧ r = ⟦ c1 ⟧ r ⊎ ⟦ c2 ⟧ r
⟦ c1 ⊗ c2 ⟧ r = ⟦ c1 ⟧ r × ⟦ c2 ⟧ r

-- Least fixed point of Codes
data μ_ (c : Code) : Set where
  <_> : ⟦ c ⟧ (μ c) → μ c
