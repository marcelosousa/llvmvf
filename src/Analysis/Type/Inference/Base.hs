{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Base
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type inference 
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Base where

import qualified Data.Set as S

import Language.LLVMIR (Identifier, Identifiers, Type)
import Analysis.Type.Memory.TyAnn
import Analysis.Type.Memory.Util
import Analysis.Type.Util

import Control.Monad
import Control.Applicative
import Control.Monad.State

-- A bit of unicode non-sense
(∪) ∷ Ord α ⇒ S.Set α → S.Set α → S.Set α
(∪) = S.union

ε ∷ Ord α ⇒ S.Set α
ε = S.empty

(∙) ∷ Ord α ⇒ α → S.Set α
(∙) = S.singleton

(∘) :: Ord α ⇒ α → S.Set α → S.Set α
(∘) = S.insert

(↑) ∷ Τ → Τα
(↑) = liftTy

(↣) ∷ (Monad m) ⇒ a → m a
(↣) = return

-- | Auxiliar Function
τList ∷ (TyConstr a) ⇒ S.Set Τℂ → [a] → State Γ (S.Set Τℂ)
τList = foldM τℂu

τℂu ∷ (TyConstr α) ⇒ S.Set Τℂ → α → State Γ (S.Set Τℂ)
τℂu τℂs α = do ατℂ ← τℂ α
               (↣) $ τℂs ∪ ατℂ

type Τ = Type
type Τα = TyAnn
type Id = Identifier

data ΤClass = 
  ΤInt | ΤFlt | ΤPtr | Τ1NA

  deriving (Eq, Ord,Show)

-- Constraint Element
data ℂ = ℂτ Τα -- Type α
       | ℂπ Id -- Type of Id
       | ℂc ΤClass -- Class of  
  deriving (Eq, Ord,Show)

-- Type Constraint
data Τℂ = ℂ :=: ℂ -- same type
        | ℂ :<: ℂ -- subtyping i1 :<: i2
        | ℂ :≤: ℂ -- less than 
        | ℂ :≌: ℂ -- bit size equality
  deriving (Eq, Ord,Show)

(>:) ∷ ℂ → ℂ → Τℂ
c1 >: c2 = c2 :<: c1

(≥:) ∷ ℂ → ℂ → Τℂ
c1 ≥: c2 = c2 :≤: c1

-- Environment
data Γ = Γ 
	{ 
	  fn ∷ Id -- Current Function
	, bb ∷ Id -- Current Basic Block
  	}

-- update the function in the
-- environment
νfn ∷ Id → State Γ ()
νfn n = do γ@Γ{..} ← get
           put γ{fn = n}

-- update the bb in the
-- environment
νbb ∷ Id → State Γ ()
νbb n = do γ@Γ{..} ← get
           put γ{bb = n}

δfn ∷ State Γ Id
δfn = do γ@Γ{..} ← get
         (↣) fn

δbb ∷ State Γ Id
δbb = do γ@Γ{..} ← get
         (↣) bb

-- Type Constraint Class
class TyConstr a where
    τℂ ∷ a → State Γ (S.Set Τℂ)

-- Type Inference Class
class Constr a where
	π ∷ a → ℂ