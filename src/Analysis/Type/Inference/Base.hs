{-# LANGUAGE UnicodeSyntax, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Base
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type inference 
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Base where

import qualified Data.Set as S

import Language.LLVMIR (Identifier(..), Identifiers, Type)
import Analysis.Type.Memory.TyAnn
import Analysis.Type.Memory.Util
import Analysis.Type.Util
import Prelude.Unicode ((⧺))
import UU.PPrint 

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

(↑^) ∷ Τ → Ταρ → Τα
(↑^) = liftTyGen

(↣) ∷ (Monad m) ⇒ a → m a
(↣) = return

-- | Auxiliar Function
τList ∷ (TyConstr α) ⇒ S.Set Τℂ → [α] → ℂState
τList = foldM τℂu

τℂu ∷ (TyConstr α) ⇒ S.Set Τℂ → α → ℂState
τℂu τℂs α = do ατℂ ← τℂ α
               (↣) $ τℂs ∪ ατℂ

type Τ = Type
type Τα = TyAnn
type Ταρ = TyAnnot
type Id = Identifier

data ΤClass = 
  ΤInt | ΤFlt | ΤPtr | Τ1NA | Τ1 | ΤAgg
  deriving (Eq, Ord,Show)

classOf ∷ Τα → ΤClass → Bool
(TyPri (TyInt _)) `classOf` ΤInt = True
(TyPri _)         `classOf` Τ1 = True
_ `classOf` _ = undefined

-- Constraint Element
data ℂ = ℂτ Τα -- Type α
       | ℂπ Id -- Type of Id
       | ℂc ΤClass -- Class of
       | ℂι ℂ Int  -- for GEP instruction
       | ℂp ℂ Ταρ  -- Pointer to ℂ Τα
       | ℂλ [ℂ] ℂ  -- Function
  deriving (Eq, Ord)

instance Show ℂ where
  show (ℂτ τα)    = "ℂτ(" ⧺ show τα ⧺ ")"
  show (ℂπ α)     = "ℂπ(" ⧺ (show $ pretty α) ⧺ ")"
  show (ℂι c i)   = "ℂι(" ⧺ show c ⧺ "," ⧺ show i ⧺ ")"
  show (ℂc τc)    = "ℂc(" ⧺ show τc ⧺ ")"
  show (ℂp c ταρ) = "ℂp(" ⧺ show c ⧺ "," ⧺ show ταρ ⧺ ")"
  show (ℂλ cl c)  = "ℂλ(" ⧺ show cl ⧺ "→" ⧺ show c ⧺ ")"

-- Normalize the constraint
(⤜) ∷ ℂ → Ταρ → ℂ
(ℂc _) ⤜ ταρ = error "⤜: can't lift class constraint"
(ℂτ τ) ⤜ ταρ = ℂτ $ TyDer $ TyPtr τ ταρ
c      ⤜ ταρ = ℂp c ταρ

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
data Ε = Ε 
	{ 
	  fn ∷ (Id,[ℂ]) -- Current Function
	, bb ∷ Id -- Current Basic Block
  	}

εΕ = Ε ((Global ""),[]) (Global "")

type ΕState α = State Ε α
type ℂState = ΕState (S.Set Τℂ)

-- update the function in the
-- environment
νfn ∷ (Id,[ℂ]) → ΕState ()
νfn n = do γ@Ε{..} ← get
           put γ{fn = n}

-- update the bb in the
-- environment
νbb ∷ Id → ΕState ()
νbb n = do γ@Ε{..} ← get
           put γ{bb = n}

δfn ∷ ΕState (Id,[ℂ])
δfn = do γ@Ε{..} ← get
         (↣) fn

δbb ∷ ΕState Id
δbb = do γ@Ε{..} ← get
         (↣) bb

-- Type Constraint Class
class TyConstr a where
    τℂ ∷ a → ℂState

-- Type Inference Class
class Constr a where
	π ∷ a → ℂ