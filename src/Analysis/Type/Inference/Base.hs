{-# LANGUAGE UnicodeSyntax, RecordWildCards, TupleSections #-}
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
τList ∷ (TyConstr α) ⇒ S.Set Τℂ' → [α] → ℂState
τList = foldM τℂu

τℂu ∷ (TyConstr α) ⇒ S.Set Τℂ' → α → ℂState
τℂu τℂs α = do ατℂ ← τℂ α
               (↣) $ τℂs ∪ ατℂ

τListR ∷ (TyConstrR α) ⇒ S.Set Τℂ → [α] → ΕState (S.Set Τℂ)
τListR = foldM τℂuR

τℂuR ∷ (TyConstrR α) ⇒ S.Set Τℂ → α → ΕState (S.Set Τℂ)
τℂuR τℂs α = do ατℂ ← τℂr α
                (↣) $ τℂs ∪ ατℂ

type Τ = Type
type Τα = TyAnn
type Ταρ = TyAnnot
type Id = Identifier

data TClass = 
  TInt | TFlt | TPtr | T1NA | T1 | TAgg
  deriving (Eq, Ord,Show)

classOf ∷ Τα → TClass → Bool
(TyPri (TyInt _))     `classOf` TInt = True
(TyPri TyFloat)       `classOf` TFlt = True
(TyDer (TyPtr _ _))   `classOf` TPtr = True
(TyPri TyVoid)        `classOf` T1   = False
(TyPri _)             `classOf` T1   = True
(TyDer (TyFun _ _ _)) `classOf` T1   = False
(TyDer _)             `classOf` T1   = True
(TyDer (TyAgg _))     `classOf` TAgg = True
τα                    `classOf` TAgg = False
τα                    `classOf` T1NA = (τα `classOf` T1) && not (τα `classOf` TAgg)
α `classOf` β = error $ "classOf error: " ⧺ show α ⧺ " " ⧺ show β

-- Constraint Element
data ℂ = ℂτ Τα -- Type α
       | ℂπ Id -- Type of Id
       | ℂc TClass -- Class of
       | ℂι ℂ [Int] -- for GEP instruction
       | ℂp ℂ Ταρ  -- Pointer to ℂ Τα
       | ℂλ [ℂ] ℂ  -- Function
  deriving (Eq, Ord, Show)

instance ShowType ℂ where
  showType γ (ℂτ τα)    = "Ctau(" ⧺ showType γ τα ⧺ ")"
  showType γ (ℂπ α)     = "Cv(" ⧺ (show $ pretty α) ⧺ ")"
  showType γ (ℂι c i)   = "Cgep(" ⧺ showType γ c ⧺ "," ⧺ show i ⧺ ")"
  showType γ (ℂc τc)    = "Ccl(" ⧺ show τc ⧺ ")"
  showType γ (ℂp c ταρ) = "Cptr(" ⧺ showType γ c ⧺ "," ⧺ show ταρ ⧺ ")"
  showType γ (ℂλ cl c)  = "Cfn(" ⧺ foldr (\a r → showType γ a ⧺ "," ⧺ r) "" cl ⧺ "->" ⧺ showType γ c ⧺ ")"

-- Normalize the constraint
(⤜) ∷ ℂ → Ταρ → ℂ
(ℂc _) ⤜ ταρ = error "⤜: can't lift class constraint"
(ℂτ τ) ⤜ ταρ = ℂτ $ TyDer $ TyPtr τ ταρ
c      ⤜ ταρ = ℂp c ταρ

-- Type Constraint
-- Simplify this later
type Τℂ' = (Τℂ,Int)

liftΤℂ ∷ Int → S.Set Τℂ → S.Set Τℂ'
liftΤℂ pc = S.map (,pc)

data Τℂ = ℂ :=: ℂ -- same type
        | ℂ :<: ℂ -- subtyping i1 :<: i2
        | ℂ :≤: ℂ -- less than 
        | ℂ :≌: ℂ -- bit size equality
  deriving (Eq, Ord, Show)

instance ShowType Τℂ where
  showType γ (α :=: β) = showType γ α ⧺ " :=: " ⧺ showType γ β
  showType γ (α :<: β) = showType γ α ⧺ " :<: " ⧺ showType γ β
  showType γ (α :≤: β) = showType γ α ⧺ " :≤: " ⧺ showType γ β
  showType γ (α :≌: β) = showType γ α ⧺ " :≌: " ⧺ showType γ β

(>:) ∷ ℂ → ℂ → Τℂ
c1 >: c2 = c2 :<: c1

(≥:) ∷ ℂ → ℂ → Τℂ
c1 ≥: c2 = c2 :≤: c1

-- Environment
data Ε = Ε 
	{ 
	  fn ∷ (Id,[ℂ]) -- Current Function
	, bb ∷ Id -- Current Basic Block
  , vfns ∷ [Id] -- Variadic functions
  	}

εΕ = Ε ((Global ""),[]) (Global "")

type ΕState α = State Ε α
type ℂState = ΕState (S.Set Τℂ')

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

δvfns ∷ ΕState ([Id])
δvfns = do γ@Ε{..} ← get
           (↣) vfns
           
-- Type Constraint Class
class TyConstr a where
    τℂ ∷ a → ℂState

class TyConstrR a where
    τℂr ∷ a → ΕState (S.Set Τℂ)

-- Type Inference Class
class Constr a where
	π ∷ a → ℂ