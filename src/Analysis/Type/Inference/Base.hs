{-# LANGUAGE UnicodeSyntax, RecordWildCards, TupleSections, TypeSynonymInstances, FlexibleInstances #-}
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
import UU.PPrint hiding ((<$>))

import Data.List

import Control.Monad
import Control.Applicative
import Control.Monad.State

import qualified Debug.Trace as Trace
import qualified Data.Map as M

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
α `classOf` β = error $ "classOf error: " -- ⧺ show α ⧺ " " ⧺ show β

-- Constraint Element
data ℂ = ℂτ Τα -- Type α
       | ℂπ Id -- Type of Id
       | ℂp ℂ Ταρ  -- Pointer to ℂ Τα
       | ℂλ [ℂ] ℂ  -- Function
       | ℂι ℂ [Int] -- for GEP instruction
       | ℂq ℂ -- type qualifier of id 
  deriving (Eq, Ord, Show)

simplify ∷ NamedTypes → ℂ → Maybe Τα
simplify nt c = case c of
  ℂτ t → Just t
  ℂπ n → Nothing
  ℂι c idxs → (\t → gepType nt t idxs) <$> simplify nt c
  ℂp c a → (\t → TyDer $ TyPtr t a) <$> simplify nt c
  ℂλ a r → (\ta tr → TyDer $ TyFun ta tr False) <$> mapM (simplify nt) a <*> simplify nt r
  ℂq c → Nothing

generalizeCons ∷ Int → ℂ → M.Map String Ταρ → (Int, ℂ, M.Map String Ταρ)
generalizeCons counter c env = case c of
  ℂτ τ → let (ncounter, nτ,env') = generalizeType counter τ env
         in (ncounter, ℂτ nτ,env') 
  ℂπ _ → (counter, c, env)
  ℂp cp ann → let (ncounter, nann, env') = generalizeAnn counter ann env
                  (ncounter',cp',env'') = generalizeCons ncounter cp env'
              in (ncounter', ℂp cp' nann, env'')
  ℂλ ca cr → 
    let (nc, nca, env') = 
          if length ca == 0
          then (counter, [],env)
          else let (nco, ncaa, env') = generalizeCons counter (last ca) env
              in foldr (\c (nco,lc, env') → 
                    let (nc, ncc, env) = generalizeCons nco c env'
                    in (nc, ncc:lc,env))  (nco,[ncaa],env') (init ca)
        (ncr', ncr,env'') = generalizeCons nc cr env'
    in (ncr', ℂλ nca ncr,env'')
  ℂι c idxs → let (ncounter, nc,env') = generalizeCons counter c env
              in (ncounter, ℂι nc idxs,env') 
  ℂq c → let (ncounter, nc, env') = generalizeCons counter c env
         in (ncounter, ℂq nc, env') 

isComplexConstr ∷ ℂ → Bool
isComplexConstr c = case c of
  ℂτ _ → False
  ℂπ _ → False
  ℂp cp _ → isComplexConstr cp
  ℂλ ca cr → any isComplexConstr (cr:ca)
  ℂι _ _ → True
  ℂq _ → True

vars ∷ ℂ → [Id]
vars c = case c of
  ℂτ _ → []
  ℂπ x → [x]
  ℂp cp _ → vars cp
  ℂλ ca cr → nub $ concatMap vars (cr:ca)
  ℂι ca _ → vars ca
  ℂq ca → vars ca

isℂτ ∷ ℂ → Bool
isℂτ (ℂτ _) = True
isℂτ _ = False

gepType ∷ NamedTypes → Τα → [Int] → Τα
gepType nt ty [] = error "gepType: empty indices"
gepType nt (TyDer (TyPtr ty _)) idxs = gepType' nt ty $ tail idxs
gepType nt ty idxs = error "gepType: wrong type"

gepType' ∷ NamedTypes → Τα → [Int] → Τα
gepType' nt ty [] = ty
gepType' nt ty (i:is) = 
  case ty of
    TyDer (TyAgg (TyArr n ety)) → 
      if i >= n 
      then Trace.trace "warning: possible array out of bounds" $ gepType' nt ety is
      else gepType' nt ety is
    TyDer (TyAgg (TyStr ns ne etys)) →
      case M.lookup ns nt of
        Nothing → getSubstruct nt etys (i:is)
        Just (TyDer (TyAgg (TyStr _ _ etys'))) → getSubstruct nt etys' (i:is)                  
    _ → error "gepType': not an aggregate type"

getSubstruct ∷ NamedTypes → [Τα] → [Int] → Τα
getSubstruct nt etys (i:is) =
  if i >= (length etys)
  then error "gepType: index is out of bounds"
  else gepType' nt (etys!!i) is


instance ShowType ℂ where
  showType γ (ℂτ τα)    = "Ctau(" ⧺ showType γ τα ⧺ ")"
  showType γ (ℂπ α)     = "Cv(" ⧺ (show $ pretty α) ⧺ ")"
  showType γ (ℂι c i)   = "Cgep(" ⧺ showType γ c ⧺ "," ⧺ show i ⧺ ")"
  showType γ (ℂp c ταρ) = "Cptr(" ⧺ showType γ c ⧺ "," ⧺ show ταρ ⧺ ")"
  showType γ (ℂλ cl c)  = "Cfn(" ⧺ foldr (\a r → showType γ a ⧺ "->" ⧺ r) "" cl ⧺ showType γ c ⧺ ")"
  showType γ (ℂq c)     = "Cq(" ⧺ showType γ c ⧺ ")"

-- Normalize the constraint
(⤜) ∷ ℂ → Ταρ → ℂ
(ℂτ τ) ⤜ ταρ = ℂτ $ TyDer $ TyPtr τ ταρ
c      ⤜ ταρ = ℂp c ταρ

-- Type Constraint
-- Simplify this later
type Τℂ' = (Τℂ,Int)

liftΤℂ ∷ Int → S.Set Τℂ → S.Set Τℂ'
liftΤℂ pc = S.map (,pc)

data Τℂ = ℂ :=: ℂ -- same type
--        | ℂ :<: ℂ -- subtyping i1 :<: i2
--        | ℂ :≤: ℂ -- less than 
--        | ℂ :≌: ℂ -- bit size equality
  deriving (Eq, Ord,Show)

generalizeTyConst ∷ Τℂ' → (Int, S.Set Τℂ',M.Map String Ταρ) → (Int, S.Set Τℂ',M.Map String Ταρ)
generalizeTyConst (c1 :=: c2, pc) (c, res,env) =
  let (nc,nc1,env') = generalizeCons c c1 env
      (nc',nc2,env'') = generalizeCons nc c2 env'
  in (nc', S.insert (nc1 :=: nc2,pc) res, env'')

instance ShowType Τℂ' where
  showType γ (c,i) = showType γ c ⧺ "@pc(" ⧺ show i ⧺ ")"

instance ShowType Τℂ where
  showType γ (α :=: β) = showType γ α ⧺ " :=: " ⧺ showType γ β
--  showType γ (α :<: β) = showType γ α ⧺ " :<: " ⧺ showType γ β
--  showType γ (α :≤: β) = showType γ α ⧺ " :≤: " ⧺ showType γ β
--  showType γ (α :≌: β) = showType γ α ⧺ " :≌: " ⧺ showType γ β

--(>:) ∷ ℂ → ℂ → Τℂ
--c1 >: c2 = c2 :<: c1

--(≥:) ∷ ℂ → ℂ → Τℂ
--c1 ≥: c2 = c2 :≤: c1

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