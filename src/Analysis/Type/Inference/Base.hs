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
       | ℂι ℂ [Int] Ταρ -- for GEP instruction
       | ℂq ℂ -- type qualifier of id 
       | ℂa Τα ℂ
  deriving (Eq, Ord)

-- generate all constraints with type variables
-- 
simplify ∷ NamedTypes → ℂ → Maybe Τα
simplify nt c = case c of
  ℂτ t → Just t
  ℂπ n → Nothing
  ℂι c idxs a → (\t → gepType nt t idxs a) <$> simplify nt c
  ℂp c a → (\t → TyDer $ TyPtr t a) <$> simplify nt c
  ℂλ a r → (\ta tr → TyDer $ TyFun ta tr False) <$> mapM (simplify nt) a <*> simplify nt r
  ℂq c → Nothing

generalizeCons ∷ Int → ℂ → M.Map String [Ταρ] → (Int, ℂ, M.Map String [Ταρ])
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
  ℂι c idxs ann → let (ncounter, nc,env') = generalizeCons counter c env
                      (ncounter', nann, env'') = generalizeAnn ncounter ann env'
                in (ncounter', ℂι nc idxs nann,env'') 
  ℂq c → let (ncounter, nc, env') = generalizeCons counter c env
         in (ncounter, ℂq nc, env') 

isComplexConstr ∷ ℂ → Bool
isComplexConstr c = case c of
  ℂτ _ → False
  ℂπ _ → False
  ℂp cp _ → isComplexConstr cp
  ℂλ ca cr → any isComplexConstr (cr:ca)
  ℂι _ _ _ → True
  ℂq _ → True
  ℂa _ _ → True

vars ∷ ℂ → [Id]
vars c = case c of
  ℂτ _ → []
  ℂπ x → [x]
  ℂp cp _ → vars cp
  ℂλ ca cr → nub $ concatMap vars (cr:ca)
  ℂι ca _ _ → vars ca
  ℂq ca → vars ca
  ℂa _ ca → vars ca

isℂτ ∷ ℂ → Bool
isℂτ (ℂτ _) = True
isℂτ _ = False

gepType ∷ NamedTypes → Τα → [Int] → Ταρ → Τα
gepType nt ty [] ann = error "gepType: empty indices"
gepType nt (TyDer (TyPtr ty _)) idxs ann = gepType' nt ty (tail idxs) ann
gepType nt ty idxs ann = error "gepType: wrong type"

gepType' ∷ NamedTypes → Τα → [Int] → Ταρ → Τα
gepType' nt ty [] ann = TyDer $ TyPtr ty ann
gepType' nt ty (i:is) ann = 
  case ty of
    TyDer (TyAgg (TyArr n ety)) → 
      if i >= n 
      then Trace.trace "warning: possible array out of bounds" $ gepType' nt ety is ann
      else gepType' nt ety is ann
    TyDer (TyAgg (TyStr ns ne etys)) →
      case M.lookup ns nt of
        Nothing → getSubstruct nt etys (i:is) ann
        Just (TyDer (TyAgg (TyStr _ _ etys'))) → getSubstruct nt etys' (i:is) ann                  
    _ → error "gepType': not an aggregate type"

getSubstruct ∷ NamedTypes → [Τα] → [Int] → Ταρ → Τα
getSubstruct nt etys (i:is) ann =
  if i >= (length etys)
  then error "gepType: index is out of bounds"
  else gepType' nt (etys!!i) is ann


instance Show ℂ where
  show (ℂτ τα)    = "Ctau(" ⧺ show τα ⧺ ")"
  show (ℂπ α)     = "Cv(" ⧺ (show $ pretty α) ⧺ ")"
  show (ℂι c i ταρ)   = "Cgep(" ⧺ show c ⧺ "," ⧺ show i ⧺ "," ⧺ show ταρ ⧺ ")"
  show (ℂp c ταρ) = "Cptr(" ⧺ show c ⧺ "," ⧺ show ταρ ⧺ ")"
  show (ℂλ cl c)  = "Cfn(" ⧺ foldr (\a r → show a ⧺ "->" ⧺ r) "" cl ⧺ show c ⧺ ")"
  show (ℂq c)     = "Cq(" ⧺ show c ⧺ ")"
  show (ℂa t c)   = "Ca(" ⧺ show t ⧺ ", " ⧺ show c ⧺ ")"

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
  deriving (Eq, Ord)

generalizeTyConst ∷ Τℂ' → (Int, S.Set Τℂ',M.Map String [Ταρ]) → (Int, S.Set Τℂ',M.Map String [Ταρ])
generalizeTyConst (c1 :=: c2, pc) (c, res,env) =
  let (nc,nc1,env') = generalizeCons c c1 env
      (nc',nc2,env'') = generalizeCons nc c2 env'
  in (nc', S.insert (nc1 :=: nc2,pc) res, env'')

--instance Show Τℂ' where
-- show (c,i) = show c ⧺ "@pc(" ⧺ show i ⧺ ")"

instance Show Τℂ where
  show (α :=: β) = show α ⧺ " :=: " ⧺ show β
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