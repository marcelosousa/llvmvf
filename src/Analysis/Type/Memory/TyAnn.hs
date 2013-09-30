{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.TyAnn
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Memory.TyAnn where

import Language.LLVMIR (Identifier, Identifiers)

import Control.Applicative
import Prelude.Unicode ((≡),(⧺))

import qualified Data.Map as M

import Debug.Trace 

type NamedTypes = M.Map String TyAnn

type TysAnn = [TyAnn]

-- I want to make this a TyAnn A
-- which receives the TyAnnot as A
data TyAnn = TyBot
           | TyUndef
           | TyPri TyPri
           | TyDer TyDer
  deriving (Eq, Ord, Show)

data TyPri = TyVoid
           | TyInt Int
           | TyFloat -- Just a tag
           | TyMetadata
           | TyLabel
  deriving (Eq, Ord)

data TyDer = TyAgg TyAgg
           | TyVec Int TyAnn
           | TyFun TysAnn TyAnn Bool
           | TyPtr TyAnn  TyAnnot
  deriving (Eq, Ord, Show)

data TyAgg = TyArr  Int TyAnn        -- Derived + Aggregate  
           | TyStr String Int TysAnn -- Derived + Aggregate
  deriving (Eq, Ord, Show)

data TyAnnot = UserAddr
             | KernelAddr
             | AnyAddr 
  deriving (Eq, Ord, Show)

{-
data TyAnnot = TyIOAddr
             | TyRegAddr TyRegAddr
             | TyAny 
  deriving (Eq, Ord)

data TyRegAddr = 
    UserAddr
  | KernelAddr KernelAddr
  | AnyRegAddr
  deriving (Eq, Ord)

data UserAddr = UserVirtualAddr
  deriving (Eq, Ord)

data KernelAddr = 
    KernelLogicalAddr 
  | KernelVirtualAddr
--  | KernelPhysicalAddr
  deriving (Eq, Ord)

anyRegAddr ∷ TyAnnot
anyRegAddr = TyRegAddr AnyRegAddr

kLogAddr ∷ TyAnnot
kLogAddr = TyRegAddr $ KernelAddr $ KernelLogicalAddr

kVirAddr ∷ TyAnnot
kVirAddr = TyRegAddr $ KernelAddr $ KernelVirtualAddr

uVirAddr ∷ TyAnnot
uVirAddr = TyRegAddr UserAddr
-}
i ∷ Int → TyAnn
i n = TyPri $ TyInt n

class ShowType α where
  showType ∷ NamedTypes → α → String

instance ShowType TyAnn where
  showType γ TyBot = "⊥"
  showType γ TyUndef = "undef"
  showType γ (TyPri t) = show t
  showType γ (TyDer t) = showType γ t

instance Show TyPri where
  show TyVoid     = "void"
  show TyFloat    = "float"
  show TyLabel    = "label"
  show TyMetadata = "metadata"
  show (TyInt n)  = "i"++show n

instance ShowType TyDer where
  showType γ (TyAgg t) = showType γ t
  showType γ (TyVec n t) = "<" ++ show n ++ " x " ++ showType γ t ++ ">"
  showType γ (TyFun [] t _) = "fn :: " ++ showType γ t
  showType γ (TyFun tys t _) = "fn :: " ++ (foldr (\x s -> showType γ x ++ " -> " ++ s) (showType γ t) tys)
  showType γ (TyPtr ty tya) = "*" ++ show tya ++ "(" ++ showType γ ty ++ ")"

instance ShowType TyAgg where
  showType γ (TyArr n t) = "[" ++ show n ++ " x " ++ showType γ t ++ "]"
  showType γ (TyStr nm n []) = 
    case M.lookup nm γ of
      Nothing → error "showType failed in struct"
      Just t  → nm ++ "=" ++ showType γ t
  showType γ (TyStr nm n t) = "{" ++ (foldr (\x s -> showType γ x ++ ", " ++ s) (showType γ $ last t) (init t)) ++ "}"

{-
instance Show TyAnnot where
  show TyIOAddr = "IOAddr"
  show (TyRegAddr t) = show t
  show TyAny = "AnyAddr"

instance Show TyRegAddr where
  show UserAddr = "UVirtualAddr"
  show (KernelAddr ka) = show ka
  show AnyRegAddr = "AnyRegAddr"

instance Show KernelAddr where
  show KernelLogicalAddr = "KLogicalAddr"
  show KernelVirtualAddr = "KVirtualAddr"
-}

class AEq α where
  (≅) ∷ NamedTypes → α → α → Maybe α

instance AEq TyAnn where
  (≅) nτ TyUndef τ = Just τ
  (≅) nτ τ TyUndef = Just τ
  (≅) nτ (TyDer τ1) (TyDer τ2) = TyDer <$> (≅) nτ τ1 τ2
  (≅) nτ τ1 τ2 = if τ1 ≡ τ2
                 then Just τ1
                 else Nothing

instance AEq TyDer where
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

instance AEq TyAgg where
  (≅) nτ (TyArr n τ1) (TyArr m τ2) = 
    if n ≡ m
    then TyArr n <$> (≅) nτ τ1 τ2
    else Nothing
  (≅) nτ (TyStr n i τ1) (TyStr m j τ2) = -- Source of all troubles
    eqTyStr nτ (n,i,τ1) (m,j,τ2)
  (≅) nτ τ1 τ2 = Nothing

cmpList ∷ NamedTypes → TysAnn → TysAnn → Maybe TysAnn
cmpList nτ a1 a2 = sequence $ map (uncurry ((≅) nτ)) $ zip a1 a2

eqTyStr :: NamedTypes -> (String,Int,[TyAnn]) -> (String,Int,[TyAnn]) -> Maybe TyAgg 
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
{-
    case M.lookup nr nmdtye of
      Nothing -> case M.lookup ns nmdtye of
        Nothing -> n == m && (and $ map (uncurry ((<=>) nmdtye)) $ zip r s)
        Just (TyStruct ns' m' s') -> eqStruct nmdtye (nr,n,r) (ns',m',s')
        Just _ -> False
      Just (TyStruct nr' n' r') -> case M.lookup ns nmdtye of
        Nothing -> eqStruct nmdtye (nr',n',r') (ns,m,s)
        Just (TyStruct ns' m' s') -> n' == m' && nr == ns
        Just _ -> False 
      Just _ -> False
-}
class IEq α where
  (≌) ∷ α → α → Maybe α

instance IEq TyAnnot where
  UserAddr   ≌ KernelAddr = Nothing
  UserAddr   ≌ UserAddr   = Just UserAddr
  KernelAddr ≌ KernelAddr = Just KernelAddr
  AnyAddr    ≌ a = Just a
  a ≌ β = β ≌ a 
{-
instance IEq TyAnnot where
  TyIOAddr ≌ (TyRegAddr α) = Nothing
  TyIOAddr ≌ _             = Just TyIOAddr
  (TyRegAddr α) ≌ TyIOAddr = Nothing
  (TyRegAddr α) ≌ TyAny     = Just $ TyRegAddr α
  (TyRegAddr α) ≌ (TyRegAddr β) = TyRegAddr <$> α ≌ β
  TyAny ≌ α     = Just α

instance IEq TyRegAddr where
  UserAddr ≌ (KernelAddr β) = Nothing
  UserAddr ≌ _ = Just UserAddr
  (KernelAddr α) ≌ UserAddr = Nothing
  (KernelAddr α) ≌ AnyRegAddr = Just $ KernelAddr α
  (KernelAddr α) ≌ (KernelAddr β) = 
    if α == β
    then Just (KernelAddr α)
    else Nothing
  AnyRegAddr  ≌ α = Just α
-}

