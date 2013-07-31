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
           | TyJumpTo Identifiers
  deriving (Eq, Ord)

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
--           | TyLab TysAnn TyAnn
--           | TyOpa String
  deriving (Eq, Ord)

data TyAgg = TyArr  Int TyAnn        -- Derived + Aggregate  
           | TyStr String Int TysAnn -- Derived + Aggregate
  deriving (Eq, Ord)

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

i ∷ Int → TyAnn
i n = TyPri $ TyInt n

instance Show TyAnn where
  show TyBot = "⊥"
  show TyUndef = "undef"
  show (TyPri t) = show t
  show (TyDer t) = show t
  show (TyJumpTo ids) = "{#" ++ show ids ++ "}"

instance Show TyPri where
  show TyVoid     = "void"
  show TyFloat    = "float"
  show TyLabel    = "label"
  show TyMetadata = "metadata"
  show (TyInt n)  = "i"++show n

instance Show TyDer where
  show (TyAgg t) = show t
  show (TyVec n t) = "<" ++ show n ++ " x " ++ show t ++ ">"
  show (TyFun [] t _) = "fn :: " ++ show t
  show (TyFun tys t _) = "fn :: " ++ (foldr (\x s -> show x ++ " -> " ++ s) (show t) tys)
  show (TyPtr ty tya) = "*(" ++ show ty ++ ") " ++ show tya

instance Show TyAgg where
  show (TyArr n t) = "[" ++ show n ++ " x " ++ show t ++ "]"
  show (TyStr nm n []) = "{" ++ nm ++ "}"
  show (TyStr nm n t) = "{" ++ (foldr (\x s -> show x ++ ", " ++ s) (show $ last t) (init t)) ++ "}"

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


