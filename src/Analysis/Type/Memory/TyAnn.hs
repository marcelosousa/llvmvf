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

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as MB

import qualified Debug.Trace as Trace

trace s f = f
--trace = Trace.trace

type NamedTypes = M.Map String TyAnn

type TysAnn = [TyAnn]

-- I want to make this a TyAnn A
-- which receives the TyAnnot as A
data TyAnn = TyBot
           | TyUndef
           | TyPri TyPri
           | TyDer TyDer
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
  deriving (Eq, Ord)

data TyAgg = TyArr  Int TyAnn        -- Derived + Aggregate  
           | TyStr String Int TysAnn -- Derived + Aggregate
  deriving (Eq, Ord)

data TyAnnot = UserAddr
             | KernelAddr
             | AnyAddr 
             | TyVar String
  deriving (Eq, Ord, Show)

resolve ∷ M.Map String [TyAnnot] → TyAnn → TyAnn
resolve env ty = trace ("resolve: " ++ show ty ) $ case ty of
  TyDer tyd → case tyd of
    TyAgg tya → case tya of
      TyArr i tye → let ntye = resolve env tye
                    in TyDer $ TyAgg $ TyArr i ntye
      TyStr sn n tys → let ntys = map (resolve env) tys
                       in TyDer $ TyAgg $ TyStr sn n ntys
    TyVec i tye → let ntye = resolve env tye
                  in TyDer $ TyVec i ntye
    TyFun tyes tyr b → let ntyes = map (resolve env) tyes
                           ntyr = resolve env tyr
                       in TyDer $ TyFun ntyes ntyr b
    TyPtr typ tya → let ntyp = resolve env typ
                        ntya = resolveAnn [] env tya 
                    in TyDer $ TyPtr ntyp ntya
  _ → ty

resolveAnn ∷ [String] → M.Map String [TyAnnot] → TyAnnot → TyAnnot
resolveAnn log env ann@(TyVar s) = trace ("resolveAnn: " ++ show log ++ " " ++ show ann ) $
  fst $ resolveAnnAux log env ann
resolveAnn log env a = a

resolveAnnAux log env ann@(TyVar s) =
  if s `elem` log 
  then (AnyAddr,log) -- Loop
  else 
    let vals = M.assocs env
        cleft = MB.fromMaybe [] $ M.lookup s env
        helper = \(a,v) → (TyVar a):(L.delete ann v)
        cright = concatMap helper $ filter (\(a,v) → ann `elem` v) vals
        c = L.nub $ cleft ++ cright
    in computeAnnot (s:log) env c
resolveAnnAux log env a = (a,log) 

computeAnnot ∷ [String] → M.Map String [TyAnnot] → [TyAnnot] → (TyAnnot, [String])
computeAnnot log env [] = (AnyAddr,log)
computeAnnot log env (x:xs) = 
  let (ta,nlog) = resolveAnnAux log env x
      (tb,nlog') = computeAnnot nlog env xs
  in case ta ≌ tb of
    Nothing → error $ "computeAnnot: Unification error " ++ show log ++ "\n" ++ show ta ++ "\n" ++ show tb
    Just t → (t,nlog')

resolveAnnot ∷ [String] → M.Map String [TyAnnot] → TyAnnot → TyAnnot → TyAnnot
resolveAnnot log env lhs rhs = 
  let ta = fst $ resolveAnnAux log env lhs
      tb = fst $ resolveAnnAux log env rhs
  in case ta ≌ tb of
    Nothing → error $ "resolveAnnot: Unification error " ++ show log ++ "\n" ++ show (lhs,ta) ++ "\n" ++ show (rhs,tb)
    Just t → t

getTypeQual ∷ TyAnn → Maybe TyAnnot
getTypeQual ty = case ty of
  TyDer (TyPtr _ ta) → Just ta
  _ → Nothing

expandType ∷ NamedTypes → Int → TyAnn → [Int] → (Int, TyAnn, TyAnn)
expandType nt counter ty [] = error "expandType: empty indices list"
expandType nt counter ty idxs =
  case ty of 
    TyDer (TyPtr ity ann) → 
      let (nc, nity, rty) = expandTypeAgg nt counter False ity $ tail idxs
          nty = TyDer (TyPtr nity ann)
      in (nc, nty, rty)
    _ → error "expandType: type is not a pointer" 

expandTypeAgg ∷ NamedTypes → Int → Bool → TyAnn → [Int] → (Int, TyAnn, TyAnn)
expandTypeAgg nt counter isNty ty [] = (counter, ty, ty)
expandTypeAgg nt counter isNty ty (x:xs) = trace ("expandTypeAgg " ++ show x) $ case ty of
  TyDer (TyAgg ity) → 
    case ity of
      TyArr i tye → 
        let (nc, nite, rty) = expandTypeAgg nt counter isNty tye xs
            nity = TyDer $ TyAgg $ TyArr i nite
        in if i>x
           then (nc, nity, rty)
           else trace "expandTypeAgg: possible array out of bounds" $ (nc, nity, rty)
      TyStr sn n tys → 
        if length tys >= x && (not $ null tys)
        then                        -- we already have it
          let ity = tys!!x 
              (nc, genTy,_) = generalizeType counter ity M.empty
              (nc',nity,rty) = expandTypeAgg nt nc False genTy xs
              (btys,atys) = splitAt x tys  
              ntys = btys ++ (nity:tail atys)
              nty = TyDer $ TyAgg $ TyStr sn n ntys
          in (nc',nty,rty)
        else case M.lookup sn nt of -- search for it 
          Nothing → error $ "expandTypeAgg: not in map " ++ show ty
          Just nty → if isNty
                     then error $ "expandTypeAgg: out of bounds " ++ show nty
                     else expandTypeAgg nt counter True nty (x:xs)
  _ → error $ "expandTypeAgg: " ++ show ty ++ " is not aggregate " ++ show (x:xs)

generalizeType ∷ Int → TyAnn → M.Map String [TyAnnot] → (Int, TyAnn, M.Map String [TyAnnot]) 
generalizeType counter ty env = case ty of
  TyDer tyd → case tyd of
    TyAgg tya → case tya of
      TyArr i tye → let (nc,ntye,env') = generalizeType counter tye env
                    in (nc, TyDer $ TyAgg $ TyArr i ntye, env')
      TyStr sn n tys → let (nc,ntys,env') = generalizeTypes counter tys env
                       in (nc, TyDer $ TyAgg $ TyStr sn n ntys,env')
    TyVec i tye → let (nc,ntye,env') = generalizeType counter tye env
                  in (nc, TyDer $ TyVec i ntye,env')
    TyFun tyes tyr b → let (nc,ntyes,env') = generalizeTypes counter tyes env
                           (nc',ntyr,env'') = generalizeType nc tyr env'
                       in (nc', TyDer $ TyFun ntyes ntyr b,env'')
    TyPtr typ tya → let (nc,ntyp,env') = generalizeType counter typ env
                        (nc',ntya,env'') = generalizeAnn nc tya env'
                    in (nc', TyDer $ TyPtr ntyp ntya,env'')
  _ → (counter, ty, env)

generalizeAnn ∷ Int → TyAnnot → M.Map String [TyAnnot] → (Int, TyAnnot, M.Map String [TyAnnot])
generalizeAnn counter tya env = case tya of
  AnyAddr → (counter+1, TyVar $ "tyqVar"++show counter, env)
  _ → let var = "tyqVar"++show counter
      in (counter+1, TyVar var, M.insertWith (++) var [tya] env)

generalizeTypes ∷ Int → TysAnn → M.Map String [TyAnnot] → (Int, TysAnn, M.Map String [TyAnnot])
generalizeTypes c [] env = (c,[],env)
generalizeTypes c xs env = 
  let (nc,ty,env') = generalizeType c (last xs) env
  in foldr (\tye (ic,r,env) → 
    let (nic, ntye,env') = generalizeType ic tye env
    in (nic, ntye:r,env')) (nc,[ty],env) (init xs)

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
--  | KernelPhycalAddr
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

instance Show TyAnn where
  show TyBot = "⊥"
  show TyUndef = "undef"
  show (TyPri t) = show t
  show (TyDer t) = show t

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
  show (TyPtr ty tya) = "*" ++ show tya ++ "(" ++ show ty ++ ")"

instance Show TyAgg where
  show (TyArr n t) = "[" ++ show n ++ " x " ++ show t ++ "]"
  show (TyStr "" 0 []) = "{}*"
  show (TyStr nm n t) = nm ++ "{" ++ (foldr (\x s -> show x ++ ", " ++ s) "" t) ++ "}"

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
                then TyStr "BLA" n <$> cmpList nτ ατ βτ
                else Nothing
    (nβ,m,βτ) → case M.lookup nβ nτ of
      Nothing → if n ≡ m && length ατ ≡ length βτ
                then TyStr "BLA" n <$> cmpList nτ ατ βτ
                else Nothing
      Just η@(TyDer (TyAgg (TyStr nη o ητ))) →
        if nβ ≡ nη
        then if n ≡ o
             then TyStr "BLA" n <$> cmpList nτ ατ ητ
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
  UserAddr   ≌ α          = Just UserAddr
  KernelAddr ≌ UserAddr   = Nothing
  KernelAddr ≌ α          = Just KernelAddr
  TyVar s    ≌ UserAddr   = Just UserAddr
  TyVar s    ≌ KernelAddr = Just KernelAddr
  TyVar s    ≌ AnyAddr    = Just $ TyVar s
  TyVar s    ≌ TyVar r    = 
    if s == r
    then Just $ TyVar s
    else Nothing
  AnyAddr    ≌ α          = Just α
  
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

