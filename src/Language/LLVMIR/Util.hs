{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Util
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Util where

import Language.LLVMIR

import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Prelude.Unicode ((⧺))

nextUnique :: Int -> (Int, Int)
nextUnique u = (u+1, u)

emptyFunction :: Function
emptyFunction = FunctionDef (Global "undefined") ExternalLinkage TyVoid False [] []

valueIdentifier :: Value -> Maybe Identifier
valueIdentifier (Id i _) = Just i
valueIdentifier (Constant (GlobalValue (GlobalVariable i _))) = Just i
valueIdentifier (Constant (GlobalValue (FunctionValue i _))) = Just i
valueIdentifier _ = Nothing

valueIdentifier' :: String -> Value -> Identifier
valueIdentifier' e v = 
  case valueIdentifier v of
    Nothing -> error $ e ++ ":valueIdentifier " ++ show v
    Just i  -> i

identifierName :: Identifier -> String
identifierName (Global n) = n
identifierName (Local  n) = n

isGlobalId ∷ Identifier → Bool
isGlobalId (Global _) = True
isGlobalId _ = False

identifierValue :: Value -> String
identifierValue v = identifierName $ valueIdentifier' "" v

findBasicBlock :: Identifier -> Function -> BasicBlock
findBasicBlock i (FunctionDecl _ _ _ _ _) = error $ "findBasicBlock: " ++ show i ++ " not found."
findBasicBlock i (FunctionDef  _ _ _ _ _ body) =
    fromMaybe (error $ "findBasicBlock: " ++ show i ++ " not found.") $
    find (isBasicBlock i) body

isBasicBlock :: Identifier -> BasicBlock -> Bool
isBasicBlock i (BasicBlock j _ _ _) = i==j

fnBasicBlockIds :: Function -> [Identifier]
fnBasicBlockIds (FunctionDecl _ _ _ _ _) = []
fnBasicBlockIds (FunctionDef  _ _ _ _ _ body) = map getBBId body

getBBId :: BasicBlock -> Identifier
getBBId (BasicBlock i _ _ _) = i

getModFns :: Module -> Functions
getModFns (Module id layout target gvars funs nmdtys) = funs

δModNmds ∷ Module → NamedTypes
δModNmds (Module id layout target gvars funs nmdtys) = nmdtys

variadicFns ∷ Module → [Identifier]
variadicFns (Module id layout target gvars funs nmdtys) = 
  let fns = M.filter isVariadic funs
  in M.keys fns

isVariadic ∷ Function → Bool
isVariadic (FunctionDecl _ _ _ b _)   = b
isVariadic (FunctionDef  _ _ _ b _ _) = b

infoValue :: Value -> Either Identifier (Identifier, [Int])
infoValue v = case v of
  Constant (ConstantExpr (GetElementPtrConstantExpr τ i idxs)) ->
    let vi = valueIdentifier' "infovalue gep" i
        is = map intConstantValue idxs
    in Right $ (vi,is) 
  _ -> Left $ valueIdentifier' "infovalue" v


intConstantValue :: Value -> Int
intConstantValue (Constant (SmpConst (ConstantInt v _))) = v
intConstantValue sc = error $ "intConstantValue: " ++ show sc

getNames ∷ Module → S.Set Identifier
getNames (Module id layout target gvars funs nmdtys) = 
    let gvarsid = S.fromList $ map getNameGlobal gvars 
        funsid  = S.fromList $ M.keys funs
    in gvarsid `S.union` funsid

getNameGlobal ∷ Global → Identifier
getNameGlobal (GlobalVar name _ _ _ _ _ _) = name

class Typing α where
  typeOf ∷ α → Type 

instance Typing Value where
  typeOf (Id _ τ) = τ
  typeOf (Constant c) = typeOf c

instance Typing Constant where
  typeOf c = case c of
    SmpConst sc    → typeOf sc
    CmpConst cc    → typeOf cc
    GlobalValue gv → typeOf gv
    ConstantExpr e → typeOf e
    _ → TyUndefined

instance Typing SimpleConstant where
   typeOf (ConstantInt _ τ) = τ
   typeOf (ConstantFP  fp)  = typeOf fp
   typeOf (ConstantPointerNull τ) = τ

instance Typing ConstantFP where
   typeOf (ConstantFPFloat  _ τ) = τ
   typeOf (ConstantFPDouble _ τ) = τ

instance Typing ComplexConstant where
  typeOf cc = case cc of
    ConstantAggregateZero  τ → τ
    ConstantDataSequential c → typeOf c
    ConstantStruct  τ _ → τ
    ConstantArray   τ _ → τ
    ConstantVector  τ _ → τ

instance Typing ConstantDataSequential where
   typeOf cds = case cds of
    ConstantDataArray  τ _ → τ
    ConstantDataVector τ _ → τ

instance Typing GlobalValue where
  typeOf gv = case gv of
    FunctionValue  n τ → τ
    GlobalAlias    n τ → τ
    GlobalVariable n τ → τ

instance Typing ConstantExpr where
  typeOf expr = case expr of
    UnaryConstantExpr         _ _ _ τ → τ
    CompareConstantExpr       cmpExpr → error "typeOf CompareConstantExpr"
    GetElementPtrConstantExpr τ s is  → τ
    _ → error "typeOf other expression"
{-    ExtractElementConstantExpr
    ExtractValueConstantExpr
    InsertElementConstantExpr
    InsertValueConstantExpr
    SelectConstantExpr
    ShuffleVectorConstantExpr
    BinaryConstantExpr
-}

class Sizable α where
  sizeOf ∷ α → Int

instance Sizable Type where
  sizeOf τ = case τ of
    TyInt      p        → p
    TyFloatPoint τ      → sizeOf τ 
    TyPointer  _        → 8
    TyVector   numEl τ  → numEl * sizeOf τ
    TyArray    numEl τ  → numEl * sizeOf τ
    TyStruct _ numEl τs → sum $ map sizeOf τs
    Tyx86MMX            → 64
    _ → error $ "sizeOf type: " ⧺ show τ

instance Sizable TyFloatPoint where
  sizeOf τ = case τ of
    TyHalf      → 16
    TyFloat     → 32
    TyDouble    → 64
    TyFP128     → 128
    Tyx86FP80   → 80
    TyPPCFP128  → 128

(<=>) :: NamedTypes -> Type -> Type -> Bool
(<=>) nmdtye TyVoid      TyVoid      = True
(<=>) nmdtye Tyx86MMX    Tyx86MMX    = True
(<=>) nmdtye TyLabel     TyLabel     = True
(<=>) nmdtye TyMetadata  TyMetadata  = True
(<=>) nmdtye TyOpaque    TyOpaque    = True
(<=>) nmdtye TyUndefined TyUndefined = True
(<=>) nmdtye (TyInt p)         (TyInt n)         = p == n
(<=>) nmdtye (TyFloatPoint p)  (TyFloatPoint n)  = p == n
(<=>) nmdtye (TyPointer p)     (TyPointer n)     = (<=>) nmdtye p n
(<=>) nmdtye (TyVector n r)    (TyVector m s)    = n == m && (<=>) nmdtye r s
(<=>) nmdtye (TyArray  n r)    (TyArray  m s)    = n == m && (<=>) nmdtye r s
(<=>) nmdtye (TyStruct nr n r) (TyStruct ns m s) = eqStruct nmdtye (nr,n,r) (ns,m,s)
(<=>) nmdtye x y = False

eqStruct :: NamedTypes -> (String,Int,[Type]) -> (String,Int,[Type]) -> Bool
eqStruct nmdtye ("",n,r) ("",m,s) = n == m && (and $ map (uncurry ((<=>) nmdtye)) $ zip r s)
eqStruct nmdtye ("",n,r) (ns,m,s) = 
  case M.lookup ns nmdtye of
    Nothing -> n == m && (and $ map (uncurry ((<=>) nmdtye)) $ zip r s)
    Just (TyStruct ns' m' s') -> n==m && (and $ map (uncurry ((<=>) nmdtye)) $ zip r s')
    Just _ -> False
eqStruct nmdtye (nr,n,r) ("",m,s) = 
  case M.lookup nr nmdtye of
    Nothing -> n == m && (and $ map (uncurry ((<=>) nmdtye)) $ zip r s)
    Just (TyStruct nr' n' r') -> eqStruct nmdtye (nr',n',r') ("",m,s)
    Just _ -> False
eqStruct nmdtye (nr,n,r) (ns,m,s) = 
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

isSmpTy :: Type -> Bool
isSmpTy (TyInt x)  = let n = div x 8
                     in n == 1 || n == 2 || n == 4 || n == 8
isSmpTy (TyFloatPoint TyFloat) = True
isSmpTy (TyFloatPoint TyDouble) = True
isSmpTy _ = False 

isInt :: Type -> Bool
isInt (TyInt _) = True
isInt _ = False

isFloat :: Type -> Bool
isFloat (TyFloatPoint _) = True
isFloat _ = False

isVector :: Type -> Bool
isVector (TyVector s ty) = True
isVector _ = False

isPointer :: Type -> Bool
isPointer (TyPointer _) = True
isPointer _ = False

isAgg :: Type -> Bool
isAgg (TyArray _ _) = True
isAgg (TyStruct _ _ _) = True
isAgg _ = False

getIntValue :: Value -> Int
getIntValue (Constant (SmpConst (ConstantInt i _))) = i
getIntValue v = error $ "getIntvalue: not const Int" ⧺ show v


lookupBasicBlock :: BasicBlocks -> Identifier -> Maybe BasicBlock
lookupBasicBlock [] l = Nothing
lookupBasicBlock (bb@(BasicBlock l _ _ _):bbs) i | i == l = Just bb
                                               | otherwise = lookupBasicBlock bbs i


-- TODO: Complete this definition
isFstClass :: Type -> Bool
isFstClass (TyInt _)        = True
isFstClass (TyFloatPoint _) = True
isFstClass (TyPointer _)    = True
isFstClass (TyVector _ _)   = True
isFstClass (TyStruct _ _ _) = True
isFstClass (TyArray _ _)    = True
isFstClass TyLabel          = True
isFstClass TyMetadata       = True
isFstClass _ = False


notAggFstClass :: Type -> Bool
notAggFstClass ty = (not $ isAgg ty) && isFstClass ty