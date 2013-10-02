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

makeGlobal :: String -> Identifier
makeGlobal n = Global n

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

isolateFunction :: Identifier -> Module -> Module
isolateFunction i (Module id layout target gvars funs nmdtys) = 
  let fns = M.filterWithKey (\k _ -> i == k) funs
  in Module id layout target [] fns nmdtys 
 
isVariadic ∷ Function → Bool
isVariadic (FunctionDecl _ _ _ b _)   = b
isVariadic (FunctionDef  _ _ _ b _ _) = b

fnDefined :: Module -> [Identifier]
fnDefined mdl = let fns = M.elems $ getModFns mdl
                in catMaybes $ map isDefined fns
      
isDefined :: Function -> Maybe Identifier
isDefined (FunctionDecl _ _ _ _ _) = Nothing 
isDefined (FunctionDef  n _ _ _ _ _) = Just n

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
