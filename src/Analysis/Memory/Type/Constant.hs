-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Constant
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Convinced the that the typing relation for constants is:
-- Gamma |- c : tau
-------------------------------------------------------------------------------

module Analysis.Memory.Type.Constant (typeConstant,typeConstant') where

import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.Type.Util
import Language.LLVMIR
import qualified Data.Map as M
import qualified Data.Maybe as MB

-- Type Constant
typeConstant :: TyEnv -> Constant -> Maybe Type
typeConstant tye c = case c of
  UndefValue      -> Just TyUndefined
  PoisonValue     -> error "typeConstant: PoisonValue not supported"
  BlockAddr       -> error "typeConstant: BlockAddr not supported"
  SmpConst sc     -> typeSimpleConstant tye sc
  CmpConst cc     -> typeComplexConstant tye cc
  GlobalValue gv  -> typeGlobalValue tye gv 
  ConstantExpr ec -> Just $ typeExpression tye ec 

-- typeSimpleConstant
typeSimpleConstant :: TyEnv -> SimpleConstant -> Maybe Type
typeSimpleConstant tye c = case c of
  -- ConstantInt
  ConstantInt _ ty@(TyInt x) -> Just ty
  ConstantInt _ err          -> error $ "typeSimpleConstant: ConstantInt must be of type iX. Given: " ++ show err
  -- ConstantFP
  ConstantFP fp -> typeConstantFP tye fp
  -- ConstantPointerNull
  ConstantPointerNull ty@(TyPointer t) -> Just ty
  ConstantPointerNull err              -> error $ "typeSimpleConstant: ConstantPointerNull must be of type Ptr. Given: " ++ show err 

-- typeConstantFP
typeConstantFP :: TyEnv -> ConstantFP -> Maybe Type
typeConstantFP tye (ConstantFPFloat  _ ty@(TyFloatPoint TyFloat))  = Just ty
typeConstantFP tye (ConstantFPDouble _ ty@(TyFloatPoint TyDouble)) = Just ty
typeConstantFP _   cfp = error $ "typeConstantFP: " ++ show cfp 

-- typeComplexConstant
typeComplexConstant :: TyEnv -> ComplexConstant -> Maybe Type
typeComplexConstant tye c = case c of
  ConstantAggregateZero  ty  -> Just ty
  ConstantDataSequential cds -> typeConstantDataSequential tye cds
  ConstantStruct     ty vals -> case ty of
        (TyStruct _ n tys) -> let lty = map (typeValue tye) vals
                                  zlty = zip tys lty 
                                  c = all (\(a,b) -> a == b) zlty -- Implies that the order is the same.
                              in if n == length vals && n == length tys && c
                                 then Just ty
                                 else Nothing
        _ -> error $ "typeComplexConstant: ConstantStruct " ++ show ty  
  ConstantArray      ty vals -> case ty of
        (TyArray n ety) -> let lty = map (typeValue tye) vals
                               c = all (==ety) lty
                           in if n == length vals && c
                              then Just ty
                              else Nothing 
  ConstantVector     ty vals -> case ty of
        (TyVector n ety) -> let lty = map (typeValue tye) vals
                                c = all (==ety) lty
                            in if n == length vals && c
                               then Just ty
                               else Nothing 

-- typeConstantDataSequential
typeConstantDataSequential :: TyEnv -> ConstantDataSequential -> Maybe Type
typeConstantDataSequential tye (ConstantDataArray ty@(TyArray _ ety)  _)  = if isSmpTy ety
                                                                            then Just ty
                                                                            else error $ errorMsg "ConstantDataArray" "TyArray" ty
typeConstantDataSequential tye (ConstantDataVector ty@(TyVector _ ety) _) = if isSmpTy ety
                                                                            then Just ty
                                                                            else error $ errorMsg "ConstantDataVector" "TyVector" ty
typeConstantDataSequential tye c = error $ "typeConstantDataSequential: Given " ++ show c

errorMsg s r v = "typeConstantDataSequential: " ++ s ++ " does not have " ++ r ++ " with a simple type. Given: " ++ show v

-- typeValue
typeValue :: TyEnv -> Value -> Type
typeValue tye (Id v ty) =  typeValueGen tye v ty "tyValue:Id"
typeValue tye (Constant c) = MB.fromMaybe (error "typeValue fromMaybe") $ typeConstant tye c


typeGlobalValue :: TyEnv -> GlobalValue -> Maybe Type 
typeGlobalValue tye (FunctionValue  n ty) = Just $ typeValueGen tye n ty "typeGlobalValue:FunctionValue"
typeGlobalValue tye (GlobalAlias    n ty) = Just $ typeValueGen tye n ty "typeGlobalValue:GlobalAlias"
typeGlobalValue tye (GlobalVariable n ty) = Just $ typeValueGen tye n ty "typeGlobalValue:GlobalVariable"

typeValueGen :: TyEnv -> Identifier -> Type -> String -> Type
typeValueGen tye v ty s = case M.lookup v tye of
                            Nothing -> error $ s ++ ": " ++ show v ++ " is not in the context: " ++ show tye
                            Just t  -> if t == ty
                                       then ty
                                       else error $ s ++ ": Given " ++ show ty ++ ". Expected " ++ show t


-- typeExpression
typeExpression :: TyEnv -> ConstantExpr -> Type
typeExpression tye (CompareConstantExpr ce) = typeCompareConstantExpr tye ce
typeExpression tye (GetElementPtrConstantExpr v idxs) = typeGetElementPtrConstantExpr tye v idxs
typeExpression tye (UnaryConstantExpr name _ _ _) = error $ "typeExpression: UnaryConstantExpr " ++ show name ++ " not supported."
typeExpression tye e = error $ "typeExpression: " ++ show e ++ " not supported."


typeGetElementPtrConstantExpr :: TyEnv -> Value -> Values -> Type
typeGetElementPtrConstantExpr tye v idxs = case typeValue tye v of
      TyPointer ty -> if and $ map (isInt . typeValue tye) idxs
                      then if isAgg ty
                           then getTypeAgg ty $ map getIntValue idxs
                           else error $ "typeGetElementPtrConstantExpr: " ++ show ty ++ " is not aggregate."  
                      else error $ "typeGetElementPtrConstantExpr: not all indices are integers" 
      ty -> error $ "typeGetElementPtrConstantExpr: " ++ show ty 

getIntValue :: Value -> Int
getIntValue (Constant (SmpConst (ConstantInt i _))) = i
getIntValue _ = -1

getTypeAgg :: Type -> [Int] -> Type
getTypeAgg ty [] = ty
getTypeAgg ty (x:xs) = case ty of 
      TyArray s t -> if isAgg t 
                     then if x < 0 || x >= s 
                          then error $ "getTypeAgg: out of bounds"
                          else getTypeAgg t xs
                     else error $ "getTypeAgg: " ++ show t ++ " is not aggregate. (1)"  
      TyStruct _ s t -> if x < 0 || x >= s 
                        then error $ "getTypeAgg: out of bounds"
                        else let nt = t !! x
                             in if isAgg nt 
                                then getTypeAgg nt xs
                                else error $ "getTypeAgg: " ++ show nt ++ " is not aggregate. (2)" 
      _  -> error $ "getTypeAgg: " ++ show ty ++ " is not aggregate. (3)"   

typeCompareConstantExpr :: TyEnv -> CompareConstantExpr -> Type
typeCompareConstantExpr tye (ICmpExpr _ ty op1 op2) = let top1 = typeValue tye op1
                                                          top2 = typeValue tye op2
                                                          (b,i) = isComparableTypeInt top1
                                                      in if top1 == top2 && b && ((i==0 && isInt ty) || (i==1 && isVector ty)) 
                                                         then ty
                                                         else error "typeCompareConstantExpr: error" 
typeCompareConstantExpr tye (FCmpExpr _ ty op1 op2) = let top1 = typeValue tye op1
                                                          top2 = typeValue tye op2
                                                          (b,i) = isComparableTypeFloat top1
                                                      in if top1 == top2 && b && ((i==0 && isInt ty) || (i==1 && isVector ty)) 
                                                         then ty
                                                         else error "typeCompareConstantExpr: error" 

isComparableTypeInt :: Type -> (Bool, Int)
isComparableTypeInt (TyInt _) = (True, 0)
isComparableTypeInt (TyVector _ (TyInt _)) = (True,1)
isComparableTypeInt (TyPointer _) = (True,0) -- Suspicious
isComparableTypeInt _ = (False,0)

isComparableTypeFloat :: Type -> (Bool, Int)
isComparableTypeFloat (TyFloatPoint _) = (True,0)
isComparableTypeFloat (TyVector _ (TyFloatPoint _)) = (True,1)
isComparableTypeFloat (TyPointer _) = (True,0) -- Suspicious
isComparableTypeFloat _ = (False, 0)

-------------------------------------------------------------------
-- Constant TyAnn Inference
typeConstant' :: TyAnnEnv -> Constant -> (TyAnn, TyAnnEnv)
typeConstant' tye c = case c of
  UndefValue      -> (T.TyUndef, tye)
  PoisonValue     -> error "typeConstant: PoisonValue not supported"
  BlockAddr       -> error "typeConstant: BlockAddr not supported"
  SmpConst sc     -> sconstTyInf tye sc
  CmpConst cc     -> cconstTyInf tye cc
  GlobalValue gv  -> gvTyInf tye gv 
  ConstantExpr ec -> econstTyInf tye ec 

-- Simple Constant TyAnn Inference
sconstTyInf :: TyAnnEnv -> SimpleConstant -> (TyAnn, TyAnnEnv)
sconstTyInf tye c = case c of
  ConstantInt _ ty -> case ty of
       TyInt s -> (T.TyPri $ T.TyInt s, tye)
       err     -> error "typeConstant: ConstantInt must be of type iX" 
  ConstantFP fp -> (T.TyPri T.TyFloat, tye)
  ConstantPointerNull ty -> case ty of
       TyPointer t -> (liftTy ty, tye)
       _           -> error "typeConstant: ConstantPointerNull must be of type Ptr" 

-- Complex Constant TyAnn Inference
cconstTyInf :: TyAnnEnv -> ComplexConstant -> (TyAnn, TyAnnEnv)
cconstTyInf tye c = case c of
  ConstantAggregateZero  ty  -> (liftTy ty, tye) 
  ConstantDataSequential cds -> cdsconstTyInf tye cds
  ConstantStruct     ty vals -> (liftTy ty, tye) -- TODO 
  ConstantArray      ty vals -> (liftTy ty, tye) -- TODO
  ConstantVector     ty vals -> (liftTy ty, tye) -- TODO

-- Constant Data Sequential TyAnn Inference
-- TODO check that all vals are ConstantInt/ConstantFP
cdsconstTyInf :: TyAnnEnv -> ConstantDataSequential -> (TyAnn, TyAnnEnv)
cdsconstTyInf tye c = case c of
  ConstantDataArray  ty _ -> case ty of
                               TyArray  _ ety -> if isSmpTy ety
                                                 then (liftTy ety, tye)
                                                 else error "cdsconstTyInf: ConstantDataArray does not have TyArray with a simple type"
  ConstantDataVector ty _ -> case ty of
                               TyVector _ ety -> if isSmpTy ety
                                                 then (liftTy ety, tye)
                                                 else error "cdsconstTyInf: ConstantDataVector does not have TyArray with a simple type"

-- Global Variable Constant Type Inference
gvTyInf :: TyAnnEnv -> GlobalValue -> (TyAnn, TyAnnEnv)
gvTyInf tye v = case v of
  FunctionValue  n ty -> gvTyInfA tye n ty
  GlobalAlias    n ty -> gvTyInfA tye n ty
  GlobalVariable n ty -> gvTyInfA tye n ty

gvTyInfA tye n ty = case M.lookup n tye of
                      Nothing  -> error "gvTyInf"
                      Just tyr -> (unify (liftTy ty) tyr, tye) 

-- Constant Expression Type Inference
econstTyInf :: TyAnnEnv -> ConstantExpr -> (TyAnn, TyAnnEnv)
econstTyInf = undefined