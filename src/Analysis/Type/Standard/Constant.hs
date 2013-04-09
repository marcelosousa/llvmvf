-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Standard.Constant
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Convinced the that the typing relation for constants is:
-- Gamma |- c : tau
-------------------------------------------------------------------------------

module Analysis.Type.Standard.Constant where

import Analysis.Type.Util
import Language.LLVMIR
import qualified Data.Map as M

-- typeValue
typeValue :: TyEnv -> Value -> Type
typeValue tye (Id v ty)    = typeValueGen tye v ty "tyValue:Id"
typeValue tye (Constant c) = typeConstant tye c

typeGlobalValue :: TyEnv -> GlobalValue -> Type 
typeGlobalValue tye (FunctionValue  n ty) = typeValueGen tye n ty "typeGlobalValue:FunctionValue"
typeGlobalValue tye (GlobalAlias    n ty) = typeValueGen tye n ty "typeGlobalValue:GlobalAlias"
typeGlobalValue tye (GlobalVariable n ty) = typeValueGen tye n ty "typeGlobalValue:GlobalVariable"

typeValueGen :: TyEnv -> Identifier -> Type -> String -> Type
typeValueGen tye v ty s = case M.lookup v tye of
                            Nothing -> error $ s ++ ": " ++ show v ++ " is not in the context: " ++ show tye
                            Just t  -> if t == ty
                                       then ty
                                       else error $ s ++ ": Given " ++ show ty ++ ". Expected " ++ show t

-- Type Constant
typeConstant :: TyEnv -> Constant -> Type
typeConstant tye c = case c of
  UndefValue      -> TyUndefined
  PoisonValue     -> error "typeConstant: PoisonValue not supported"
  BlockAddr       -> error "typeConstant: BlockAddr not supported"
  SmpConst sc     -> typeSimpleConstant tye sc
  CmpConst cc     -> typeComplexConstant tye cc
  GlobalValue gv  -> typeGlobalValue tye gv 
  ConstantExpr ec -> typeExpression tye ec 

-- typeSimpleConstant
typeSimpleConstant :: TyEnv -> SimpleConstant -> Type
typeSimpleConstant tye c = case c of
  -- ConstantInt
  ConstantInt _ ty@(TyInt x) -> ty -- Here I could check if the value fits in that number of bits.
  ConstantInt _ err          -> error $ "typeSimpleConstant: ConstantInt must be of type iX. Given: " ++ show err
  -- ConstantFP
  ConstantFP fp -> typeConstantFP tye fp
  -- ConstantPointerNull
  ConstantPointerNull ty@(TyPointer t) -> ty
  ConstantPointerNull err              -> error $ "typeSimpleConstant: ConstantPointerNull must be of type Ptr. Given: " ++ show err 

-- typeConstantFP
typeConstantFP :: TyEnv -> ConstantFP -> Type
typeConstantFP tye (ConstantFPFloat  _ ty@(TyFloatPoint TyFloat))  = ty
typeConstantFP tye (ConstantFPDouble _ ty@(TyFloatPoint TyDouble)) = ty
typeConstantFP _   cfp = error $ "typeConstantFP: " ++ show cfp

-- typeComplexConstant
typeComplexConstant :: TyEnv -> ComplexConstant -> Type
typeComplexConstant tye c = case c of
  ConstantAggregateZero  ty  -> ty
  ConstantDataSequential cds -> typeConstantDataSequential tye cds
  ConstantStruct     ty vals -> case ty of
        (TyStruct _ n tys) -> let lty = map (typeValue tye) vals
                                  zlty = zip tys lty 
                                  c = all (\(a,b) -> a == b) zlty -- Implies that the order is the same.
                              in if n == length vals && n == length tys && c
                                 then ty
                                 else error $ "typeComplexConstant: ConstantStruct " ++ show vals ++ "-" ++ show tys 
        _ -> error $ "typeComplexConstant: ConstantStruct " ++ show ty  
  ConstantArray      ty vals -> case ty of
        (TyArray n ety) -> let lty = map (typeValue tye) vals
                               c = all (==ety) lty
                           in if n == length vals && c
                              then ty
                              else error $ "typeComplexConstant: ConstantArray " ++ show vals ++ "-" ++ show n  
  ConstantVector     ty vals -> case ty of
        (TyVector n ety) -> let lty = map (typeValue tye) vals
                                c = all (==ety) lty
                            in if n == length vals && c
                               then ty
                               else error $ "typeComplexConstant: TyVector " ++ show vals ++ "-" ++ show n  

-- typeConstantDataSequential
typeConstantDataSequential :: TyEnv -> ConstantDataSequential -> Type
typeConstantDataSequential tye c = case c of 
  ConstantDataArray ty@(TyArray _ ety)  _ ->  if isSmpTy ety
                                              then ty
                                              else error $ errorMsg "ConstantDataArray" "TyArray" ty
  ConstantDataVector ty@(TyVector _ ety) _ -> if isSmpTy ety
                                              then ty
                                              else error $ errorMsg "ConstantDataVector" "TyVector" ty
  c -> error $ "typeConstantDataSequential: Given " ++ show c

errorMsg s r v = "typeConstantDataSequential: " ++ s ++ " does not have " ++ r ++ " with a simple type. Given: " ++ show v

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
