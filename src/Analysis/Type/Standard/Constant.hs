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
import Language.LLVMIR.Util
import qualified Data.Map as M
import Debug.Trace (trace)

-- typeValue
typeValue :: NamedTypes -> TyEnv -> Value -> Type
typeValue nmdtye tye (Id v ty)    = typeValueGen tye v ty (==) "tyValue:Id"
typeValue nmdtye tye (Constant c) = typeConstant nmdtye tye c

-- Type Constant
typeConstant :: NamedTypes -> TyEnv -> Constant -> Type
typeConstant nmdtye tye c = case c of
  UndefValue      -> TyUndefined
  PoisonValue     -> error "typeConstant: PoisonValue not supported"
  BlockAddr       -> error "typeConstant: BlockAddr not supported"
  SmpConst sc     -> typeSimpleConstant             sc
  CmpConst cc     -> typeComplexConstant nmdtye tye cc
  GlobalValue gv  -> typeGlobalValue            tye id (==) gv  
  ConstantExpr ec -> typeExpression      nmdtye tye ec 

-- typeSimpleConstant
typeSimpleConstant :: SimpleConstant -> Type
typeSimpleConstant c = case c of
  -- ConstantInt
  ConstantInt _ ty@(TyInt x) -> ty -- Here I could check if the value fits in that number of bits.
  ConstantInt _ err          -> error $ "typeSimpleConstant: ConstantInt must be of type iX. Given: " ++ show err
  -- ConstantFP
  ConstantFP fp -> typeConstantFP fp
  -- ConstantPointerNull
  ConstantPointerNull ty@(TyPointer t) -> ty
  ConstantPointerNull err              -> error $ "typeSimpleConstant: ConstantPointerNull must be of type Ptr. Given: " ++ show err 

-- typeConstantFP
typeConstantFP :: ConstantFP -> Type
typeConstantFP (ConstantFPFloat  _ ty@(TyFloatPoint TyFloat))  = ty
typeConstantFP (ConstantFPDouble _ ty@(TyFloatPoint TyDouble)) = ty
typeConstantFP cfp = error $ "typeConstantFP: " ++ show cfp

-- typeComplexConstant
typeComplexConstant :: NamedTypes -> TyEnv -> ComplexConstant -> Type
typeComplexConstant nmdtye tye c = case c of
  ConstantAggregateZero  ty  -> ty
  ConstantDataSequential cds -> typeConstantDataSequential cds
  ConstantStruct     ty vals -> case ty of 
        (TyStruct _ n tys) -> let lty = map (typeValue nmdtye tye) vals -- TODO: Need to change this for named type
                                  zlty = zip tys lty 
                                  c = all (\(a,b) -> a == b) zlty -- Implies that the order is the same.
                              in if n == length vals && n == length tys && c
                                 then ty
                                 else error $ "typeComplexConstant: ConstantStruct " ++ show vals ++ "-" ++ show tys 
        _ -> error $ "typeComplexConstant: ConstantStruct " ++ show ty  
  ConstantArray      ty vals -> case ty of
        (TyArray n ety) -> let lty = map (typeValue nmdtye tye) vals
                               c = all (==ety) lty
                           in if n == length vals && c
                              then ty
                              else error $ "typeComplexConstant: ConstantArray " ++ show vals ++ "-" ++ show n  
  ConstantVector     ty vals -> case ty of
        (TyVector n ety) -> let lty = map (typeValue nmdtye tye) vals
                                c = all (==ety) lty
                            in if n == length vals && c
                               then ty
                               else error $ "typeComplexConstant: TyVector " ++ show vals ++ "-" ++ show n  

-- typeConstantDataSequential
typeConstantDataSequential :: ConstantDataSequential -> Type
typeConstantDataSequential c = case c of 
  ConstantDataArray ty@(TyArray _ ety)  _ ->  if isSmpTy ety
                                              then ty
                                              else error $ errorMsg "ConstantDataArray" "TyArray" ty
  ConstantDataVector ty@(TyVector _ ety) _ -> if isSmpTy ety
                                              then ty
                                              else error $ errorMsg "ConstantDataVector" "TyVector" ty
  c -> error $ "typeConstantDataSequential: Given " ++ show c

errorMsg s r v = "typeConstantDataSequential: " ++ s ++ " does not have " ++ r ++ " with a simple type. Given: " ++ show v

-- typeExpression
typeExpression :: NamedTypes -> TyEnv -> ConstantExpr -> Type
typeExpression nmdtye tye (CompareConstantExpr ce)           = typeCompareConstantExpr nmdtye tye ce
typeExpression nmdtye tye (GetElementPtrConstantExpr v idxs) = typeGetElementPtrConstantExpr nmdtye tye v idxs
typeExpression nmdtye tye e@(UnaryConstantExpr name i v ty)  = typeUnaryExpression nmdtye tye name i v ty
typeExpression nmdtye tye e = error $ "typeExpression: " ++ show e ++ " not supported."

typeGetElementPtrConstantExpr :: NamedTypes -> TyEnv -> Value -> Values -> Type
typeGetElementPtrConstantExpr nmdtye tye v idxs = --trace ("gep: " ++ show v ++ " " ++ show idxs) $
  case typeValue nmdtye tye v of
      TyPointer ty -> if and $ map (isInt . typeValue nmdtye tye) idxs
                      then if isAgg ty
                           then getTypeAgg nmdtye ty $ map getIntValue $ tail idxs
                           else error $ "typeGetElementPtrConstantExpr: " ++ show ty ++ " is not aggregate."  
                      else error $ "typeGetElementPtrConstantExpr: not all indices are integers" 
      ty -> error $ "typeGetElementPtrConstantExpr: " ++ show ty 

getTypeAgg :: NamedTypes -> Type -> [Int] -> Type
getTypeAgg nmdtye ty [] = TyPointer ty
getTypeAgg nmdtye ty (x:xs) = case ty of 
      TyArray s t -> if x < 0 || x >= s 
                     then error $ "getTypeAgg: out of bounds"
                     else getTypeAgg nmdtye t xs
      TyStruct n s t -> if x < 0 || x >= s 
                        then error $ "getTypeAgg: out of bounds"
                        else let nt = case M.lookup n nmdtye of
                                       Nothing -> t !! x
                                       Just (TyStruct _ r t') -> if r == s 
                                                                 then t' !! x
                                                                 else error $ "getTypeAgg: Should not happen"
                             in getTypeAgg nmdtye nt xs
      _  -> error $ "getTypeAgg: " ++ show ty ++ " is not aggregate. (3)"   

typeCompareConstantExpr :: NamedTypes -> TyEnv -> CompareConstantExpr -> Type
typeCompareConstantExpr nmdtye tye (ICmpExpr _ ty op1 op2) = 
      let top1 = typeValue nmdtye tye op1
          top2 = typeValue nmdtye tye op2
          (b,i) = isComparableTypeInt top1
      in if top1 == top2 && b && ((i==0 && isInt ty) || (i==1 && isVector ty)) 
         then ty
         else error "typeCompareConstantExpr: error" 
typeCompareConstantExpr nmdtye tye (FCmpExpr _ ty op1 op2) = 
      let top1 = typeValue nmdtye tye op1
          top2 = typeValue nmdtye tye op2
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

typeUnaryExpression :: NamedTypes -> TyEnv -> String -> Int -> Value -> Type -> Type
typeUnaryExpression nmdtye tye n opcode val ty =
  case opcode of
    41 -> let tyv = typeValue nmdtye tye val
          in if isPointer tyv && isInt ty 
             then trace ("typeUnaryExpression: " ++ show val) $ ty
             else error $ "PtrToInt(1): Either type is not pointer or not int: " ++ show [tyv, ty] ++ "\n" ++ show val
    43 -> let tyv = typeValue nmdtye tye val
          in if notAggFstClass tyv && notAggFstClass ty
             then if sizeOf tyv == sizeOf ty
                  then ty
                  else error $ "bitcast: types with different bit sizes:" ++ show tyv ++ "\n" ++ show ty
     else error $ "bitcast: One of the types " ++ show [tyv, ty] ++ " is aggregate or not fst class" 
    x  -> error $ "typeUnaryExpression: " ++ show n ++ " not supported."

      