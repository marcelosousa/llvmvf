-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Constant
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Constant where

import qualified Data.Map as M

import Language.LLVMIR

import Analysis.Type.Util
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.Context
import Analysis.Type.Memory.TyAnn (TyAnn, TyAnnot)
import Analysis.Type.Standard.Constant
import qualified Analysis.Type.Memory.TyAnn as T

-- type analyse value
tyanValue :: NamedTyEnv -> TyAnnEnv -> Value -> TyLIdPair
tyanValue nmdtye tye (Id v ty)    = (typeValueGen tye v (liftTy ty) ((<~=~>) nmdtye) "tyanValue:Id", [])
tyanValue nmdtye tye (Constant c) = tyanConstant nmdtye tye c

tyanConstant :: NamedTyEnv -> TyAnnEnv -> Constant -> TyLIdPair
tyanConstant nmdtye tye c = case c of
  UndefValue      -> (T.TyUndef, [])
  PoisonValue     -> error "tyanConstant: PoisonValue not supported"
  BlockAddr       -> error "tyanConstant: BlockAddr not supported"
  SmpConst sc     -> (liftTy $ typeSimpleConstant sc, [])
  CmpConst cc     -> tyanComplexConstant nmdtye tye cc
  GlobalValue gv  -> (typeGlobalValue tye liftTy ((<~=~>) nmdtye) gv, [])
  ConstantExpr ec -> tyanExpression      nmdtye tye ec

tyanComplexConstant :: NamedTyEnv -> TyAnnEnv -> ComplexConstant -> TyLIdPair
tyanComplexConstant nmdtye tye c = case c of
  ConstantAggregateZero  ty  -> (liftTy $ ty, [])
  ConstantDataSequential cds -> (liftTy $ typeConstantDataSequential cds, [])
  ConstantStruct     ty vals -> case ty of 
        TyStruct _ n tys -> let (lty, cs) = unzip $ map (tyanValue nmdtye tye) vals -- TODO: Need to change this for named type
                                zlty = zip (map liftTy tys) lty 
                                c = all (uncurry (==)) zlty -- Implies that the order is the same.
                            in if n == length vals && n == length tys && c
                               then (liftTy ty, concat cs)
                               else error $ "typeComplexConstant: ConstantStruct " ++ show vals ++ "-" ++ show tys 
        _ -> error $ "typeComplexConstant: ConstantStruct " ++ show ty  
  ConstantArray      ty vals -> case ty of
        TyArray n ety -> let (lty, cs) = unzip $ map (tyanValue nmdtye tye) vals
                             c = all (==(liftTy ety)) lty
                         in if n == length vals && c
                            then (liftTy ty, concat cs)
                            else error $ "typeComplexConstant: ConstantArray " ++ show vals ++ "-" ++ show n  
        _ -> error $ "typeComplexConstant: ConstantArray " ++ show ty  
  ConstantVector     ty vals -> case ty of
        TyVector n ety -> let (lty, cs) = unzip $ map (tyanValue nmdtye tye) vals
                              c = all (==(liftTy ety)) lty
                          in if n == length vals && c
                             then (liftTy ty, concat cs)
                             else error $ "typeComplexConstant: TyVector " ++ show vals ++ "-" ++ show n  
        _ -> error $ "typeComplexConstant: ConstantVector " ++ show ty

-- typeExpression
tyanExpression :: NamedTyEnv -> TyAnnEnv -> ConstantExpr -> TyLIdPair
tyanExpression nmdtye tye (CompareConstantExpr ce)           = tyanCompareConstantExpr       nmdtye tye ce
tyanExpression nmdtye tye (GetElementPtrConstantExpr v idxs) = tyanGetElementPtrConstantExpr nmdtye tye v idxs
tyanExpression nmdtye tye e@(UnaryConstantExpr name i v ty)  = undefined --typeUnaryExpression nmdtye tye name i v ty
tyanExpression nmdtye tye e = error $ "typeExpression: " ++ show e ++ " not supported."

tyanGetElementPtrConstantExpr :: NamedTyEnv -> TyAnnEnv -> Value -> Values -> TyLIdPair
tyanGetElementPtrConstantExpr nmdtye tye v idxs = -- trace ("gep analysis: " ++ show v ++ " " ++ show idxs) $
  let (ty,li) = tyanValue nmdtye tye v
      c = and $ map (isAnnInt . fst . tyanValue nmdtye tye) idxs
  in case ty of
      T.TyDer (T.TyPtr typ ann) -> if c
                                   then if isAnnAgg ty
                                        then (getTypeAnnAgg nmdtye typ ann $ map getIntValue $ tail idxs,[])
                                        else error $ "tyanGetElementPtrConstantExpr: " ++ show ty ++ " is not aggregate."  
                                   else error $ "tyanGetElementPtrConstantExpr: not all indices are integers" 
      ty -> error $ "tyanGetElementPtrConstantExpr: " ++ show ty 


getTypeAnnAgg :: NamedTyEnv -> TyAnn -> TyAnnot -> [Int] -> TyAnn 
getTypeAnnAgg nmdtye ty ann [] = T.TyDer $ T.TyPtr ty ann
getTypeAnnAgg nmdtye ty ann (x:xs) = case ty of 
      T.TyDer (T.TyAgg (T.TyArr s t)) -> 
         if x < 0 || x >= s 
         then error $ "getTypeAnnAgg: out of bounds"
         else getTypeAnnAgg nmdtye t ann xs
      T.TyDer (T.TyAgg (T.TyStr n s t)) -> 
         if x < 0 || x >= s 
         then error $ "getTypeAnnAgg: out of bounds"
         else let nt = case M.lookup n nmdtye of
                         Nothing -> t !! x
                         Just (TyStruct _ r t') -> 
                          if r == s 
                          then liftTy $ t' !! x
                          else error $ "getTypeAnnAgg: Should not happen"
              in getTypeAnnAgg nmdtye nt ann xs
      _  -> error $ "getTypeAnnAgg: " ++ show ty ++ " is not aggregate. (3)"   

tyanCompareConstantExpr :: NamedTyEnv -> TyAnnEnv -> CompareConstantExpr -> TyLIdPair
tyanCompareConstantExpr nmdtye tye (ICmpExpr _ ty op1 op2) = 
      let (top1,lop1) = tyanValue nmdtye tye op1
          (top2,lop2) = tyanValue nmdtye tye op2
          (b,i) = isComparableTypeAnnInt top1
      in if ((<~=~>) nmdtye top1 top2) && b && ((i==0 && isInt ty) || (i==1 && isVector ty)) 
         then (liftTy ty, lop1++lop2)
         else error "typeCompareConstantExpr: error" 
tyanCompareConstantExpr nmdtye tye (FCmpExpr _ ty op1 op2) = 
      let (top1,lop1) = tyanValue nmdtye tye op1
          (top2,lop2) = tyanValue nmdtye tye op2
          (b,i) = isComparableTypeAnnFloat top1
      in if ((<~=~>) nmdtye top1 top2) && b && ((i==0 && isInt ty) || (i==1 && isVector ty)) 
         then (liftTy ty, lop1++lop2)
         else error "typeCompareConstantExpr: error" 

isComparableTypeAnnInt :: TyAnn -> (Bool, Int)
isComparableTypeAnnInt (T.TyPri (T.TyInt _)) = (True, 0)
isComparableTypeAnnInt (T.TyDer (T.TyVec _ (T.TyPri (T.TyInt _)))) = (True,1)
isComparableTypeAnnInt (T.TyDer (T.TyPtr _ _)) = (True,0) -- Suspicious
isComparableTypeAnnInt _ = (False,0)

isComparableTypeAnnFloat :: TyAnn -> (Bool, Int)
isComparableTypeAnnFloat (T.TyPri T.TyFloat) = (True,0)
isComparableTypeAnnFloat (T.TyDer (T.TyVec _ (T.TyPri T.TyFloat))) = (True,1)
isComparableTypeAnnFloat (T.TyDer (T.TyPtr _ _)) = (True,0) -- Suspicious
isComparableTypeAnnFloat _ = (False, 0)

{-
typeUnaryExpression :: NamedTyEnv -> TyEnv -> String -> Int -> Value -> Type -> Type
typeUnaryExpression nmdtye tye n opcode val ty =
  case opcode of
    41 -> let tyv = typeValue nmdtye tye val
          in if isPointer tyv && isInt ty 
             then ty
             else error $ "PtrToInt(1): Either type is not pointer or not int: " ++ show [tyv, ty] ++ "\n" ++ show val
    43 -> let tyv = typeValue nmdtye tye val
          in if notAggFstClass tyv && notAggFstClass ty
             then if sizeof tyv == sizeof ty
                  then ty
                  else error $ "bitcast: types with different bit sizes:" ++ show tyv ++ "\n" ++ show ty
     else error $ "bitcast: One of the types " ++ show [tyv, ty] ++ " is aggregate or not fst class" 
    x  -> error $ "typeUnaryExpression: " ++ show n ++ " not supported."
-}