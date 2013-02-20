-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Constant
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Convinced the that the typing relation for constants is:
-- Gamma |- c : tau
-------------------------------------------------------------------------------

module Analysis.Memory.Type.Constant (typeConstant) where

import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.Type.Util
import Language.LLVMIR
import qualified Data.Map as M

-- Constant TyAnn Inference
typeConstant :: TyAnnEnv -> Constant -> (TyAnn, TyAnnEnv)
typeConstant tye c = case c of
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
  where isSmpTy ety = case ety of
                       TyInt 8  -> True
                       TyInt 16 -> True
                       TyInt 32 -> True
                       TyInt 64 -> True
                       TyFloatPoint TyFloat -> True
                       TyFloatPoint TyDouble -> True
                       _ -> False 

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