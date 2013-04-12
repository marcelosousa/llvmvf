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
import Analysis.Type.Memory.TyAnn (TyAnn)
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
  CmpConst cc     -> undefined -- typeComplexConstant nmdtye tye cc
  GlobalValue gv  -> (typeGlobalValue tye liftTy ((<~=~>) nmdtye) gv, [])
  ConstantExpr ec -> undefined -- typeExpression      nmdtye tye ec

