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
import qualified Analysis.Type.Memory.TyAnn as T

-- type analyse value
tyanValue :: NamedTyEnv -> TyAnnEnv -> Value -> TyLIdPair
tyanValue nmdtye tye (Id v ty)    = (typeValueGen tye v (liftTy ty) ((<~=~>) nmdtye) "tyanValue:Id", [])
tyanValue nmdtye tye (Constant c) = tyanConstant nmdtye tye c

tyanConstant :: NamedTyEnv -> TyAnnEnv -> Constant -> TyLIdPair
tyanConstant = undefined

