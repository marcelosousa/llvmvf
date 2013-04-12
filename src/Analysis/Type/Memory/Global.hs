-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Global
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Rules for Globals
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Global (tyanGlobal) where

import qualified Data.Map as M

import Language.LLVMIR

import Analysis.Type.Util
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.Context
import Analysis.Type.Memory.Constant (typeConstant)
--import Analysis.Type.Memory.TyAnn (TyAnn, TyAnnEnv)
--import qualified Analysis.Type.Memory.TyAnn as T

import Debug.Trace (trace)

-- Incomplete
tyanGlobal :: NamedTyEnv -> TyAnnEnv -> Global -> TyAnnEnv
tyanGlobal nmdtye tye (GlobalVar i l False isUAddr ty Nothing  align) = insert i (liftTy ty) tye
tyanGlobal nmdtye tye (GlobalVar i l True  isUAddr ty (Just c) align) = undefined
--  let t = typeConstant nmdtye tye c
--      tyt = liftTy ty
--  in if (<=>) nmdtye (TyPointer t TyAny) ty
--     then insert i ty tye
--     else error $ "tyanGlobal(1): " ++ show i ++ "\n" ++ show t ++ "\n" ++ show ty
tyanGlobal nmdtye tye gv = error $ "tyanGlobal(2): " ++ show gv
