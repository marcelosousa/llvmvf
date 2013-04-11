-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Standard.Global
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Rules for Globals
-------------------------------------------------------------------------------

module Analysis.Type.Standard.Global (typeCheckGlobal) where

import Analysis.Type.Util
import Analysis.Type.Standard.Constant (typeConstant)
import Language.LLVMIR
import qualified Data.Map as M
import Debug.Trace (trace)

-- Type Check Global
typeCheckGlobal :: NamedTyEnv -> TyEnv -> Global -> TyEnv
typeCheckGlobal nmdtye tye (GlobalVar i l False isUAddr ty Nothing align) = insert i ty tye
typeCheckGlobal nmdtye tye (GlobalVar i l True isUAddr ty (Just c) align) = 
  let t = typeConstant nmdtye tye c
  in if (<=>) nmdtye (TyPointer t) ty 
     then insert i ty tye
     else error $ "typeCheckGlobal(1): " ++ show i ++ "\n" ++ show t ++ "\n" ++ show ty
typeCheckGlobal nmdtye tye gv = error $ "typeCheckGlobal(2): " ++ show gv
