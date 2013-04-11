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
--  in if (<=>) nmdtye (TyPointer t) ty 
--     then insert i ty tye
--     else error $ "typeCheckGlobal(1): " ++ show i ++ "\n" ++ show t ++ "\n" ++ show ty
typeCheckGlobal nmdtye tye gv = error $ "typeCheckGlobal(2): " ++ show gv

--typeGlobal :: TyAnnEnv -> Global -> (TyAnn, TyAnnEnv)
--typeGlobal tyenv (GlobalVar i l isConst isUAddr ty iconst align) = 
--  let ta = liftTy ty
--  in case iconst of
--      Nothing -> (ta, M.insert i ta tyenv)
--      Just c  -> let (t,te) = typeConstant' tyenv c
--                 in  if ta <: (T.TyDer (T.TyPtr t T.TyAny))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
--                 	 then (ta, M.insert i ta te)
--                 	 else error $ "typeGlobal: Disjoint types " ++ show ta ++ " " ++ show t

