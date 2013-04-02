-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Global
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Rules for Globals
-------------------------------------------------------------------------------

module Analysis.Memory.Type.Global (typeGlobal, typeCheckGlobal) where

import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.Type.Util
import Analysis.Memory.Type.Constant (typeConstant, typeConstant')
import Language.LLVMIR
import qualified Data.Map as M

-- Global TyAnn Inference
-- Incomplete
typeGlobal :: TyAnnEnv -> Global -> (TyAnn, TyAnnEnv)
typeGlobal tyenv (GlobalVar i l isConst isUAddr ty iconst align) = 
  let ta = liftTy ty
  in case iconst of
      Nothing -> (ta, M.insert i ta tyenv)
      Just c  -> let (t,te) = typeConstant' tyenv c
                 in  if ta <: (T.TyDer (T.TyPtr t T.TyAny))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
                 	 then (ta, M.insert i ta te)
                 	 else error $ "typeGlobal: Disjoint types " ++ show ta ++ " " ++ show t

typeCheckGlobal :: TyEnv -> Global -> (Bool, TyEnv)
typeCheckGlobal tye (GlobalVar i l False isUAddr ty Nothing align) = (True, insert i ty tye)
typeCheckGlobal tye (GlobalVar i l True isUAddr ty (Just c) align) = case typeConstant tye c of
                                                                          Nothing -> (False, tye)
                                                                          Just t  -> if t == ty 
                                                                                     then (True, insert i ty tye)
                                                                                     else (False, tye)
typeCheckGlobal tye gv = error $ "typeCheckGlobal: " ++ show gv
