-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Global
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Rules for Globals
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Global (typeGlobal) where

import Analysis.Type.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Type.Memory.TyAnn as T
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.Constant (typeConstant')
import Language.LLVMIR
import qualified Data.Map as M
import Debug.Trace (trace)

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

