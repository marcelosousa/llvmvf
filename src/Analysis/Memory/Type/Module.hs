-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Memory.Type.Module where

import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.Type.Function
import Analysis.Memory.Type.Util
import Analysis.Memory.Type.Constant
import Language.LLVMIR
import qualified Data.Map as M

-- Module TyAnn Inference
modTyInf :: Module -> ([TyAnn], M.Map String (TyAnn, TyAnnEnv))
modTyInf (Module i l t gvs fns nmdtys) = let (vtys, tye) = gLstTyInf M.empty gvarTyInf gvs
                                         in (vtys, M.map (fnTyInf tye) fns)

-- Global TyAnn Inference
gvarTyInf :: TyAnnEnv -> Global -> (TyAnn, TyAnnEnv)
gvarTyInf tyenv (GlobalVar i l isConst isUAddr ty iconst align) = 
  let ta = liftTy ty
  in case iconst of
      Nothing -> (ta, tyenv)
      Just c  -> let (t,te) = constTyInf tyenv c
                 in  (castTy t ta, te) 
