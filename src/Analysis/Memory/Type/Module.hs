-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Memory.Type.Module (typeModule,typeCheckModule) where

import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.Type.Function (typeFunction, typeCheckFunction)
import Analysis.Memory.Type.Util
import Analysis.Memory.Type.Global   (typeGlobal, typeCheckGlobal)
import Language.LLVMIR
import qualified Data.Map as M

-- Module TyAnn Inference
typeModule :: Module -> ([TyAnn], M.Map String (TyAnn, TyAnnEnv))
typeModule (Module i l t gvs fns nmdtys) = let (vtys, tye) = gLstTyInf M.empty typeGlobal gvs
                                           in (vtys, M.empty) -- M.map (typeFunction tye) fns)

typeCheckModule :: Module -> (TyEnv, M.Map String TyEnv)
typeCheckModule (Module _ _ _ gvars funs _) = let tye = typeCheckGlo gvars M.empty
                                              in (tye, typeCheckFuns tye funs)

typeCheckGlo :: Globals -> TyEnv -> TyEnv
typeCheckGlo []     tye = tye
typeCheckGlo (x:xs) tye = let tye' = typeCheckGlobal tye x
                          in typeCheckGlo xs tye'

typeCheckFuns :: TyEnv -> Functions -> M.Map String TyEnv
typeCheckFuns tye funs = M.map (typeCheckFunction tye) funs  