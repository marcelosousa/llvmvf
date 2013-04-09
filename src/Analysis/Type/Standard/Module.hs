-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Standard.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Standard.Module (typeCheckModule) where

import Analysis.Type.Standard.Function (typeCheckFunction)
import Analysis.Type.Util
import Analysis.Type.Standard.Global   (typeCheckGlobal)
import Language.LLVMIR
import qualified Data.Map as M

typeCheckModule :: Module -> (TyEnv, M.Map String TyEnv)
typeCheckModule (Module _ _ _ gvars funs _) = let tye = typeCheckGlo gvars M.empty
                                              in (tye, typeCheckFuns tye funs)

typeCheckGlo :: Globals -> TyEnv -> TyEnv
typeCheckGlo []     tye = tye
typeCheckGlo (x:xs) tye = let tye' = typeCheckGlobal tye x
                          in typeCheckGlo xs tye'

typeCheckFuns :: TyEnv -> Functions -> M.Map String TyEnv
typeCheckFuns tye funs = M.map (typeCheckFunction tye) funs