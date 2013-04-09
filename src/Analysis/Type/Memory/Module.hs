-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Module (typeModule) where

import Analysis.Type.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Type.Memory.TyAnn as T
import Analysis.Type.Memory.Function (typeFunction)
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.Global   (typeGlobal)
import Language.LLVMIR
import qualified Data.Map as M

-- Module TyAnn Inference
typeModule :: Module -> ([TyAnn], M.Map String (TyAnn, TyAnnEnv))
typeModule (Module i l t gvs fns nmdtys) = let (vtys, tye) = gLstTyInf M.empty typeGlobal gvs
                                           in undefined
