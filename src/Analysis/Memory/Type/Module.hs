-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Memory.Type.Module (typeModule) where

import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.Type.Function (typeFunction)
import Analysis.Memory.Type.Util
import Analysis.Memory.Type.Global   (typeGlobal)
import Language.LLVMIR
import qualified Data.Map as M

-- Module TyAnn Inference
typeModule :: Module -> ([TyAnn], M.Map String (TyAnn, TyAnnEnv))
typeModule (Module i l t gvs fns nmdtys) = let (vtys, tye) = gLstTyInf M.empty typeGlobal gvs
                                           in (vtys, M.empty) -- M.map (typeFunction tye) fns)
