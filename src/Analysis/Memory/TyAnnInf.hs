-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.TyAnnInf
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Memory.TyAnnInf where

import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import Analysis.Memory.Type.Module (modTyInf)
import Language.LLVMIR
import qualified Data.Map as M

-- Module TyAnn Inference
typeAnalysis :: Module -> ([TyAnn], M.Map String (TyAnn, TyAnnEnv))
typeAnalysis = modTyInf