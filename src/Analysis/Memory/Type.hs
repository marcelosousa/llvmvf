-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Analysis (Inference)
-------------------------------------------------------------------------------

module Analysis.Memory.Type where

import Analysis.Memory.Type.Util (TyEnv)
import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import Analysis.Memory.Type.Module (typeModule, typeCheckModule)
import Language.LLVMIR
import qualified Data.Map as M

-- Module TyAnn Inference
typeAnalysis :: Module -> ([TyAnn], M.Map String (TyAnn, TyAnnEnv))
typeAnalysis = typeModule

typeCheck :: Module -> (TyEnv, M.Map String TyEnv)
typeCheck = typeCheckModule
