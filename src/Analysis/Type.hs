-------------------------------------------------------------------------------
-- Module    :  Analysis.Type
-- Copyright :  (c) 2013 Marcelo Sousa
-- Main module of Type based analysis for LLVM IR
-- 1) Standard type system based on the documentation of LLVM IR
--    with a type checker.
-- 2) Refined type for separation IO/Regular memory and 
--    identification of use after free. This is related to alias analysis.
-------------------------------------------------------------------------------
module Analysis.Type where

import Analysis.Type.Util (TyEnv)
import Analysis.Type.Standard.Module (typeCheckModule, STyRes)
import Analysis.Type.Memory.Context (RTyRes)
import Analysis.Type.Memory.Module (tyanModule)
import Language.LLVMIR
import qualified Data.Map as M

typeCheck :: Module -> STyRes 
typeCheck = typeCheckModule
-- 
typeAnalysis :: Module -> RTyRes
typeAnalysis = tyanModule