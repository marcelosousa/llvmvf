 {-# LANGUAGE UnicodeSyntax #-}
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

import qualified Data.Map as M
import qualified Data.Set as S

import Language.LLVMIR

import Analysis.Type.Util (TyEnv)
import Analysis.Type.Standard.Module (typeCheckModule, STyRes)
import Analysis.Type.Memory.Context (RTyRes)
import Analysis.Type.Memory.Module (tyanModule)
import Analysis.Type.Inference.Module (typeAnnInference,typeConstraints)
import Analysis.Type.Inference.Base
import Analysis.Type.Inference.Solver
import Data.Set
import Control.Monad
import UU.PPrint

typeCheck :: Module -> STyRes 
typeCheck = typeCheckModule
-- 
typeAnalysis :: Module -> RTyRes
typeAnalysis = tyanModule

-- Type Annotated Inference
typeInference ∷ Module → IO () -- Γ -- Set Τℂ
typeInference mdl = forM_ (concatMap M.assocs $ typeAnnInference mdl) (\(a,b) -> print (show $ pretty a,b)) 

typeConstraint ∷ Module → IO ()
typeConstraint mdl = forM_ (concatMap S.toList $ typeConstraints mdl) print