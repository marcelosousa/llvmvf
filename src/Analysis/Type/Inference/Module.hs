{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Module (typeAnnInference,typeConstraints) where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.LLVMIR
import Analysis.Type.Inference.Base
import Analysis.Type.Inference.Global
import Analysis.Type.Inference.Function
import Analysis.Type.Inference.Solver

import Control.Monad
import Control.Monad.State

typeAnnInference ∷ Module → [Γ] 
typeAnnInference = map (⊨) . typeConstraints

typeConstraints ∷ Module → [S.Set Τℂ]
typeConstraints mdl = evalState (τℂs mdl) εΕ

-- | Compute type constraints
-- Compute individually for functions
τℂs ∷ Module → State Ε [S.Set Τℂ]
τℂs (Module i l t gvs fns nmdtys) = do
    gvsℂs ← τList ε gvs
    mapM (τℂu gvsℂs) $ M.elems fns
    --τList gvsℂs $ M.elems fns
