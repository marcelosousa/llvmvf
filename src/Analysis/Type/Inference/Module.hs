{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Module (typeAnnInference) where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.LLVMIR
import Analysis.Type.Inference.Base
import Analysis.Type.Inference.Global
import Analysis.Type.Inference.Function

import Control.Monad
import Control.Monad.State

typeAnnInference ∷ Module → S.Set Τℂ
typeAnnInference mdl = evalState (τℂs mdl) εΓ

-- | Compute type constraints
τℂs ∷ Module → State Γ (S.Set Τℂ)
τℂs (Module i l t gvs fns nmdtys) = do
    gvsℂs ← τList ε gvs
    τList gvsℂs $ M.elems fns
