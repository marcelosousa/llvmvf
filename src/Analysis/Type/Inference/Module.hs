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
import Analysis.Type.Inference.Worklist

import Control.Monad
import Control.Monad.State

typeAnnInference ∷ Module → Γ --S.Set Τℂ
typeAnnInference mdl = (⊨) $ evalState (τℂs mdl) εΕ

-- | Compute type constraints
τℂs ∷ Module → ℂState
τℂs (Module i l t gvs fns nmdtys) = do
    gvsℂs ← τList ε gvs
    τList gvsℂs $ M.elems fns
