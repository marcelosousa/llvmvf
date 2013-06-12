{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Module (typeAnnInference,typeConstraints) where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.LLVMIR hiding (Type(..))
import Analysis.Type.Inference.Base
import Analysis.Type.Inference.Global
import Analysis.Type.Inference.Function
import Analysis.Type.Inference.Solver
import Analysis.Type.Memory.TyAnn

import Control.Monad
import Control.Monad.State

typeAnnInference ∷ Module → [Γ] 
typeAnnInference = map (⊨) . typeConstraints

typeConstraints ∷ Module → [S.Set Τℂ]
typeConstraints mdl = evalState (τℂs mdl) εΕ

ioremap ∷ Τℂ
ioremap = let nℂ = ℂπ (Global "ioremap") 
              τℂr =  ℂτ $ TyDer $ TyPtr (i 8) TyIOAddr
              τℂ = ℂλ [ℂτ (i 64), ℂτ (i 64)] τℂr
          in nℂ :=: τℂ

iτℂ ∷ S.Set Τℂ
iτℂ = ioremap ∘ ε

-- | Compute type constraints
-- Compute individually for functions
τℂs ∷ Module → State Ε [S.Set Τℂ]
τℂs (Module i l t gvs fns nmdtys) = do
    gvsℂs ← τList iτℂ gvs
    mapM (τℂu gvsℂs) $ M.elems fns
    --τList gvsℂs $ M.elems fns
