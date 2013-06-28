{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Module (typeAnnInference,typeConstraints,typeAnnInferenceIP,typeInfModules,typeAnnInferenceGlobals) where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.LLVMIR hiding (Type(..),Id, NamedTypes)
import Language.LLVMIR.Util (variadicFns,isGlobalId,δModNmds)
import Analysis.Type.Inference.Base
import Analysis.Type.Inference.Global
import Analysis.Type.Inference.Function
import Analysis.Type.Inference.Solver
import Analysis.Type.Memory.TyAnn
import Analysis.Type.Inference.Initial

import Control.Monad
import Control.Monad.State
import Prelude.Unicode ((⧺))

typeAnnInference ∷ Module → M.Map Id Γ 
typeAnnInference mdl = 
	let (nmdτ, gτℂ, fnτℂ) = typeConstraints mdl
	    gγ = (⊨) nmdτ M.empty gτℂ
	    gnγ = M.map ((⊨) nmdτ gγ) fnτℂ
	in M.insert (Global "globals") gγ gnγ  

typeConstraints ∷ Module → (NamedTypes, S.Set Τℂ, M.Map Id (S.Set Τℂ))
typeConstraints mdl = evalState (τℂs mdl) $ εΕ $ variadicFns mdl

typeAnnInferenceGlobals ∷ Module → M.Map Id Γ
typeAnnInferenceGlobals mdl = 
  let γ = typeAnnInference mdl
  in M.map (M.filterWithKey (const . isGlobalId)) γ

typeAnnInferenceIP ∷ Module → Γ
typeAnnInferenceIP mdl = 
  let nmdτ = M.map (\τ → (↑^) τ TyAny) $ δModNmds mdl
      γ  = typeAnnInferenceGlobals mdl
      γ' = M.map (M.mapWithKey toℂ) γ
      gτℂ = M.fold (\g s → (S.fromList (M.elems g)) ∪ s) ε γ'
  in (⊨) nmdτ M.empty gτℂ

toℂ ∷ Id → Τα → Τℂ
toℂ n τ = ℂπ n :=: ℂτ τ

-- | Compute type constraints
-- Compute individually for functions
τℂs ∷ Module → State Ε (NamedTypes, S.Set Τℂ, M.Map Id (S.Set Τℂ))
τℂs (Module i l t gvs fns nmdtys) = do
    gvsℂs ← τList iτℂ gvs
    --lℂs ← mapM (τℂu gvsℂs) $ M.elems fns
    lℂs ← mapM τℂ $ M.elems fns
    let nτs = M.map (\τ → (↑^) τ TyAny) nmdtys
    (↣) $ (nτs, gvsℂs, M.fromList $ zip (M.keys fns) lℂs)
    --τList gvsℂs $ M.elems fns

typeInfModules ∷ [Module] → Γ
typeInfModules mdls =
  let γs = map typeAnnInferenceIP mdls
      γs' = map (M.mapWithKey toℂ) γs
      mτℂ = foldr (\g s → (S.fromList (M.elems g)) ∪ s) ε γs'
      nmdtys = map (M.map (\τ → (↑^) τ TyAny) . δModNmds) mdls
      nmdτs = foldr M.union M.empty nmdtys
  in (⊨) nmdτs M.empty mτℂ
