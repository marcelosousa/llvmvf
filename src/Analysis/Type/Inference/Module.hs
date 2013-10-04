{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Module (typeAnnInference,typeConstraints,typeAnnInferenceIP,typeAnnInferenceGlobals) where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.LLVMIR hiding (Type(..),Id, NamedTypes)
import Language.LLVMIR.Util (variadicFns,isGlobalId,δModNmds,identifierName)
import Analysis.Type.Inference.Base
import Analysis.Type.Inference.Global
import Analysis.Type.Inference.Function
import Analysis.Type.Inference.Solver
import Analysis.Type.Memory.TyAnn
import Analysis.Type.Inference.Initial

import Control.Monad
import Control.Monad.State
import Prelude.Unicode ((⧺))

typeAnnInference ∷ Module → (NamedTypes, M.Map Id Γ)
typeAnnInference mdl = 
	let (nmdτ, gτℂ, fnτℂ) = typeConstraints mdl
	    --gγ = (⊨) nmdτ M.empty gτℂ
	    gnγ = M.map (\c → solve nmdτ M.empty (c ∪ gτℂ)) fnτℂ
	in (nmdτ, gnγ) --M.insert (Global "globals") gγ gnγ)

typeConstraints ∷ Module → (NamedTypes, S.Set Τℂ', M.Map Id (S.Set Τℂ'))
typeConstraints mdl = evalState (τℂs mdl) $ εΕ $ variadicFns mdl

typeAnnInferenceGlobals ∷ Module → (NamedTypes, M.Map Id Γ)
typeAnnInferenceGlobals mdl = 
  let (nmdτ, γ) = typeAnnInference mdl
  in (nmdτ, M.map (M.filterWithKey (const . isGlobalId)) γ)

typeAnnInferenceIP ∷ Module → (NamedTypes, Γ)
typeAnnInferenceIP mdl = 
  let nmdτ = M.map (\τ → (↑^) τ AnyAddr) $ δModNmds mdl
      (nmdτ', γi)  = typeAnnInference mdl
      γtmps = M.map (\e → M.keys $ M.filterWithKey (const . not . isGlobalId) e) γi
      γzip = M.intersectionWith (,) γi γtmps
      γ = M.map (\(e,tmps) → let tmps' = map identifierName tmps
                             in M.filterWithKey (const . not . (flip elem tmps') . identifierName) e) γzip
      γ' = M.map (M.mapWithKey toℂ) γ
      gτℂ = M.fold (\g s → (S.fromList (M.elems g)) ∪ s) ε γ'
  in (nmdτ', (⊨) nmdτ M.empty (liftΤℂ 0 gτℂ))


toℂ ∷ Id → Τα → Τℂ
toℂ n τ = ℂπ n :=: ℂτ τ

-- | Compute type constraints
-- Compute individually for functions
τℂs ∷ Module → State Ε (NamedTypes, S.Set Τℂ', M.Map Id (S.Set Τℂ'))
τℂs (Module i l t gvs fns nmdtys) = do
    let iτℂ' = liftΤℂ 0 iτℂ
    gvsℂs ← τList iτℂ' gvs
    --lℂs ← mapM (τℂu gvsℂs) $ M.elems fns
    lℂs ← mapM τℂ $ M.elems fns
    let nτs = M.map (\τ → (↑^) τ AnyAddr) nmdtys
    (↣) $ (nτs, gvsℂs, M.fromList $ zip (M.keys fns) lℂs)
    --τList gvsℂs $ M.elems fns

{-
typeInfModules ∷ [Module] → Γ
typeInfModules mdls =
  let γs = map typeAnnInferenceIP mdls
      γs' = map (M.mapWithKey toℂ) γs
      mτℂ = foldr (\g s → (S.fromList (M.elems g)) ∪ s) ε γs'
      nmdtys = map (M.map (\τ → (↑^) τ TyAny) . δModNmds) mdls
      nmdτs = foldr M.union M.empty nmdtys
  in (⊨) nmdτs M.empty (liftΤℂ 0 mτℂ)
-}
