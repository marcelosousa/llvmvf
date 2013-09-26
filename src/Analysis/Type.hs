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

import Language.LLVMIR hiding (Id, NamedTypes)

import Analysis.Type.Util (TyEnv)
import Analysis.Type.Standard.Module (typeCheckModule, STyRes)
--import Analysis.Type.Memory.Context (RTyRes)
--import Analysis.Type.Memory.Module (tyanModule)
import Analysis.Type.Inference.Module (typeAnnInference,typeConstraints,typeAnnInferenceIP,typeAnnInferenceGlobals)
import Analysis.Type.Inference.Base
import Analysis.Type.Inference.Solver
import Analysis.Type.Memory.TyAnn (showType, NamedTypes)
import Data.Set
import Control.Monad
import UU.PPrint

typeCheck :: Module -> STyRes 
typeCheck = typeCheckModule
-- 
--typeAnalysis :: Module -> RTyRes
--typeAnalysis = tyanModule

-- Type Annotated Inference
typeInfIntra ∷ Module → IO ()
typeInfIntra mdl = do 
	let (nt,γ) = typeAnnInference mdl 
	forM_ (M.assocs γ) (uncurry (printTyInfFn nt))

typeInfInter ∷ Module → IO ()
typeInfInter mdl = do 
	let (nt,γ) = typeAnnInferenceIP mdl
	forM_ (M.assocs γ) (\(a,b) → print (pretty a,showType nt b))

typeInfGlobals ∷ Module → IO ()
typeInfGlobals mdl = do 
	let (nt,γ) = typeAnnInferenceGlobals mdl
	forM_ (M.assocs γ) (uncurry (printTyInfFn nt))

printTyInfFn ∷ NamedTypes → Id → Γ → IO ()
printTyInfFn nt fn γ = do
	putStrLn "----------------------"
	print $ pretty fn
	putStrLn "----------------------"
	forM_ (M.assocs γ) (\(a,b) → putStrLn (show (pretty a) ++ " ∷ " ++ showType nt b))

typeConstraint ∷ Module → IO ()
typeConstraint mdl = do
	let (nt, gs, fncs) = typeConstraints mdl
	    cs = M.assocs fncs
	printTyℂFn nt (Global "globals") gs
	forM_ cs (uncurry (printTyℂFn nt))

printTyℂFn ∷ NamedTypes → Id → S.Set Τℂ' → IO ()
printTyℂFn nt fn c = do
	putStrLn "----------------------"
	putStrLn $ show $ pretty fn
	putStrLn "----------------------"
	forM_ (S.toList c) (\co → putStrLn $ showType nt co)

-- Type unify inter modular
{-
typeAnalysis ∷ [Module] → IO ()
typeAnalysis mdls = do
	let γ = typeInfModules mdls
	forM_ (M.assocs γ) (\(a,b) → print (pretty a,b)) 
-}