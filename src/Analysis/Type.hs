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

import Language.LLVMIR hiding (Id)

import Analysis.Type.Util (TyEnv)
import Analysis.Type.Standard.Module (typeCheckModule, STyRes)
--import Analysis.Type.Memory.Context (RTyRes)
--import Analysis.Type.Memory.Module (tyanModule)
import Analysis.Type.Inference.Module (typeAnnInference,typeConstraints,typeAnnInferenceIP,typeInfModules,typeAnnInferenceGlobals)
import Analysis.Type.Inference.Base
import Analysis.Type.Inference.Solver
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
	let γ = M.assocs $ typeAnnInference mdl 
	forM_ γ (uncurry printTyInfFn)

typeInfInter ∷ Module → IO ()
typeInfInter mdl = do 
	let γ = typeAnnInferenceIP mdl
	forM_ (M.assocs γ) (\(a,b) → print (pretty a,b))

typeInfGlobals ∷ Module → IO ()
typeInfGlobals mdl = do 
	let γ = M.assocs $ typeAnnInferenceGlobals mdl
	forM_ γ (uncurry printTyInfFn)

printTyInfFn ∷ Id → Γ → IO ()
printTyInfFn fn γ = do
	putStrLn "----------------------"
	print $ pretty fn
	putStrLn "----------------------"
	forM_ (M.assocs γ) (\(a,b) → print (pretty a,b))

typeConstraint ∷ Module → IO ()
typeConstraint mdl = do
	let (_, gs, fncs) = typeConstraints mdl
	    cs = M.assocs fncs
	printTyℂFn (Global "globals") gs
	forM_ cs (uncurry printTyℂFn)

printTyℂFn ∷ Id → S.Set Τℂ → IO ()
printTyℂFn fn c = do
	putStrLn "----------------------"
	putStrLn $ show $ pretty fn
	putStrLn "----------------------"
	forM_ (S.toList c) print

-- Type unify inter modular
typeAnalysis ∷ [Module] → IO ()
typeAnalysis mdls = do
	let γ = typeInfModules mdls
	forM_ (M.assocs γ) (\(a,b) → print (pretty a,b)) 
