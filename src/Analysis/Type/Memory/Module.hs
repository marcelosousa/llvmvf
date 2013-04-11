-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Module (tyanModule) where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.LLVMIR

import Analysis.Type.Util
import Analysis.Type.Memory.Context
import Analysis.Type.Memory.Global   (tyanGlobal)
import Analysis.Type.Memory.Function (tyanFunction, tyanFnSig)

-- Module TyAnn Inference
tyanModule :: Module -> RTyRes
tyanModule (Module i l t gvs fns nmdtys) = 
	let tye = foldr (flip (tyanGlobal nmdtys)) M.empty gvs
    in RTyRes i tye $ tyanFunctions nmdtys tye fns

tyanFunctions :: NamedTyEnv -> TyAnnEnv -> Functions -> M.Map Name Context
tyanFunctions nmdtye tye funs = let ntye =  M.fold (flip tyanFnSig) tye funs
                                in M.map (tyanFunction nmdtye (S.empty,ntye)) funs
