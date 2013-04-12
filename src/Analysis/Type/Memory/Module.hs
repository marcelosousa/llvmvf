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

ic :: Context
ic = (S.empty, M.empty)

-- Module TyAnn Inference
tyanModule :: Module -> RTyRes
tyanModule (Module i l t gvs fns nmdtys) = 
	let con = foldr (flip (tyanGlobal nmdtys)) ic gvs
    in RTyRes i con $ tyanFunctions nmdtys con fns

tyanFunctions :: NamedTyEnv -> Context -> Functions -> M.Map Name Context
tyanFunctions nmdtye (c,e) funs = let ntye =  M.fold (flip tyanFnSig) e funs
                                  in M.map (tyanFunction nmdtye (c,ntye)) funs
