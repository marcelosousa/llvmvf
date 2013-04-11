-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Standard.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Standard.Module (typeCheckModule, STyRes) where

import Analysis.Type.Standard.Function (typeCheckFunction,typeFunction)
import Analysis.Type.Util
import Analysis.Type.Standard.Global   (typeCheckGlobal)
import Language.LLVMIR
import Language.LLVMIR.Printer
import UU.PPrint 
import qualified Data.Map as M

data STyRes = STyRes String TyEnv (M.Map String TyEnv)

typeCheckModule :: Module -> STyRes
typeCheckModule (Module i _ _ gvars funs nmdtys) = 
	let tye = typeCheckGlo nmdtys M.empty gvars 
    in STyRes i tye $ typeCheckFuns nmdtys tye funs

typeCheckGlo :: NamedTyEnv -> TyEnv -> Globals -> TyEnv
typeCheckGlo nmdtye tye []     = tye
typeCheckGlo nmdtye tye (x:xs) = 
	let tye' = typeCheckGlobal nmdtye tye x
    in typeCheckGlo nmdtye tye' xs 

typeCheckFuns :: NamedTyEnv -> TyEnv -> Functions -> M.Map String TyEnv
typeCheckFuns nmdtye tye funs = let ntye =  M.fold (\f r -> typeFunction r f) tye funs
                                in M.map (typeCheckFunction nmdtye ntye) funs

instance Show STyRes where
	show (STyRes s gs fns) = 
		 "Standard Type Analysis\n"
	  ++ "Module " ++ s ++ "\n"
	  ++ del 
	  ++ "Global Variables\n"
	  ++ prettyTy gs ++ "\n"
	  ++ del 
	  ++ foldr prettyFn del (M.toList fns)

del :: String 
del = "========================\n"

prettyFn :: (String, TyEnv) -> String -> String
prettyFn (n,ty) r = "Function " ++ n ++ "\n"
	  		   ++ prettyTy ty
	  		   ++ r 


prettyTy :: TyEnv -> String
prettyTy m = foldr prettyE "" $ M.toList m
  where prettyE (i,ty) s = (show $ pretty i) ++ "::" ++ (show $ pretty ty) ++ "\n" ++ s