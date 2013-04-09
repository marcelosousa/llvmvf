-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Standard.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Standard.Module (typeCheckModule, STyRes) where

import Analysis.Type.Standard.Function (typeCheckFunction)
import Analysis.Type.Util
import Analysis.Type.Standard.Global   (typeCheckGlobal)
import Language.LLVMIR
import Language.LLVMIR.Printer
import UU.PPrint 
import qualified Data.Map as M

data STyRes = STyRes String TyEnv (M.Map String TyEnv)

typeCheckModule :: Module -> STyRes
typeCheckModule (Module i _ _ gvars funs _) = let tye = typeCheckGlo gvars M.empty
                                              in STyRes i tye $ typeCheckFuns tye funs

typeCheckGlo :: Globals -> TyEnv -> TyEnv
typeCheckGlo []     tye = tye
typeCheckGlo (x:xs) tye = let tye' = typeCheckGlobal tye x
                          in typeCheckGlo xs tye'

typeCheckFuns :: TyEnv -> Functions -> M.Map String TyEnv
typeCheckFuns tye funs = M.map (typeCheckFunction tye) funs

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