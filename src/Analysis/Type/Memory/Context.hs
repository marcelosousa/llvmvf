-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Context
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Context where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.LLVMIR (Identifier)
import Language.LLVMIR.Printer
import UU.PPrint 

import Analysis.Type.Memory.TyAnn

type Name = String
type Constrs  = S.Set (Identifier, Identifier)
type TyAnnEnv = M.Map Identifier TyAnn
type Context  = (Constrs, TyAnnEnv)

data RTyRes = RTyRes Name Context (M.Map Name Context)

-- 
instance Show RTyRes where
	show (RTyRes s gs fns) = 
		 "Memory Type Analysis\n"
	  ++ "Module " ++ s ++ "\n"
	  ++ del 
	  ++ "Global Variables\n"
	  ++ prettyContext gs ++ "\n"
	  ++ del 
	  ++ foldr prettyFn del (M.toList fns)

del :: String 
del = "========================\n"

prettyFn :: (String, Context) -> String -> String
prettyFn (n,ty) r = "Function " ++ n ++ "\n"
	  		   ++ prettyContext ty
	  		   ++ r 

prettyContext :: Context -> String
prettyContext (c,e) = "Constraints\n"
                  ++ prettyConstrs c
                  ++ prettyTyAnnEnv e

prettyConstrs :: Constrs -> String
prettyConstrs = foldr prettyC "" . S.toList
	where prettyC (x,y) c = (show $ pretty x) ++ "<~" ++ (show $ pretty y) ++ "\n" ++ c
	
prettyTyAnnEnv :: TyAnnEnv -> String
prettyTyAnnEnv m = foldr prettyE "" $ M.toList m
  where prettyE (i,ty) s = (show $ pretty i) ++ "::" ++ (show ty) ++ "\n" ++ s