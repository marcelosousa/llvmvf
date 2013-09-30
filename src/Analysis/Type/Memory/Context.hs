{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Context
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Context where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.LLVMIR (Identifier, Identifiers)
import Language.LLVMIR.Printer
import UU.PPrint 

import Analysis.Type.Memory.TyAnn
import Debug.Trace (trace)
type Name = String
type Constrs  = S.Set (Identifier, Identifier)
type TyAnnEnv = M.Map Identifier TyAnn
type Context  = (Constrs, TyAnnEnv)

data RTyRes = RTyRes Name Context (M.Map Identifier Context)

filterIContext :: Context -> Identifier -> Context
filterIContext (cnstrs, tyen) i = let c = S.toList cnstrs
                                      idxs = splitConstrs c i [i] c
                                      tyen' = M.filterWithKey (\k _ -> not $ elem k idxs) tyen
                                  in (cnstrs,tyen')


splitConstrs :: [(Identifier, Identifier)] -> Identifier -> Identifiers -> [(Identifier, Identifier)] -> Identifiers
splitConstrs [] i li ol = li
splitConstrs ((a,b):xs) i li ol | i == b = if a `elem` li 
	                                       then splitConstrs xs i li ol
	                                       else let l1 = splitConstrs ol a (a:li) ol
	                                                l2 = splitConstrs xs i (a:li) ol
	                                            in l1++l2	                                           
splitConstrs ((a,b):xs) i li ol | otherwise = splitConstrs xs i li ol 

-- 
instance ShowType RTyRes where
	showType γ (RTyRes s gs fns) = 
		 "Memory Type Analysis\n"
	  ++ "Module " ++ s ++ "\n"
	  ++ del 
	  ++ "Global Variables\n"
	  ++ prettyContext γ gs ++ "\n"
	  ++ del 
	  ++ foldr (prettyFn γ) del (M.toList fns)

del :: String 
del = "========================\n"

prettyFn ∷ NamedTypes → (Identifier, Context) → String → String
prettyFn γ (n,ty) r = "Function " ++ show n ++ "\n"
	  		   ++ prettyContext γ ty
	  		   ++ r 

prettyContext ∷ NamedTypes → Context → String
prettyContext γ (c,e) = "Constraints\n"
                  ++ prettyConstrs c
                  ++ "Type Environment\n"
                  ++ prettyTyAnnEnv γ e
                  ++ "\n-----------------------\n"

prettyConstrs :: Constrs -> String
prettyConstrs = foldr prettyC "" . S.toList
	where prettyC (x,y) c = (show $ pretty x) ++ "<~" ++ (show $ pretty y) ++ "\n" ++ c
	
prettyTyAnnEnv ∷ NamedTypes → TyAnnEnv → String
prettyTyAnnEnv γ m = foldr prettyE "" $ M.toList m
  where prettyE (i,ty) s = (show $ pretty i) ++ "::" ++ (showType γ ty) ++ "\n" ++ s