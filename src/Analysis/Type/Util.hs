-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Util
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Util where

import Language.LLVMIR
import qualified Data.Map as M

type TyEnv = M.Map Identifier Type

data TyClass = TyClassInt | TyClassFloat

isSmpTy :: Type -> Bool
isSmpTy (TyInt x)  = let n = div x 8
                     in n == 1 || n == 2 || n == 4 || n == 8
isSmpTy (TyFloatPoint TyFloat) = True
isSmpTy (TyFloatPoint TyDouble) = True
isSmpTy _ = False 

isInt :: Type -> Bool
isInt (TyInt _) = True
isInt _ = False

isVector :: Type -> Bool
isVector (TyVector s ty) = True
isVector _ = False

isAgg :: Type -> Bool
isAgg (TyArray _ _) = True
isAgg (TyStruct _ _ _) = True
isAgg _ = False

isTyOrVecOfTy :: TyClass -> Type -> Bool
isTyOrVecOfTy TyClassInt (TyInt _) = True
isTyOrVecOfTy TyClassInt (TyVector _ (TyInt _)) = True
isTyOrVecOfTy TyClassFloat (TyFloatPoint _) = True
isTyOrVecOfTy TyClassFloat (TyVector _ (TyFloatPoint _)) = True
isTyOrVecOfTy _ _ = False

-- TODO: Complete this definition
isFstClass :: Type -> Bool
isFstClass x = True

insert :: (Ord k, Show k, Show a) => k -> a -> M.Map k a -> M.Map k a
insert k v m = case M.lookup k m of
	                Nothing -> M.insert k v m
	                Just _  -> error $ "insert: " ++ show k ++ " " ++ show v ++ " " ++ show m