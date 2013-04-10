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

isFloat :: Type -> Bool
isFloat (TyFloatPoint _) = True
isFloat _ = False

isVector :: Type -> Bool
isVector (TyVector s ty) = True
isVector _ = False

isPointer :: Type -> Bool
isPointer (TyPointer _) = True
isPointer _ = False

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

notAggFstClass :: Type -> Bool
notAggFstClass ty = isAgg ty && isFstClass ty

insert :: (Ord k, Show k, Show a) => k -> a -> M.Map k a -> M.Map k a
insert k v m = case M.lookup k m of
	                Nothing -> M.insert k v m
	                Just _  -> error $ "insert: " ++ show k ++ " " ++ show v ++ " " ++ show m


sizeof :: Type -> Int
sizeof (TyInt        p         ) = p  
sizeof (TyFloatPoint TyHalf    ) = 16
sizeof (TyFloatPoint TyFloat   ) = 32
sizeof (TyFloatPoint TyDouble  ) = 64
sizeof (TyFloatPoint TyFP128   ) = 128
sizeof (TyFloatPoint Tyx86FP80 ) = 80
sizeof (TyFloatPoint TyPPCFP128) = 128
sizeof (TyVector     numEl ty  ) = numEl * sizeof ty
sizeof (TyPointer    ty        ) = sizeof ty 
sizeof (TyArray      numEl ty  ) = numEl * sizeof ty
sizeof (TyStruct   _ numEl tys ) = sum $ map sizeof tys
sizeof (TyVoid                 ) = error "sizeof"
sizeof (Tyx86MMX               ) = error "sizeof"
sizeof (TyLabel                ) = error "sizeof"
sizeof (TyMetadata             ) = error "sizeof"
sizeof (TyOpaque               ) = error "sizeof"
sizeof (TyUndefined            ) = error "sizeof"
sizeof (TyFunction pty rty iv  ) = error "sizeof"
sizeof (TyJumpTo lb            ) = error "sizeof"
