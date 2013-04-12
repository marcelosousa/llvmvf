-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Util
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Util where

import Language.LLVMIR
import qualified Data.Map as M

type TyEnv = M.Map Identifier Type
type NamedTyEnv = M.Map Id Type

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
notAggFstClass ty = (not $ isAgg ty) && isFstClass ty

insert :: (Ord k, Show k, Show a) => k -> a -> M.Map k a -> M.Map k a
insert k v m = case M.lookup k m of
	                Nothing -> M.insert k v m
	                Just _  -> error $ "insert: " ++ show k ++ " " ++ show v ++ " " ++ show m

typeValueGen :: (Ord k, Show k, Show a) => M.Map k a -> k -> a -> (a -> a -> Bool) -> String -> a
typeValueGen tye v ty op s = case M.lookup v tye of
                              Nothing -> ty -- trace (s ++ ": " ++ show v ++ " is not in the context: " ++ show tye) $ ty
                              Just t  -> if t `op` ty
                                         then ty
                                         else error $ s ++ ": Given " ++ show ty ++ ". Expected " ++ show t

sizeof :: Type -> Int
sizeof (TyInt        p         ) = p  
sizeof (TyFloatPoint TyHalf    ) = 16
sizeof (TyFloatPoint TyFloat   ) = 32
sizeof (TyFloatPoint TyDouble  ) = 64
sizeof (TyFloatPoint TyFP128   ) = 128
sizeof (TyFloatPoint Tyx86FP80 ) = 80
sizeof (TyFloatPoint TyPPCFP128) = 128
sizeof (TyPointer    ty        ) = 8 --sizeof ty 
sizeof (TyVector     numEl ty  ) = numEl * sizeof ty
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

(<=>) :: NamedTyEnv -> Type -> Type -> Bool
(<=>) nmdtye TyVoid      TyVoid      = True
(<=>) nmdtye Tyx86MMX    Tyx86MMX    = True
(<=>) nmdtye TyLabel     TyLabel     = True
(<=>) nmdtye TyMetadata  TyMetadata  = True
(<=>) nmdtye TyOpaque    TyOpaque    = True
(<=>) nmdtye TyUndefined TyUndefined = True
(<=>) nmdtye (TyInt p)         (TyInt n)         = p == n
(<=>) nmdtye (TyFloatPoint p)  (TyFloatPoint n)  = p == n
(<=>) nmdtye (TyPointer p)     (TyPointer n)     = (<=>) nmdtye p n
(<=>) nmdtye (TyVector n r)    (TyVector m s)    = n == m && (<=>) nmdtye r s
(<=>) nmdtye (TyArray  n r)    (TyArray  m s)    = n == m && (<=>) nmdtye r s
(<=>) nmdtye (TyStruct nr n r) (TyStruct ns m s) = eqStruct nmdtye (nr,n,r) (ns,m,s)
(<=>) nmdtye x y = False

eqStruct :: NamedTyEnv -> (String,Int,[Type]) -> (String,Int,[Type]) -> Bool
eqStruct nmdtye ("",n,r) ("",m,s) = n == m && (and $ map (uncurry ((<=>) nmdtye)) $ zip r s)
eqStruct nmdtye ("",n,r) (ns,m,s) = 
	case M.lookup ns nmdtye of
		Nothing -> n == m && (and $ map (uncurry ((<=>) nmdtye)) $ zip r s)
		Just (TyStruct ns' m' s') -> n==m && (and $ map (uncurry ((<=>) nmdtye)) $ zip r s')
		Just _ -> False
eqStruct nmdtye (nr,n,r) ("",m,s) = 
	case M.lookup nr nmdtye of
		Nothing -> n == m && (and $ map (uncurry ((<=>) nmdtye)) $ zip r s)
		Just (TyStruct nr' n' r') -> eqStruct nmdtye (nr',n',r') ("",m,s)
		Just _ -> False
eqStruct nmdtye (nr,n,r) (ns,m,s) = 
	case M.lookup nr nmdtye of
		Nothing -> case M.lookup ns nmdtye of
			Nothing -> n == m && (and $ map (uncurry ((<=>) nmdtye)) $ zip r s)
			Just (TyStruct ns' m' s') -> eqStruct nmdtye (nr,n,r) (ns',m',s')
			Just _ -> False
		Just (TyStruct nr' n' r') -> case M.lookup ns nmdtye of
			Nothing -> eqStruct nmdtye (nr',n',r') (ns,m,s)
			Just (TyStruct ns' m' s') -> n' == m' && nr == ns
			Just _ -> False 
		Just _ -> False