-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Util
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Memory.Type.Util where

import Analysis.Memory.TyAnn (TyAnn)
import qualified Analysis.Memory.TyAnn as T
import Language.LLVMIR
import qualified Data.Map as M

type TyEnv = M.Map Identifier Type

data TyClass = TyClassInt | TyClassFloat

gLstTyInf tyenv f [] = ([], tyenv)
gLstTyInf tyenv f (x:xs) = let (ty,tye) = f tyenv x
                               (tys,tyef) = gLstTyInf tye f xs
                           in (ty:tys, tyef)

-- Lift a LLVM IR Type to the most generic Type Annotation
liftTy :: Type -> TyAnn
liftTy TyUndefined       = T.TyUndef
liftTy TyVoid            = T.TyPri T.TyVoid
liftTy Tyx86MMX          = error "liftTy: Tyx86MMX not supported"
liftTy TyMetadata        = error "liftTy: TyMetadata not supported"
liftTy (TyInt s)         = T.TyPri $ T.TyInt s
liftTy (TyFloatPoint f)  = T.TyPri T.TyFloat
liftTy TyOpaque          = T.TyDer $ T.TyOpa ""
liftTy (TyArray s ty)    = T.TyDer $ T.TyAgg $ T.TyArr s $ liftTy ty
liftTy (TyStruct n s ty) = T.TyDer $ T.TyAgg $ T.TyStr n s $ map liftTy ty
liftTy TyLabel           = T.TyDer $ T.TyLab [] T.TyBot
liftTy (TyFunction a r)  = T.TyDer $ T.TyFun (map liftTy a) [liftTy r]
liftTy (TyPointer ty)    = T.TyDer $ T.TyPtr (liftTy ty) T.TyAny
liftTy (TyVector s ty)   = T.TyDer $ T.TyVec s $ liftTy ty

unify :: TyAnn -> TyAnn -> TyAnn
unify = undefined

castTy :: TyAnn -> TyAnn -> TyAnn
castTy = undefined

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

-- Subtyping relation 
(<:) :: TyAnn -> TyAnn -> Bool
(T.TyDer (T.TyPtr t1 T.TyAny)) <: (T.TyDer (T.TyPtr t2 k)) = True
t1 <: t2 = t1 == t2

insert :: (Ord k, Show k, Show a) => k -> a -> M.Map k a -> M.Map k a
insert k v m = case M.lookup k m of
	                Nothing -> M.insert k v m
	                Just _  -> error $ "insert: " ++ show k ++ " " ++ show v ++ " " ++ show m