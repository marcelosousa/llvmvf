{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Util
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Util where

import Language.LLVMIR
import Language.LLVMIR.Util

import qualified Data.Map as M
import Prelude.Unicode ((⧺))

import Analysis.Type.TypeQual (TypeQual)
import qualified Analysis.Type.TypeQual as T


import Debug.Trace(trace)

type TyEnv = M.Map Identifier Type

data TyClass = TyClassInt | TyClassFloat
  deriving (Show, Eq, Ord)
  
isTyOrVecOfTy :: TyClass -> Type -> Bool
isTyOrVecOfTy TyClassInt (TyInt _) = True
isTyOrVecOfTy TyClassInt (TyVector _ (TyInt _)) = True
isTyOrVecOfTy TyClassFloat (TyFloatPoint _) = True
isTyOrVecOfTy TyClassFloat (TyVector _ (TyFloatPoint _)) = True
isTyOrVecOfTy _ _ = False

insert :: (Show a) => Identifier -> a -> M.Map Identifier a -> M.Map Identifier a
insert k@(Global i) v m = case M.lookup k m of
	                Nothing -> M.insert k v m
	                Just _  -> m
insert k@(Local i) v m = case M.lookup k m of
	                Nothing -> M.insert k v m
	                Just _  -> error $ "insert: " ++ show k ++ " " ++ show v ++ " " ++ show m

typeValueGen :: (Ord k, Show k, Show a) => M.Map k a -> k -> a -> (a -> a -> Bool) -> String -> a
typeValueGen tye v ty op s = case M.lookup v tye of
                              Nothing -> error $ "Type error: " ++ show v ++ " is not in the environment: " ++ show tye 
                              Just t  -> if t `op` ty
                                         then t -- trace ("typeValueGen: " ++ show v ++ ":" ++ show t) $ t
                                         else error $ s ++ ": Given " ++ show ty ++ ". Expected " ++ show t ++ " in " ++ show v

typeGlobalValue :: (Show a) => M.Map Identifier a -> (Type -> a) -> (a -> a -> Bool) -> GlobalValue -> a 
typeGlobalValue tye f op (FunctionValue  n ty) = typeValueGen tye n (f ty) op "typeGlobalValue:FunctionValue"
typeGlobalValue tye f op (GlobalAlias    n ty) = typeValueGen tye n (f ty) op "typeGlobalValue:GlobalAlias"
typeGlobalValue tye f op (GlobalVariable n ty) = typeValueGen tye n (f ty) op "typeGlobalValue:GlobalVariable"
                             
                                                                               


--type TyLIdPair = (TyAnn, Identifiers)

-- Lift a LLVM IR Type to the most generic Type Annotation
liftTy :: Type -> TyAnn
liftTy ty = liftTyGen ty T.TyAny


liftTyGen :: Type -> TyAnnot -> TyAnn
liftTyGen TyVoid              a = T.TyPri T.TyVoid
liftTyGen TyLabel             a = T.TyPri T.TyLabel
liftTyGen TyMetadata          a = T.TyPri T.TyMetadata
liftTyGen (TyFloatPoint f)    a = T.TyPri T.TyFloat
liftTyGen Tyx86MMX            a = error "liftTy: Tyx86MMX not supported"
liftTyGen TyOpaque            a = error "liftTy: TyOpaque"
liftTyGen (TyInt s)           a = T.TyPri $ T.TyInt s
liftTyGen (TyArray s ty)      a = T.TyDer $ T.TyAgg $ T.TyArr s $ liftTyGen ty a
liftTyGen (TyStruct n s ty)   a = T.TyDer $ T.TyAgg $ T.TyStr n s $ map (flip liftTyGen a) ty
liftTyGen (TyFunction as r iv) a = T.TyDer $ T.TyFun (map (flip liftTyGen a) as) (liftTyGen r a) iv
liftTyGen (TyPointer ty)      a = T.TyDer $ T.TyPtr (liftTyGen ty a) a
liftTyGen (TyVector s ty)     a = T.TyDer $ T.TyVec s $ liftTyGen ty a

erase :: TyAnn -> Type
erase (T.TyPri T.TyVoid)                    = TyVoid
erase (T.TyPri T.TyLabel)                   = TyLabel
erase (T.TyPri T.TyMetadata)                = TyMetadata
erase (T.TyPri T.TyFloat)                   = TyFloatPoint $ TyFloat
erase (T.TyPri (T.TyInt s))                 = TyInt s
erase (T.TyDer (T.TyAgg (T.TyArr s ty)))    = TyArray s $ erase ty
erase (T.TyDer (T.TyAgg (T.TyStr n s tys))) = TyStruct n s $ map erase tys
erase (T.TyDer (T.TyFun tys ty v))          = TyFunction (map erase tys) (erase ty) v
erase (T.TyDer (T.TyPtr ty _))              = TyPointer $ erase ty
erase (T.TyDer (T.TyVec s ty))              = TyVector s $ erase ty
erase x = error $ "erase " ++ show x 

instance Sizable TyAnn where
	sizeOf τ = sizeOf $ erase τ

-- Subtyping relation 
--(<:) :: TyAnn -> TyAnn -> Bool
--(T.TyDer (T.TyPtr t2 k)) <: (T.TyDer (T.TyPtr t1 T.TyAny)) = True
--t1 <: t2 = t1 == t2

isAnnAgg :: TyAnn -> Bool
isAnnAgg (T.TyDer (T.TyAgg _)) = True
isAnnAgg _ = False

isAnnInt :: TyAnn -> Bool
isAnnInt (T.TyPri (T.TyInt _)) = True
isAnnInt _ = False

isAnnFloat :: TyAnn -> Bool
isAnnFloat (T.TyPri T.TyFloat) = True
isAnnFloat _ = False