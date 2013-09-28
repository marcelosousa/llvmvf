-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Util
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Util where

import qualified Data.Map as M

import Language.LLVMIR
import Language.LLVMIR.Util

import Analysis.Type.Util
import Analysis.Type.Memory.TyAnn (TyAnn,TyAnnot)
import Analysis.Type.Memory.Context
import qualified Analysis.Type.Memory.TyAnn as T

type TyLIdPair = (TyAnn, Identifiers)

-- Lift a LLVM IR Type to the most generic Type Annotation
--liftTy :: Type -> TyAnn
--liftTy ty = liftTyGen ty T.TyAny

liftTy :: Type -> TyAnn
liftTy ty = liftTyGen ty T.AnyAddr

liftTyGen :: Type -> TyAnnot -> TyAnn
liftTyGen TyUndefined         a = T.TyUndef
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
--liftTyGen (TyJumpTo i)        a = T.TyJumpTo i


erase :: TyAnn -> Type
erase T.TyUndef                             = TyUndefined
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
erase x = error $ "erase TODO" -- ++ show x 

eraseEnv :: TyAnnEnv -> TyEnv
eraseEnv = M.map erase

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
