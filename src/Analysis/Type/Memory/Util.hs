-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Util
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Util where

import qualified Data.Map as M

import Language.LLVMIR

import Analysis.Type.Util
import Analysis.Type.Memory.TyAnn (TyAnn)
import Analysis.Type.Memory.Context
import qualified Analysis.Type.Memory.TyAnn as T

type TyLIdPair = (TyAnn, Identifiers)

-- Lift a LLVM IR Type to the most generic Type Annotation
liftTy :: Type -> TyAnn
liftTy TyUndefined       = T.TyUndef
liftTy TyVoid            = T.TyPri T.TyVoid
liftTy TyLabel           = T.TyPri T.TyLabel
liftTy TyMetadata        = T.TyPri T.TyMetadata
liftTy (TyFloatPoint f)  = T.TyPri T.TyFloat
liftTy Tyx86MMX          = error "liftTy: Tyx86MMX not supported"
liftTy TyOpaque          = error "liftTy: TyOpaque"
liftTy (TyInt s)         = T.TyPri $ T.TyInt s
liftTy (TyArray s ty)    = T.TyDer $ T.TyAgg $ T.TyArr s $ liftTy ty
liftTy (TyStruct n s ty) = T.TyDer $ T.TyAgg $ T.TyStr n s $ map liftTy ty
liftTy (TyFunction a r iv)  = T.TyDer $ T.TyFun (map liftTy a) (liftTy r) iv
liftTy (TyPointer ty)    = T.TyDer $ T.TyPtr (liftTy ty) T.TyAny
liftTy (TyVector s ty)   = T.TyDer $ T.TyVec s $ liftTy ty
liftTy (TyJumpTo i)      = T.TyJumpTo i

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
erase (T.TyJumpTo i)                        = TyJumpTo i
erase x = error $ "erase " ++ show x 

eraseEnv :: TyAnnEnv -> TyEnv
eraseEnv = M.map erase

-- Subtyping relation 
(<:) :: TyAnn -> TyAnn -> Bool
(T.TyDer (T.TyPtr t1 T.TyAny)) <: (T.TyDer (T.TyPtr t2 k)) = True
t1 <: t2 = t1 == t2

isAnnAgg :: TyAnn -> Bool
isAnnAgg (T.TyDer (T.TyAgg _)) = True
isAnnAgg _ = False

isAnnInt :: TyAnn -> Bool
isAnnInt (T.TyPri (T.TyInt _)) = True
isAnnInt _ = False

isAnnFloat :: TyAnn -> Bool
isAnnFloat (T.TyPri T.TyFloat) = True
isAnnFloat _ = False

(<~=~>) :: NamedTyEnv -> TyAnn -> TyAnn -> Bool
(<~=~>) nmdtye qtya qtyb = True
{-
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
-}