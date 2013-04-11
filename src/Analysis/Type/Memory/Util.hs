-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Util
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Util where

import Analysis.Type.Memory.TyAnn (TyAnn)
import qualified Analysis.Type.Memory.TyAnn as T
import Language.LLVMIR
import qualified Data.Map as M

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
liftTy (TyFunction a r iv)  = T.TyDer $ T.TyFun (map liftTy a) [liftTy r]
liftTy (TyPointer ty)    = T.TyDer $ T.TyPtr (liftTy ty) T.TyAny
liftTy (TyVector s ty)   = T.TyDer $ T.TyVec s $ liftTy ty

unify :: TyAnn -> TyAnn -> TyAnn
unify = undefined

castTy :: TyAnn -> TyAnn -> TyAnn
castTy = undefined

-- Subtyping relation 
(<:) :: TyAnn -> TyAnn -> Bool
(T.TyDer (T.TyPtr t1 T.TyAny)) <: (T.TyDer (T.TyPtr t2 k)) = True
t1 <: t2 = t1 == t2
