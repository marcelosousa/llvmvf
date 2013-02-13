-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.TyAnnInf
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Memory.TyAnnInf where

import Analysis.Memory.TyAnn (TyAnn)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.TypingAnn
import Language.LLVMIR
import qualified Data.Map as M

-- Function TyAnn Inference
fntyinf :: TyAnnEnv -> Function -> (TyAnn, TyAnnEnv)
fntyinf = undefined

-- Basic Block TyAnn Inference
bbtyinf :: TyAnnEnv -> BasicBlock -> (TyAnn, TyAnnEnv)
bbtyinf tyenv (BasicBlock l instr) = undefined

ityinf :: TyAnnEnv -> Instruction -> (TyAnn, TyAnnEnv)
ityinf tyenv (Ret _ VoidRet)      = (T.TyVoid, tyenv)
ityinf tyenv (Ret _ (ValueRet v)) = vtyinf tyenv v
ityinf tyenv (Unreachable _)      = (T.TyUndefined, tyenv) -- Unreachable has no defined semantics 
ityinf tyenv (Add _ i ty op1 op2) = undefined
-- Conversion Operations
ityinf tyenv (SExt    _ i v ty)   = convTyInf tyenv i v ty
ityinf tyenv (BitCast _ i v ty)   = convTyInf tyenv i v ty
-- Memory Operations
-- The pointer of a load must a first class type.
ityinf tyenv (Load _ i v a)       = let (vty, tye) = vtyinf tyenv v
                                    in (vty, M.insert i vty tye)


vtyinf :: TyAnnEnv -> Value -> (TyAnn, TyAnnEnv)
vtyinf tyenv (Id v ty) = let vty = liftTy ty
                         in case M.lookup v tyenv of
                              Nothing  -> error $ "vtyinf"  -- (vty, M.insert v vty tyenv)
                              Just tya -> let nty = unify vty tya
                                          in (nty, M.adjust (const nty) v tyenv)

convTyInf :: TyAnnEnv -> Identifier -> Value -> Type -> (TyAnn, TyAnnEnv)
convTyInf tyenv i v ty = let ity = liftTy ty
                             (vty, tye) = vtyinf tyenv v
                             nty = castTy vty ity
                         in (nty, M.insert i nty tye)

liftTy :: Type -> TyAnn
liftTy = undefined

unify :: TyAnn -> TyAnn -> TyAnn
unify = undefined

castTy :: TyAnn -> TyAnn -> TyAnn
castTy = undefined
