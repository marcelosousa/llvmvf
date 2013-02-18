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

-- Module TyAnn Inference
modtyinf :: Module -> ([TyAnn], [TyAnn])
modtyinf (Module i l t gvs fns nmdtys) = undefined

-- Global TyAnn Inference
gvarTyInf :: TyAnnEnv -> Global -> (TyAnn, TyAnnEnv)
gvarTyInf tyenv (GlobalVar i l isConst isUAddr ty iconst align) = 
  let ta = liftTy ty
  in case iconst of
      Nothing -> (ta, tyenv)
      Just c  -> let (t,te) = constTyInf tyenv c
                 in  (castTy t ta, te) 
 
-- Function TyAnn Inference
fntyinf :: TyAnnEnv -> Function -> (TyAnn, TyAnnEnv)
fntyinf = undefined

-- Basic Block TyAnn Inference
bbtyinf :: TyAnnEnv -> BasicBlock -> (TyAnn, TyAnnEnv)
bbtyinf tyenv (BasicBlock l instr) = undefined

ityinf :: TyAnnEnv -> Instruction -> (TyAnn, TyAnnEnv)
ityinf tyenv (Ret _ VoidRet)      = (T.TyPri T.TyVoid, tyenv)
ityinf tyenv (Ret _ (ValueRet v)) = vtyinf tyenv v
ityinf tyenv (Unreachable _)      = (T.TyBot, tyenv) -- Unreachable has no defined semantics 
ityinf tyenv (Add _ i ty op1 op2) = undefined
-- Conversion Operations
ityinf tyenv (SExt    _ i v ty)   = convTyInf tyenv i v ty
ityinf tyenv (BitCast _ i v ty)   = convTyInf tyenv i v ty
-- Memory Operations
-- The pointer of a load must a first class type.
ityinf tyenv (Load _ i v a)       = let (vty, tye) = vtyinf tyenv v
                                    in (vty, M.insert i vty tye)


-- Value TyAnn Inference
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
-- Constant TyAnn Inference
constTyInf :: TyAnnEnv -> Constant -> (TyAnn, TyAnnEnv)
constTyInf = undefined

liftTy :: Type -> TyAnn
liftTy = undefined

unify :: TyAnn -> TyAnn -> TyAnn
unify = undefined

castTy :: TyAnn -> TyAnn -> TyAnn
castTy = undefined
