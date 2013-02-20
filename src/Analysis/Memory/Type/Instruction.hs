-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Instruction
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Memory.Type.Instruction where

import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.Type.Util
import Analysis.Memory.Type.Constant
import Language.LLVMIR
import qualified Data.Map as M

iTyInf :: TyAnnEnv -> Instruction -> (TyAnn, TyAnnEnv)
iTyInf tyenv (Ret _ VoidRet)      = (T.TyPri T.TyVoid, tyenv)
iTyInf tyenv (Ret _ (ValueRet v)) = vTyInf tyenv v
iTyInf tyenv (Unreachable _)      = (T.TyBot, tyenv) -- Unreachable has no defined semantics 
iTyInf tyenv (Add _ i ty op1 op2) = undefined
-- Conversion Operations
iTyInf tyenv (SExt    _ i v ty)   = convTyInf tyenv i v ty
iTyInf tyenv (BitCast _ i v ty)   = convTyInf tyenv i v ty
-- Memory Operations
-- The pointer of a load must a first class type.
iTyInf tyenv (Load _ i v a)       = let (vty, tye) = vTyInf tyenv v
                                    in (vty, M.insert i vty tye)

-- Auxiliar Function
convTyInf :: TyAnnEnv -> Identifier -> Value -> Type -> (TyAnn, TyAnnEnv)
convTyInf tyenv i v ty = let ity = liftTy ty
                             (vty, tye) = vTyInf tyenv v
                             nty = castTy vty ity
                         in (nty, M.insert i nty tye)

-- Value TyAnn Inference
vTyInf :: TyAnnEnv -> Value -> (TyAnn, TyAnnEnv)
vTyInf tyenv val = case val of
   Id v ty -> let vty = liftTy ty
              in case M.lookup v tyenv of
                   Nothing  -> error $ "vtyinf"  -- (vty, M.insert v vty tyenv)
                   Just tya -> let nty = unify vty tya
                               in (nty, M.adjust (const nty) v tyenv)
   Constant c -> constTyInf tyenv c