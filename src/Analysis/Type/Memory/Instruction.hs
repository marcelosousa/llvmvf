-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Instruction
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Instruction where

import Analysis.Type.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Type.Memory.TyAnn as T
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.Constant
import Language.LLVMIR
import qualified Data.Map as M
import Debug.Trace (trace)

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