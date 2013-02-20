-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Value
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Memory.Type.Value where

import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.Type.Util
import Analysis.Memory.Type.Constant (typeConstant)
import Language.LLVMIR
import qualified Data.Map as M

-- Value TyAnn Inference
vTyInf :: TyAnnEnv -> Value -> (TyAnn, TyAnnEnv)
vTyInf tyenv val = case val of
   Id v ty -> let vty = liftTy ty
              in case M.lookup v tyenv of
                   Nothing  -> error $ "vtyinf"  -- (vty, M.insert v vty tyenv)
                   Just tya -> let nty = unify vty tya
                               in (nty, M.adjust (const nty) v tyenv)
   Constant c -> typeConstant tyenv c