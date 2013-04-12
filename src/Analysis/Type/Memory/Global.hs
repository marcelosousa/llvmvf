-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Global
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Rules for Globals
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Global (tyanGlobal) where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.LLVMIR

import Analysis.Type.Util
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.Context
import Analysis.Type.Memory.Constant (tyanConstant)
--import Analysis.Type.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Type.Memory.TyAnn as T

import Debug.Trace (trace)

-- Incomplete
tyanGlobal :: NamedTyEnv -> Context -> Global -> Context
tyanGlobal nmdtye (c,tye) (GlobalVar i l False isUAddr ty Nothing  align) = (c,insert i (liftTy ty) tye)
tyanGlobal nmdtye (c,tye) (GlobalVar i l True  isUAddr ty (Just cn) align) =
  let (t,li) = tyanConstant nmdtye tye cn           -- analyse the constant
      qt  = T.TyDer $ T.TyPtr t T.TyAny             -- lift the type of the constant to a qualified pointer
      qty = liftTy ty                               -- lift the type given 
      nc  = S.union c $ S.fromList $ map ((,) i) li -- update the set of constraints
  in if (<~=~>) nmdtye qt qty                       -- if qualified types are equal
     then (nc, insert i qt tye)
     else error $ "tyanGlobal(1): " ++ show i ++ "\n" ++ show t ++ "\n" ++ show ty
tyanGlobal nmdtye con gv = error $ "tyanGlobal(2): " ++ show gv
