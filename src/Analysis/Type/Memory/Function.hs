-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Function
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Function (typeFunction) where

import Analysis.Type.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Type.Memory.TyAnn as T
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.Instruction
import Language.LLVMIR
import qualified Data.Map as M
import Debug.Trace (trace)
import Data.Maybe

-- ==============================================================
-- Function TyAnn Inference
typeFunction :: TyAnnEnv -> Function -> (TyAnn, TyAnnEnv)
-- Incomplete: Need to check if the return type is compatible with the actual return type from the basic block
typeFunction tye (FunctionDef  n l rty iv pms bbs) = 
    let (ptys, tyex) = gLstTyInf tye typeParameter pms -- Type parameters
        rtyr = liftTy rty                              -- Lift return type
        (bbtyr, tyenv) = bbsTyInf tyex bbs
        tyr = T.TyDer $ T.TyFun ptys bbtyr -- TODO 
        tyey = M.insert n tyr tyenv
    in (tyr, M.insert n tyr tyex)
typeFunction tye (FunctionDecl n l rty iv pms) = 
    let (ptys, tyex) = gLstTyInf tye typeParameter pms
        tyr = T.TyDer $ T.TyFun ptys [liftTy rty]
    in (tyr, M.insert n tyr tyex)

-- Parameter TyAnn Inference
typeParameter :: TyAnnEnv -> Parameter -> (TyAnn, TyAnnEnv)
typeParameter tye (Parameter i ty) = let tyr = liftTy ty
                                     in case M.lookup i tye of
                                         Nothing  -> (tyr, M.insert i tyr tye)
                                         Just tya -> error "typeParameter"

bbsTyInf :: TyAnnEnv -> BasicBlocks -> ([TyAnn], TyAnnEnv)
bbsTyInf tye bbs = bbUnify $ map (bbTyInf tye) bbs

bbUnify :: [(Identifier, (TyAnn, TyAnnEnv))] -> ([TyAnn], TyAnnEnv)
bbUnify []     = ([], M.empty)
bbUnify (x:xs) = undefined

-- Basic Block TyAnn Inference
bbTyInf :: TyAnnEnv -> BasicBlock -> (Identifier, (TyAnn, TyAnnEnv))
bbTyInf tyenv (BasicBlock l instr) = (l, isTyInf tyenv instr)

-- TODO
isTyInf :: TyAnnEnv -> Instructions -> (TyAnn, TyAnnEnv)
isTyInf tyenv []     = (T.TyBot, tyenv)
isTyInf tyenv [x]    = iTyInf tyenv x
isTyInf tyenv (x:xs) = let (ta,te) = iTyInf tyenv x
                       in isTyInf te xs