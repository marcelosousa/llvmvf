-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Function
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Memory.Type.Function where

import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.Type.Util
import Analysis.Memory.Type.Instruction
import Language.LLVMIR
import qualified Data.Map as M

-- Function TyAnn Inference
fnTyInf :: TyAnnEnv -> Function -> (TyAnn, TyAnnEnv)
fnTyInf tye (FunctionDef  n l rty pms bbs) = let (ptys, tyex) = gLstTyInf tye parTyInf pms
                                                 rtyr = liftTy rty
                                                 (bbtyr, tyenv) = bbsTyInf tyex bbs
                                                 tyr = T.TyDer $ T.TyFun ptys bbtyr -- TODO 
                                                 tyey = M.insert n tyr tyenv
                                             in (tyr, M.insert n tyr tyex)
fnTyInf tye (FunctionDecl n l rty pms)     = let (ptys, tyex) = gLstTyInf tye parTyInf pms
                                                 tyr = T.TyDer $ T.TyFun ptys [liftTy rty]
                                             in (tyr, M.insert n tyr tyex)

-- Parameter TyAnn Inference
parTyInf :: TyAnnEnv -> Parameter -> (TyAnn, TyAnnEnv)
parTyInf tye (Parameter i ty) = let tyr = liftTy ty
                                in case M.lookup i tye of
                                  Nothing -> (tyr, M.insert i tyr tye)
                                  Just tya -> error "parTyInf"
                                  
bbsTyInf :: TyAnnEnv -> BasicBlocks -> ([TyAnn], TyAnnEnv)
bbsTyInf tye bbs = bbUnify $ map (bbTyInf tye) bbs

bbUnify :: [(Label, (TyAnn, TyAnnEnv))] -> ([TyAnn], TyAnnEnv)
bbUnify []     = ([], M.empty)
bbUnify (x:xs) = undefined

-- Basic Block TyAnn Inference
bbTyInf :: TyAnnEnv -> BasicBlock -> (Label, (TyAnn, TyAnnEnv))
bbTyInf tyenv (BasicBlock l instr) = (l, isTyInf tyenv instr)

-- TODO
isTyInf :: TyAnnEnv -> Instructions -> (TyAnn, TyAnnEnv)
isTyInf tyenv []     = (T.TyBot, tyenv)
isTyInf tyenv [x]    = iTyInf tyenv x
isTyInf tyenv (x:xs) = let (ta,te) = iTyInf tyenv x
                       in isTyInf te xs  