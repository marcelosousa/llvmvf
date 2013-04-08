-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Function
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Memory.Type.Function (typeFunction,typeCheckFunction) where

import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.Type.Util
import Analysis.Memory.Type.Instruction
import Language.LLVMIR
import qualified Data.Map as M
import Debug.Trace (trace)
import Data.Maybe

-- Type Check Function
typeCheckFunction :: TyEnv -> Function -> TyEnv
typeCheckFunction tye (FunctionDef  n l rty pms bbs) = let (tysig, tye') = typeSignature tye pms rty
                                                           ntye = insert n tysig tye'
                                                       in typeCheckBasicBlock ntye bbs (head bbs) -- assuming that head bbs is the entry block 
typeCheckFunction tye (FunctionDecl n l rty pms) = tye

typeSignature :: TyEnv -> Parameters -> Type -> (Type, TyEnv)
typeSignature tye ps rty = let (tps, ntye) = typeCheckParameters tye ps
                               fty = TyPointer $ TyFunction tps rty
                           in (fty, ntye)

typeCheckParameters :: TyEnv -> Parameters -> ([Type], TyEnv)
typeCheckParameters tye [] = ([], tye)
typeCheckParameters tye (x:xs) = let (tx,tye') = typeCheckParameter tye x
                                     (txs, nty) = typeCheckParameters tye' xs
                                 in (tx:txs, nty)

typeCheckParameter :: TyEnv -> Parameter -> (Type, TyEnv)
typeCheckParameter tye (Parameter i ty) = (ty, insert i ty tye)

typeCheckBasicBlock :: TyEnv -> BasicBlocks -> BasicBlock -> TyEnv
typeCheckBasicBlock tye bbs (BasicBlock l instr) = 
  let (tye', rty) = typeCheckInstructions tye instr 
  in case M.lookup l tye of
    Nothing -> case rty of
        TyJumpTo ids -> let bbsj = map (fromMaybe (error "typeCheckBasicBlock: cant find basic block") . findBasicBlock bbs) ids 
                        in typeCheckBasicBlocks tye' bbs bbsj  
        ty -> tye'
    Just ty -> tye 


findBasicBlock :: BasicBlocks -> Identifier -> Maybe BasicBlock
findBasicBlock [] l = Nothing
findBasicBlock (bb@(BasicBlock l _):bbs) i | i == l = Just bb
                                           | otherwise = findBasicBlock bbs i

typeCheckBasicBlocks :: TyEnv -> BasicBlocks -> BasicBlocks -> TyEnv
typeCheckBasicBlocks tye bbs [bb] = typeCheckBasicBlock tye bbs bb
typeCheckBasicBlocks tye bbs [bbt,bbf] = let tybbt = typeCheckBasicBlock tye bbs bbt 
                                             tybbf = typeCheckBasicBlock tye bbs bbf
                                         in M.union tybbt tybbf -- probably problematic
typeCheckBasicBlocks tye bbs x = error $ "typeCheckBasicBlocks: " ++ show x


-- typeCheckInstructions
typeCheckInstructions :: TyEnv -> Instructions -> (TyEnv, Type)
typeCheckInstructions tye []  = error "typeCheckInstructions: emtpy list"
typeCheckInstructions tye [i] = typeCheckInstruction tye i
typeCheckInstructions tye (x:xs) = 
  let (tye', v) = typeCheckInstruction tye x
  in typeCheckInstructions tye' xs

-- ==============================================================
-- Function TyAnn Inference
typeFunction :: TyAnnEnv -> Function -> (TyAnn, TyAnnEnv)
-- Incomplete: Need to check if the return type is compatible with the actual return type from the basic block
typeFunction tye (FunctionDef  n l rty pms bbs) = let (ptys, tyex) = gLstTyInf tye typeParameter pms -- Type parameters
                                                      rtyr = liftTy rty                              -- Lift return type
                                                      (bbtyr, tyenv) = bbsTyInf tyex bbs
                                                      tyr = T.TyDer $ T.TyFun ptys bbtyr -- TODO 
                                                      tyey = M.insert n tyr tyenv
                                                  in (tyr, M.insert n tyr tyex)
typeFunction tye (FunctionDecl n l rty pms)     = let (ptys, tyex) = gLstTyInf tye typeParameter pms
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