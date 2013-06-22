-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Standard.Function
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Standard.Function (typeFunction,typeCheckFunction) where

import Analysis.Type.Util
import Analysis.Type.Standard.Instruction
import Language.LLVMIR
import qualified Data.Map as M
import Debug.Trace (trace)
import Data.Maybe

typeFunction :: TyEnv -> Function -> TyEnv
typeFunction tye (FunctionDef  n l rty iv pms bbs) = 
    let (tysig, _) = typeSignature tye pms rty iv
    in insert n tysig tye 
typeFunction tye (FunctionDecl n l rty iv pms) = trace ("inserting " ++ show n) $ 
    let (tysig, _) = typeSignature tye pms rty iv
    in insert n tysig tye 

-- Type Check Function
typeCheckFunction :: NamedTypes -> TyEnv -> Function -> TyEnv
typeCheckFunction nmdtye tye (FunctionDef  n l rty iv pms bbs) = 
    let (tysig, ntye) = typeSignature tye pms rty iv
    in typeCheckBasicBlock nmdtye ntye bbs (head bbs) -- assuming that head bbs is the entry block 
typeCheckFunction nmdtye tye (FunctionDecl n l rty iv pms) = tye

typeSignature :: TyEnv -> Parameters -> Type -> Bool -> (Type, TyEnv)
typeSignature tye ps rty iv = let (tps, ntye) = typeCheckParameters tye ps
                                  fty = TyPointer $ TyFunction tps rty iv
                              in (fty, ntye)

typeCheckParameters :: TyEnv -> Parameters -> ([Type], TyEnv)
typeCheckParameters tye [] = ([], tye)
typeCheckParameters tye (x:xs) = let (tx,tye') = typeCheckParameter tye x
                                     (txs, nty) = typeCheckParameters tye' xs
                                 in (tx:txs, nty)

typeCheckParameter :: TyEnv -> Parameter -> (Type, TyEnv)
typeCheckParameter tye (Parameter i ty) = (ty, insert i ty tye)

typeCheckBasicBlock :: NamedTypes -> TyEnv -> BasicBlocks -> BasicBlock -> TyEnv
typeCheckBasicBlock nmdtye tye bbs (BasicBlock l phis instr tmn) = -- trace ("typeCheckBasicBlock " ++ show l) $
  let (tye', rty) = typeCheckInstructions nmdtye tye instr 
  in case M.lookup l tye of
    Nothing -> case rty of
--        TyJumpTo ids -> let bbsj = map (fromMaybe (error "typeCheckBasicBlock: cant find basic block") . findBasicBlock bbs) ids 
--                        in typeCheckBasicBlocks nmdtye (insert l rty tye') bbs bbsj  
        ty -> tye'
    Just ty -> tye 

typeCheckBasicBlocks :: NamedTypes -> TyEnv -> BasicBlocks -> BasicBlocks -> TyEnv
typeCheckBasicBlocks nmdtye tye bbs [bb] = typeCheckBasicBlock nmdtye tye bbs bb
typeCheckBasicBlocks nmdtye tye bbs [bbt,bbf] = let tybbt = typeCheckBasicBlock nmdtye tye bbs bbt 
                                                    tybbf = typeCheckBasicBlock nmdtye tye bbs bbf
                                                in M.union tybbt tybbf -- probably problematic
typeCheckBasicBlocks nmdtye tye bbs x = error $ "typeCheckBasicBlocks: " ++ show x


-- typeCheckInstructions
typeCheckInstructions :: NamedTypes -> TyEnv -> Instructions -> (TyEnv, Type)
typeCheckInstructions nmdtye tye []  = error "typeCheckInstructions: emtpy list"
typeCheckInstructions nmdtye tye [i] = typeCheckInstruction nmdtye tye i
typeCheckInstructions nmdtye tye (x:xs) = 
  let (tye', v) = typeCheckInstruction nmdtye tye x
  in typeCheckInstructions nmdtye tye' xs
