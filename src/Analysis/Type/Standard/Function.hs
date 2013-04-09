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
typeFunction tye (FunctionDef  n l rty pms bbs) = let (tysig, _) = typeSignature tye pms rty
                                                  in insert n tysig tye 
typeFunction tye (FunctionDecl n l rty pms) = let (tysig, _) = typeSignature tye pms rty
                                              in insert n tysig tye 

-- Type Check Function
typeCheckFunction :: TyEnv -> Function -> TyEnv
typeCheckFunction tye (FunctionDef  n l rty pms bbs) = let (tysig, ntye) = typeSignature tye pms rty
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
