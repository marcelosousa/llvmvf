-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Function
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Function (tyanFunction,tyanFnSig) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

import Language.LLVMIR

import Analysis.Type.Util
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.Context
import Analysis.Type.Memory.Instruction
import Analysis.Type.Memory.TyAnn (TyAnn)
import qualified Analysis.Type.Memory.TyAnn as T

import Debug.Trace (trace)

tyanFnSig :: TyAnnEnv -> Function -> TyAnnEnv
tyanFnSig tye (FunctionDef  n l rty iv pms bbs) = 
    let (tysig, _) = tyanSignature tye n pms (liftTy rty) iv
    in insert n tysig tye 
tyanFnSig tye (FunctionDecl n l rty iv pms) = -- trace ("inserting " ++ show n) $ 
    let (tysig, _) = tyanSignature tye n pms (liftTy rty) iv
    in insert n tysig tye 

tyanSignature :: TyAnnEnv -> Identifier -> Parameters -> TyAnn -> Bool -> (TyAnn, TyAnnEnv)
tyanSignature tye (Global n) ps rty iv = 
    case n of 
      "vmalloc" -> (T.TyDer (T.TyPtr (T.TyDer (T.TyFun [T.TyPri $ T.TyInt 64] (ptrtoMem T.TyRegAddr) False)) T.TyRegAddr), tye)
      "vfree"   -> (T.TyDer (T.TyPtr (T.TyDer (T.TyFun [ptrtoMem T.TyRegAddr] (T.TyPri T.TyVoid) False)) T.TyRegAddr), tye)
      "malloc"  -> (T.TyDer (T.TyPtr (T.TyDer (T.TyFun [T.TyPri $ T.TyInt 64] (ptrtoMem T.TyRegAddr) False)) T.TyRegAddr), tye)
      "free"    -> (T.TyDer (T.TyPtr (T.TyDer (T.TyFun [ptrtoMem T.TyRegAddr] (T.TyPri T.TyVoid) False)) T.TyRegAddr), tye)
      "ioremap" -> (T.TyDer (T.TyPtr (T.TyDer (T.TyFun [T.TyPri $ T.TyInt 64,T.TyPri $ T.TyInt 64] (ptrtoMem T.TyIOAddr) False)) T.TyRegAddr), tye)
      "iounmap" -> (T.TyDer (T.TyPtr (T.TyDer (T.TyFun [ptrtoMem T.TyIOAddr] (T.TyPri T.TyVoid) False)) T.TyRegAddr), tye)
      _ -> let (tps, ntye) = tyanCheckParameters tye ps
               fnty = T.TyDer $ T.TyFun tps rty iv
               fty = T.TyDer $ T.TyPtr fnty T.TyRegAddr
           in (fty, ntye)
tyanSignature tye (Local n) ps rty iv = error $ "tyanSignature: Local Identifier " ++ n

tyanCheckParameters :: TyAnnEnv -> Parameters -> ([TyAnn], TyAnnEnv)
tyanCheckParameters tye [] = ([], tye)
tyanCheckParameters tye (x:xs) = let (tx,tye')  = tyanCheckParameter tye x
                                     (txs, nty) = tyanCheckParameters tye' xs
                                 in (tx:txs, nty)

tyanCheckParameter :: TyAnnEnv -> Parameter -> (TyAnn, TyAnnEnv)
tyanCheckParameter tye (Parameter i ty) = let tya = liftTy ty
                                          in (tya, insert i tya tye)

-- Analyse a function
tyanFunction :: NamedTypes -> Context -> Function -> Context
tyanFunction nmdtye (c,tye) (FunctionDef  n l rty iv pms bbs) = 
    let (tysig, ntye) = tyanSignature tye n pms (liftTy rty) iv
    in tyanCheckBasicBlock nmdtye (c,ntye) bbs (head bbs) -- assuming that head bbs is the entry block 
tyanFunction nmdtye c (FunctionDecl n l rty iv pms) = c

tyanCheckBasicBlock :: NamedTypes -> Context -> BasicBlocks -> BasicBlock -> Context
tyanCheckBasicBlock nmdtye (c,tye) bbs (BasicBlock l phis instr tmn) = trace ("typeCheckBasicBlock " ++ show l) $
  let ((c', tye'),rty) = tyanCheckInstructions nmdtye (c,tye) instr 
  in case M.lookup l tye of
    Nothing -> case rty of
        T.TyJumpTo ids -> let bbsj = map (fromMaybe (error "typeCheckBasicBlock: cant find basic block") . findBasicBlock bbs) ids 
                          in trace ("TyJumpTo : " ++ show ids) $ tyanCheckBasicBlocks nmdtye (c',(insert l rty tye')) bbs bbsj  
        ty -> (c',tye')
    Just ty -> (c,tye) 

tyanCheckBasicBlocks :: NamedTypes -> Context -> BasicBlocks -> BasicBlocks -> Context
tyanCheckBasicBlocks nmdtye c bbs [bb] = tyanCheckBasicBlock nmdtye c bbs bb
tyanCheckBasicBlocks nmdtye c bbs [bbt,bbf] = let (ct,tybbt) = tyanCheckBasicBlock nmdtye c bbs bbt 
                                                  (cf,tybbf) = tyanCheckBasicBlock nmdtye c bbs bbf
                                              in (S.union ct cf, M.union tybbt tybbf) -- probably problematic
tyanCheckBasicBlocks nmdtye c bbs x = error $ "tyanCheckBasicBlocks: " ++ show x


-- typeCheckInstructions
tyanCheckInstructions :: NamedTypes -> Context -> Instructions -> (Context, TyAnn)
tyanCheckInstructions nmdtye c []  = error "typeCheckInstructions: emtpy list"
tyanCheckInstructions nmdtye c [i] = tyanCheckInstruction nmdtye c i
tyanCheckInstructions nmdtye c (x:xs) = 
  let (c',v) = tyanCheckInstruction nmdtye c x
  in tyanCheckInstructions nmdtye c' xs
