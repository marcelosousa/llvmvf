-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Function
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Function (tyanFunction,tyanFnSig) where

import qualified Data.Map as M
import Data.Maybe

import Language.LLVMIR

import Analysis.Type.Util
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.Context
import Analysis.Type.Memory.TyAnn (TyAnn)
import qualified Analysis.Type.Memory.TyAnn as T
--import Analysis.Type.Memory.Instruction

import Debug.Trace (trace)

tyanFunction = undefined

tyanFnSig :: TyAnnEnv -> Function -> TyAnnEnv
tyanFnSig tye (FunctionDef  n l rty iv pms bbs) = 
    let (tysig, _) = tyanSignature tye pms (liftTy rty) iv
    in insert n tysig tye 
tyanFnSig tye (FunctionDecl n l rty iv pms) = trace ("inserting " ++ show n) $ 
    let (tysig, _) = tyanSignature tye pms (liftTy rty) iv
    in insert n tysig tye 

tyanSignature :: TyAnnEnv -> Parameters -> TyAnn -> Bool -> (TyAnn, TyAnnEnv)
tyanSignature tye ps rty iv = let (tps, ntye) = tyanCheckParameters tye ps
                                  fnty = T.TyDer $ T.TyFun tps rty iv
                                  fty = T.TyDer $ T.TyPtr fnty T.TyRegAddr
                              in (fty, ntye)

tyanCheckParameters :: TyAnnEnv -> Parameters -> ([TyAnn], TyAnnEnv)
tyanCheckParameters tye [] = ([], tye)
tyanCheckParameters tye (x:xs) = let (tx,tye')  = tyanCheckParameter tye x
                                     (txs, nty) = tyanCheckParameters tye' xs
                                 in (tx:txs, nty)

tyanCheckParameter :: TyAnnEnv -> Parameter -> (TyAnn, TyAnnEnv)
tyanCheckParameter tye (Parameter i ty) = let tya = liftTy ty
                                          in (tya, insert i tya tye)