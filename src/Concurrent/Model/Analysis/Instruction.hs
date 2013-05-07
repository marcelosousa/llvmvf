{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.Instruction
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.Instruction where

import Language.LLVMIR
import Language.LLVMIR.Util
import Concurrent.Model.Analysis.ControlFlow
import Concurrent.Model.Analysis.DataFlow
import Concurrent.Model.Analysis.Context
import Concurrent.Model.Analysis.Util
import qualified Data.Map   as M
import qualified Data.Maybe as MB

analyseInstr :: Instruction -> Context ()
analyseInstr i = do 
  e@Env{..} <- getEnv
  let l@Location{..} = ploc
  case i of
    -- Terminators
    Ret pc v -> do let l' = Location fn bb pc False
                       c  = flow pc ploc ccfg
                       el = ExitLoc l' End
                       el' = el:eloc
                       e' = e {ccfg = c, ploc = l', eloc = el'}
                   putEnv e'
                   analyseRetValue v
    Unreachable pc -> let l' = Location fn bb pc False
                          c  = flow pc ploc ccfg
                          el = ExitLoc l' End
                          el' = el:eloc
                          e' = e {ccfg = c, ploc = l', eloc = el'}
                      in putEnv e'
    Br  pc v t f -> do let ti = valueIdentifier' "" t
                           fi = valueIdentifier' "" f
                           l' = Location fn bb pc False
                           c  = flow pc ploc ccfg
                           el = (ExitLoc l' $ BBLoc ti):(ExitLoc l' $ BBLoc fi):eloc
                           e' = e {ccfg = c, ploc = l', eloc = el}
                       putEnv e'
                       analyseValue v
    UBr pc d -> let di = valueIdentifier' "" d
                    l' = Location fn bb pc False
                    c  = flow pc ploc ccfg
                    el = (ExitLoc l' $ BBLoc di):eloc
                    e' = e {ccfg = c, ploc = l', eloc = el}
                in putEnv e'
    -- Phi Instructions
    PHI pc i ty vals -> do let l' = Location fn bb pc False
                               c  = flow pc ploc ccfg
                               e' = e {ccfg = c, ploc = l'}
                               (v,t) = unzip vals
                           putEnv e'
                           mapM_ analyseValue $ v ++ t 
                           analyseType ty
    -- Standard Binary Operations
    -- Integer Operations
    Add  pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    Sub  pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    Mul  pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    UDiv pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    SDiv pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    URem pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    SRem pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    -- Bitwise Binary Operations
    Shl  pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    LShr pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    AShr pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    And  pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    Or   pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    Xor  pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2 
    -- Float Operations
    FAdd pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2
    FSub pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2
    FMul pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2
    FDiv pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2
    FRem pc i ty op1 op2 -> analyseBinInstr pc ty op1 op2
    _ -> error $ "analyseInstr: " ++ show i ++ " not supported."

analyseRetValue :: RetInst -> Context ()
analyseRetValue VoidRet = return ()
analyseRetValue (ValueRet v) = analyseValue v

analyseValue :: Value -> Context ()
analyseValue v = return ()

analyseType :: Type -> Context ()
analyseType ty = return ()

analyseBinInstr :: PC -> Type -> Value -> Value -> Context ()
analyseBinInstr pc ty v1 v2 = do
    e@Env{..} <- getEnv
    let l@Location{..} = ploc
        l' = Location fn bb pc False
        c  = flow pc ploc ccfg
        e' = e {ccfg = c, ploc = l'}
    putEnv e'
    analyseValue v1
    analyseValue v2
    analyseType ty
	
flow :: PC -> Location -> ControlFlow -> ControlFlow
flow pc l@Location{..} cfg = 
    if ise
    then cfg
    else (Intra lpc pc):cfg
                          

{-
  -- Cast Operations
    Trunc    pc i v ty -> typeCastOp nmdtye tye i v ty isInt (>)    -- Truncate integers
    ZExt     pc i v ty -> typeCastOp nmdtye tye i v ty isInt (<)    -- Zero extend integers
    SExt     pc i v ty -> typeCastOp nmdtye tye i v ty isInt (<)    -- Sign extend integers
    FPTrunc  pc i v ty -> typeCastOp nmdtye tye i v ty isFloat (>)  -- Truncate floating point
    FPExt    pc i v ty -> typeCastOp nmdtye tye i v ty isFloat (<=) -- Extend floating point
    FPToUI   pc i v ty -> fptoint    nmdtye tye i v ty              -- floating point -> UInt
    FPToSI   pc i v ty -> fptoint    nmdtye tye i v ty              -- floating point -> SInt
    UIToFP   pc i v ty -> inttofp    nmdtye tye i v ty              -- UInt -> floating point
    SIToFP   pc i v ty -> inttofp    nmdtye tye i v ty              -- SInt -> floating point
    PtrToInt pc i v ty ->                                           -- Pointer -> Integer
        let tyv = typeUnaryExpression nmdtye tye "PtrToInt" 41 v ty
        in (insert i tyv tye, tyv)
    IntToPtr pc i v ty ->                                         -- Integer -> Pointer
        let tyv = typeValue nmdtye tye v
        in if isInt tyv && isPointer ty 
           then (insert i ty tye, ty)
           else error $ "IntToPtr: Either type is not pointer or not int: " ++ show [tyv, ty] 
    BitCast  pc i v ty ->                                       -- Type cast
        let tyv = typeUnaryExpression nmdtye tye "BitCast" 43 v ty
        in (insert i tyv tye, tyv)                
  -- Other Operations
     ICmp pc i cond ty op1 op2 -> typeCheckCmp TyClassInt   tye i ty (typeValue nmdtye tye op1) (typeValue nmdtye tye op2)
     FCmp pc i cond ty op1 op2 -> typeCheckCmp TyClassFloat tye i ty (typeValue nmdtye tye op1) (typeValue nmdtye tye op2)
  -- Memory Operations
     Alloca pc i ty       align   -> (insert i (TyPointer ty) tye, ty)-- Alloca should receive a size integer too  
     Store  pc   ty v1 v2 align   -> 
       case typeValue nmdtye tye v2 of
           TyPointer ty -> 
             let tyv2 = typeValue nmdtye tye v1
             in if ty == tyv2 && isFstClass ty
                then (tye, TyVoid)
                else error $ "typeCheckInstruction.Store: " ++ show ty 
           x -> error $ "typeCheck Store: " ++ show x
    Load   pc i    v     align   -> 
      case typeValue nmdtye tye v of
          TyPointer ty -> if isFstClass ty
                          then (insert i ty tye, ty)
                          else error $ "typeCheckInstruction.Load: " ++ show ty 
          x -> error $ "typeCheck Load: " ++ show x
     GetElementPtr pc i ty v idxs ->
       let ety = typeGetElementPtrConstantExpr nmdtye tye v idxs
       in if ety == ty
           then (insert i ty tye, ty)
           else error $ "typeCheckInstruction.GetElementPtr: " ++ show [ety,ty]
  -- Call Operation
      Call pc i ty callee vs -> typeCheckCall nmdtye tye i ty callee vs
  -- Selection Operations
      Select pc i cv vt vf       -> error "select operation not supported."
      ExtractValue pc i v idxs   -> 
        let ty = typeValue nmdtye tye v
        in if length idxs > 0
         then if isAgg ty
              then let t = getTypeAgg nmdtye ty idxs
                   in (insert i t tye, t)
              else error $ "ExtractValue: " ++ show ty ++ " is not aggregate."  
         else error $ "ExtractValue: empty list" 
      InsertValue pc i v vi idxs -> error "InsertValue operation not supported"
  -- Atomic Operations
      Cmpxchg   pc i mptr cval nval ord -> 
          case typeValue nmdtye tye mptr of
              TyPointer ty -> let cty = typeValue nmdtye tye cval
                                  nty = typeValue nmdtye tye nval
                            in if ty == cty && cty == nty
                               then (insert i ty tye, ty)
                               else error $ "Cmpxchg: Types are not equal " ++ show [ty,cty,nty]
              x -> error $ "Cmpxchg: Type of first element is not pointer: " ++ show x
      AtomicRMW pc i mptr val op ord -> 
          case typeValue nmdtye tye mptr of
              TyPointer ty -> let vty = typeValue nmdtye tye val
                            in if ty == vty 
                               then (insert i ty tye, ty)
                               else error $ "AtomicRMW: Types are not equal " ++ show [ty,vty]
              x -> error $ "AtomicRMW: Type of first element is not pointer: " ++ show x
-}  