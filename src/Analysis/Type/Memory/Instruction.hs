-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Memory.Instruction
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Memory.Instruction where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.LLVMIR

import Analysis.Type.Util
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.Constant
import Analysis.Type.Memory.Context
import Analysis.Type.Memory.TyAnn (TyAnn)
import Analysis.Type.Standard.Instruction
import qualified Analysis.Type.Memory.TyAnn as T

import Debug.Trace (trace)


rtyanValue :: NamedTyEnv -> TyAnnEnv -> Identifier -> Value -> (TyAnn, Constrs)
rtyanValue nmdtye tye i v = let (ty, lid) = tyanValue nmdtye tye v
                            in (ty, S.fromList $ map (\j -> (i,j)) lid) 

tyanCheckInstruction :: NamedTyEnv -> Context -> Instruction -> (Context, TyAnn)
tyanCheckInstruction nmdtye c@(ctrs,tye) i = case i of
  -- Terminators
	Ret pc VoidRet      -> (c, T.TyPri $ T.TyVoid)
	Ret pc (ValueRet v) -> (c, fst $ tyanValue nmdtye tye v) 
	Unreachable pc      -> (c, T.TyUndef) -- Unreachable has no defined semantics 
	Br  pc v t f        -> if (fst $ tyanValue nmdtye tye v) == (T.TyPri $ T.TyInt 1)
		                   then (c, liftTy $ typeCheckBranchs [t,f])
		                   else error "typeCheckInstruction.Br: Condition type is not i1"
	UBr pc d            -> (c, liftTy $ typeCheckBranchs [d])
	Switch pc ty v elems -> error "tyanCheckInstruction: Switch instruction not supported."	
  -- Phi Instructions
 	PHI pc i ty vals -> let (vs,ls) = unzip vals
 	                        (tyvs, sctrs) = unzip $ map (rtyanValue nmdtye tye i) vs
 	                        tyls = map typeCheckBranch ls
 	                        tyl  = liftTy ty
 	                        p1   = all (==tyl) tyvs
 	                        nctrs = foldr S.union ctrs sctrs
 	                    in if seq tyls p1
 	                       then ((nctrs, insert i tyl tye), tyl)
 	                       else error $ "typeCheckInstruction.PHI: " ++ show ty ++ " " ++ show tyvs
  -- Standard Binary Operations
  -- Integer Operations
 	Add  pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2 
 	Sub  pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2  
 	Mul  pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2  
 	UDiv pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2  
 	SDiv pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2  
 	URem pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2  
 	SRem pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2  
  -- Bitwise Binary Operations
	Shl  pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2 
	LShr pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2 
	AShr pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2 
	And  pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2 
	Or   pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2 
	Xor  pc i ty op1 op2 -> tyanCheckBinInstr TyClassInt nmdtye c i ty op1 op2 
  -- Float Operations
 	FAdd pc i ty op1 op2 -> tyanCheckBinInstr TyClassFloat nmdtye c i ty op1 op2
 	FSub pc i ty op1 op2 -> tyanCheckBinInstr TyClassFloat nmdtye c i ty op1 op2
 	FMul pc i ty op1 op2 -> tyanCheckBinInstr TyClassFloat nmdtye c i ty op1 op2
 	FDiv pc i ty op1 op2 -> tyanCheckBinInstr TyClassFloat nmdtye c i ty op1 op2
 	FRem pc i ty op1 op2 -> tyanCheckBinInstr TyClassFloat nmdtye c i ty op1 op2
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
	IntToPtr pc i v ty -> 										-- Integer -> Pointer
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

tyanCheckBinInstr :: TyClass -> NamedTyEnv -> Context -> Identifier -> Type -> Value -> Value -> (Context, TyAnn)
tyanCheckBinInstr TyClassInt  nmdtye (c,tye) i ty@(TyInt x) v1 v2 = 
    let (tv1,cv1) = rtyanValue nmdtye tye i v1
        (tv2,cv2) = rtyanValue nmdtye tye i v2
        tyl = liftTy ty
        nc  = c `S.union` cv1 `S.union` cv2
    in ((nc, insert i tyl tye), f tyl tv1 tv2)
tyanCheckBinInstr TyClassFloat nmdtye (c,tye) i ty@(TyFloatPoint x) v1 v2 = 
    let (tv1,cv1) = rtyanValue nmdtye tye i v1
        (tv2,cv2) = rtyanValue nmdtye tye i v2
        tyl = liftTy ty
        nc  = c `S.union` cv1 `S.union` cv2
    in ((nc,insert i tyl tye), f tyl tv1 tv2)
tyanCheckBinInstr n _ _ _ _ _ _ = error "tyanCheckBinInstr"