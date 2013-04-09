-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Standard.Instruction
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Type.Standard.Instruction where

import Analysis.Type.Util
import Analysis.Type.Standard.Constant
import Language.LLVMIR
import qualified Data.Map as M
import Debug.Trace (trace)

-- Need to pattern match on the value here.
typeCheckBranchs :: Values -> Type
typeCheckBranchs [] = error "typeCheckBranchs: Empty value list"
typeCheckBranchs l  = TyJumpTo $ map typeCheckBranch l

typeCheckBranch :: Value -> Identifier
typeCheckBranch (Id i ty) = i -- TODO need to check if ty is a TyLabel or *(TyInt 8)
typeCheckBranch v = error $ "typeCheckBranchs: Expected identifier and given " ++ show v

typeCheckInstruction :: TyEnv -> Instruction -> (TyEnv, Type)
typeCheckInstruction tye i = case i of
  -- Terminators
	Ret pc VoidRet      -> (tye, TyVoid)
	Ret pc (ValueRet v) -> (tye, typeValue tye v) 
	Unreachable pc      -> (tye, TyUndefined) -- Unreachable has no defined semantics 
	Br  pc v t f        -> if typeValue tye v == TyInt 1
		                   then (tye, typeCheckBranchs [t,f])
		                   else error "typeCheckInstruction.Br: Condition type is not i1"
	UBr pc d            -> (tye, typeCheckBranchs [d])
	Switch pc ty v elems -> error "typeCheckInstruction: Switch instruction not supported."
  -- Phi Instructions
 	PHI pc i ty vals -> let (vs,ls) = unzip vals
 	                        tyvs = map (typeValue tye) vs
 	                        tyls = map typeCheckBranch ls
 	                        p1 = all (==ty) tyvs
 	                    in if seq tyls p1
 	                       then (insert i ty tye, ty)
 	                       else error $ "typeCheckInstruction.PHI: " ++ show ty ++ " " ++ show tyvs
  -- Standard Binary Operations
  -- Integer Operations
 	Add  pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2) 
 	Sub  pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2) 
 	Mul  pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2) 
 	UDiv pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2) 
 	SDiv pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2) 
 	URem pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2) 
 	SRem pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2) 
  -- Bitwise Binary Operations
	Shl  pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2)
	LShr pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2)
	AShr pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2)
	And  pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2)
	Or   pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2)
	Xor  pc i ty op1 op2 -> typeCheckBinInstr TyClassInt tye i ty (typeValue tye op1) (typeValue tye op2)
  -- Float Operations
 	FAdd pc i ty op1 op2 -> typeCheckBinInstr TyClassFloat tye i ty (typeValue tye op1) (typeValue tye op2)
 	FSub pc i ty op1 op2 -> typeCheckBinInstr TyClassFloat tye i ty (typeValue tye op1) (typeValue tye op2)
 	FMul pc i ty op1 op2 -> typeCheckBinInstr TyClassFloat tye i ty (typeValue tye op1) (typeValue tye op2)
 	FDiv pc i ty op1 op2 -> typeCheckBinInstr TyClassFloat tye i ty (typeValue tye op1) (typeValue tye op2)
 	FRem pc i ty op1 op2 -> typeCheckBinInstr TyClassFloat tye i ty (typeValue tye op1) (typeValue tye op2)
  -- Cast Operations
	Trunc    pc i v ty -> error "cast operation not supported." -- Truncate integers
	ZExt     pc i v ty -> error "cast operation not supported." -- Zero extend integers
	SExt     pc i v ty -> error "cast operation not supported." -- Sign extend integers
	FPToUI   pc i v ty -> error "cast operation not supported." -- floating point -> UInt
	FPToSI   pc i v ty -> error "cast operation not supported." -- floating point -> SInt
	UIToFP   pc i v ty -> error "cast operation not supported." -- UInt -> floating point
	SIToFP   pc i v ty -> error "cast operation not supported." -- SInt -> floating point
	FPTrunc  pc i v ty -> error "cast operation not supported." -- Truncate floating point
	FPExt    pc i v ty -> error "cast operation not supported." -- Extend floating point
	PtrToInt pc i v ty -> error "cast operation not supported." -- Pointer -> Integer
	IntToPtr pc i v ty -> error "cast operation not supported." -- Integer -> Pointer
	BitCast  pc i v ty -> error "cast operation not supported." -- Type cast      
  -- Other Operations
 	ICmp pc i cond ty op1 op2 -> typeCheckCmp TyClassInt   tye i ty (typeValue tye op1) (typeValue tye op2)
 	FCmp pc i cond ty op1 op2 -> typeCheckCmp TyClassFloat tye i ty (typeValue tye op1) (typeValue tye op2)
  -- Memory Operations
 	Alloca pc i ty       align   -> (insert i (TyPointer ty) tye, ty)-- Alloca should receive a size integer too  
 	Store  pc   ty v1 v2 align   -> 
 	  case typeValue tye v2 of
 	  	TyPointer ty -> 
 	  	  let tyv2 = typeValue tye v1
 	  	  in if ty == tyv2 && isFstClass ty
 	  	     then (tye, TyVoid)
 	  	     else error $ "typeCheckInstruction.Store: " ++ show ty 
 	  	x -> error $ "typeCheck Store: " ++ show x
	Load   pc i    v     align   -> 
	  case typeValue tye v of
	  	TyPointer ty -> if isFstClass ty
	  		            then (insert i ty tye, ty)
	  		            else error $ "typeCheckInstruction.Load: " ++ show ty 
	  	x -> error $ "typeCheck Load: " ++ show x
 	GetElementPtr pc i ty v idxs ->
 	  let ety = typeGetElementPtrConstantExpr tye v idxs
 	  in if ety == ty
      	 then (insert i ty tye, ty)
      	 else error $ "typeCheckInstruction.GetElementPtr: " ++ show [ety,ty]
  -- Call Operation
  	Call pc i ty callee vs -> typeCheckCall tye i ty callee vs
  -- Selection Operations
  	Select pc i cv vt vf       -> error "select operation not supported."
  	ExtractValue pc i v idxs   -> error "ExtractValue operation not supported"
  	InsertValue pc i v vi idxs -> error "InsertValue operation not supported"
  -- Atomic Operations
  	Cmpxchg   pc i mptr cval nval ord -> error "Cmpxchg operation not supported"
  	AtomicRMW pc i vals op        ord -> error "AtomicRMW operation not supported"


typeCheckCall :: TyEnv -> Identifier -> Type -> Identifier -> Values -> (TyEnv, Type)
typeCheckCall tye i rfnty c args = 
	case M.lookup c tye of
		Nothing -> error $ "typeCheckCall: Function " ++ show c ++ " not in env: " ++ show tye
		Just ty -> case ty of
			TyPointer (TyFunction typms tyr) ->
			  let tyargs = map (typeValue tye) args
			  in if (length tyargs == length typms) && tyr == rfnty
			  	then if all (\(a,b) -> a == b) $ zip tyargs typms
					 then (insert i tyr tye, ty)
					 else error $ "typeCheckCall: " ++ show (zip tyargs typms)
				else error $ "typeCheckCall: length dismatch " ++ show [tyargs, typms] ++ show [tyr, ty]
			x -> error $ "typeCheckCall: Function has type: " ++ show x

typeCheckBinInstr :: TyClass -> TyEnv -> Identifier -> Type -> Type -> Type -> (TyEnv, Type)
typeCheckBinInstr TyClassInt   tye i ty@(TyInt x)        tv1 tv2 = (insert i ty tye, f ty tv1 tv2)
typeCheckBinInstr TyClassFloat tye i ty@(TyFloatPoint x) tv1 tv2 = (insert i ty tye, f ty tv1 tv2)
typeCheckBinInstr n _ _ _ _ _ = error "typeCheckBinInstr"
f t1 t2 t3 = if t1 == t2 && t2 == t3 
		     then t1
		     else error $ "typeCheckBinInstr: " ++ show [t1,t2,t3]
		     
typeCheckCmp :: TyClass -> TyEnv -> Identifier -> Type -> Type -> Type -> (TyEnv, Type)
typeCheckCmp TyClassInt tye i ty@(TyInt 1) tv1 tv2 =
    let ntye = insert i ty tye 
	in if tv1 == tv2 
	   then case tv1 of
	     TyPointer _ -> (ntye, ty)
	     TyInt _     -> (ntye, ty)
	     x           -> error $ "typeCheckCmp.TyClassInt: " ++ show x
	   else error $ "typeCheckCmp.TyClassInt: " ++ show [tv1,tv2]
typeCheckCmp TyClassInt tye i ty@(TyVector s (TyInt _)) tv1 tv2 =
    let ntye = insert i ty tye 
	in if tv1 == tv2
	   then case tv1 of
	     TyVector r (TyInt _) -> if r == s 
	     	                     then (ntye, ty)
	                             else error $ "typeCheckCmp.TyClassInt(2): " ++ show [r,s]
	     x -> error $ "typeCheckCmp.TyClassInt(2): " ++ show x
	   else error $ "typeCheckCmp.TyClassInt(2): " ++ show [tv1,tv2]
typeCheckCmp TyClassFloat tye i ty@(TyInt 1) tv1 tv2 =
    let ntye = insert i ty tye 
	in if tv1 == tv2 
	   then case tv1 of
	     TyPointer _    -> (ntye, ty)
	     TyFloatPoint _ -> (ntye, ty)
	     x -> error $ "typeCheckCmp.TyClassFloat: " ++ show x
	   else error $ "typeCheckCmp.TyClassFloat: " ++ show [tv1,tv2]
typeCheckCmp TyClassFloat tye i ty@(TyVector s (TyFloatPoint _)) tv1 tv2 =
    let ntye = insert i ty tye 
	in if tv1 == tv2 
	   then case tv1 of
	     TyVector r (TyFloatPoint _) -> if r == s 
	     	                            then (ntye, ty)
	                                    else error $ "typeCheckCmp.TyClassFloat(2): " ++ show [r,s]
	     x           -> error $ "typeCheckCmp.TyClassFloat(2): " ++ show x
	   else error $ "typeCheckCmp.TyClassFloat(2): " ++ show [tv1,tv2]
