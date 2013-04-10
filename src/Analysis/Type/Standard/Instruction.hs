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
import Data.Maybe
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
	Trunc    pc i v ty -> typeCastOp tye i v ty isInt (>)       -- Truncate integers
	ZExt     pc i v ty -> typeCastOp tye i v ty isInt (<)       -- Zero extend integers
	SExt     pc i v ty -> typeCastOp tye i v ty isInt (<)       -- Sign extend integers
	FPTrunc  pc i v ty -> typeCastOp tye i v ty isFloat (>)     -- Truncate floating point
	FPExt    pc i v ty -> typeCastOp tye i v ty isFloat (<=)    -- Extend floating point
	FPToUI   pc i v ty -> fptoint    tye i v ty                 -- floating point -> UInt
	FPToSI   pc i v ty -> fptoint    tye i v ty                 -- floating point -> SInt
	UIToFP   pc i v ty -> inttofp    tye i v ty                 -- UInt -> floating point
	SIToFP   pc i v ty -> inttofp    tye i v ty                 -- SInt -> floating point
	PtrToInt pc i v ty ->                                       -- Pointer -> Integer
		let tyv = typeValue tye v
	    in if isPointer tyv && isInt ty 
	       then (insert i ty tye, ty)
	       else error $ "PtrToInt: Either type is not pointer or not int: " ++ show [tyv, ty] 
	IntToPtr pc i v ty -> 										-- Integer -> Pointer
		let tyv = typeValue tye v
	    in if isPointer tyv && isInt ty 
	       then (insert i ty tye, ty)
	       else error $ "PtrToInt: Either type is not pointer or not int: " ++ show [tyv, ty] 
	BitCast  pc i v ty -> bitcast tye i v ty                    -- Type cast      
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


typeCastOp :: TyEnv -> Identifier -> Value -> Type -> (Type -> Bool) -> (Type -> Type -> Bool) -> (TyEnv, Type)
typeCastOp tye i v ty top op = 
	let tyv = typeValue tye v
	in if top tyv && top ty
	   then if op tyv ty
		    then (insert i ty tye, ty)
		  	else error $ "typeCastOpInt: op failed " ++ show [tyv,ty]
		else error $ "typeCastOpInt: not int " ++ show [tyv,ty]

fptoint :: TyEnv -> Identifier -> Value -> Type -> (TyEnv, Type)
fptoint tye i v ty = 
	let tyv = typeValue tye v
	in case tyv of
		TyFloatPoint fp -> if isInt ty 
			               then (insert i ty tye, ty)
			               else error $ "fptoint: Type " ++ show ty ++ " is not an int"
		TyVector n (TyFloatPoint _) -> case ty of
			TyVector m (TyInt _) -> if n == m 
									then (insert i ty tye, ty)
									else error $ "fptoint: vector sizes dont match"
			x -> error $ "fptoint: " ++ show x ++ " is not a vector of ints"
		x -> error $ "fptoint: Type " ++ show x ++ " is not a float or vector of floats"

bitcast :: TyEnv -> Identifier -> Value -> Type -> (TyEnv, Type)
bitcast tye i v ty = 
	let tyv = typeValue tye v
	in if notAggFstClass tyv && notAggFstClass ty
	   then if sizeof tyv == sizeof ty
	   	    then (insert i ty tye, ty)
	   	    else error $ "bitcast: " ++ show [tyv, ty] ++ " have different bit sizes"
	   else error $ "bitcast: One of the types " ++ show [tyv, ty] ++ " is aggregate or not fst class" 

inttofp :: TyEnv -> Identifier -> Value -> Type -> (TyEnv, Type)
inttofp tye i v ty = 
	let tyv = typeValue tye v
	in case tyv of
		TyInt _ -> if isFloat ty 
			       then (insert i ty tye, ty)
			       else error $ "inttofp: Type " ++ show ty ++ " is not a float"
		TyVector n (TyInt _) -> case ty of
			TyVector m (TyFloatPoint _) -> if n == m 
									then (insert i ty tye, ty)
									else error $ "inttofp: vector sizes dont match"
			x -> error $ "inttofp: " ++ show x ++ " is not a vector of floats"
		x -> error $ "inttofp: Type " ++ show x ++ " is not an int or vector of ints"

typeCheckCall :: TyEnv -> Identifier -> Type -> Identifier -> Values -> (TyEnv, Type)
typeCheckCall tye i rfnty c args = 
	let ty = getFnType tye c 
	in case ty of
			TyPointer (TyFunction typms tyr iv) ->
			  let tyargs = map (typeValue tye) args
			  in if tyr == rfnty
			  	 then if all (\(a,b) -> a == b) $ zip tyargs typms
				 	  then if iv || length tyargs == length typms
					       then (insert i tyr tye, ty)
					       else error $ "typeCheckCall: length mismatch in " ++ show c
					  else error $ "typeCheckCall: argument type mismatch " ++ show (zip tyargs typms)
				 else error $ "typeCheckCall: return type are different in " ++ show c ++ "\n" ++ show [tyr, ty]
			x -> error $ "typeCheckCall: Function has type: " ++ show x

getFnType :: TyEnv -> Identifier -> Type
getFnType tye ident@(Global i) =
	case M.lookup ident tye of
		Nothing -> case M.lookup (Local i) tye of
			Nothing -> error $ "getFnType: Function " ++ show ident ++ " not in env: " ++ show tye
			Just t  -> t
		Just t -> t
getFnType tye  (Local i) = error "getFnType: Local Identifier"

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
