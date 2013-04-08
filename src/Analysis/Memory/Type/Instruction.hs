-------------------------------------------------------------------------------
-- Module    :  Analysis.Memory.Type.Instruction
-- Copyright :  (c) 2013 Marcelo Sousa
-- A Type System for Memory Analysis of LLVM IR Modules
-- Type Inference
-------------------------------------------------------------------------------

module Analysis.Memory.Type.Instruction where

import Analysis.Memory.TyAnn (TyAnn, TyAnnEnv)
import qualified Analysis.Memory.TyAnn as T
import Analysis.Memory.Type.Util
import Analysis.Memory.Type.Value
import Analysis.Memory.Type.Constant
import Language.LLVMIR
import qualified Data.Map as M
import Debug.Trace (trace)

typeCheckBinInstr :: TyClass -> TyEnv -> Identifier -> Type -> Type -> Type -> (TyEnv, Maybe Type)
typeCheckBinInstr TyClassInt tye i ty@(TyInt x)        tv1 tv2 = (insert i ty tye, f ty tv1 tv2)
typeCheckBinInstr TyClassFloat tye i ty@(TyFloatPoint x) tv1 tv2 = (insert i ty tye, f ty tv1 tv2)
typeCheckBinInstr n _   _ _ _ _ = error "typeCheckBinInstr"
f t1 t2 t3 = if t1 == t2 && t2 == t3 
		     then Just t1
		     else Nothing

-- Need to pattern match on the value here.
typeCheckBranchs :: Values -> Maybe Type
typeCheckBranchs [] = Nothing
typeCheckBranchs l  = do ids <- mapM typeCheckBranch l
                         return $ TyJumpTo ids

typeCheckBranch :: Value -> Maybe Identifier
typeCheckBranch (Id i ty) = Just i -- TODO need to check if ty is a TyLabel or *(TyInt 8)
typeCheckBranch v = Nothing

typeCheckCmp :: TyClass -> TyEnv -> Identifier -> Type -> Type -> Type -> (TyEnv, Maybe Type)
typeCheckCmp TyClassInt tye i ty@(TyInt 1) tv1 tv2 =
    let ntye = insert i ty tye 
	in if tv1 == tv2 
	   then case tv1 of
	     TyPointer _ -> (ntye, Just ty)
	     TyInt _     -> (ntye, Just ty)
	     x           -> (tye, Nothing)
	   else (tye, Nothing)
typeCheckCmp TyClassInt tye i ty@(TyVector s (TyInt _)) tv1 tv2 =
    let ntye = insert i ty tye 
	in if tv1 == tv2 
	   then case tv1 of
	     TyVector r (TyInt _) -> if r == s 
	     	                     then (ntye, Just ty)
	                             else (tye, Nothing)
	     x           -> (tye, Nothing)
	   else (tye, Nothing)
typeCheckCmp TyClassFloat tye i ty@(TyInt 1) tv1 tv2 =
    let ntye = insert i ty tye 
	in if tv1 == tv2 
	   then case tv1 of
	     TyPointer _    -> (ntye, Just ty)
	     TyFloatPoint _ -> (ntye, Just ty)
	     x              -> (tye, Nothing)
	   else (tye, Nothing)
typeCheckCmp TyClassFloat tye i ty@(TyVector s (TyFloatPoint _)) tv1 tv2 =
    let ntye = insert i ty tye 
	in if tv1 == tv2 
	   then case tv1 of
	     TyVector r (TyFloatPoint _) -> if r == s 
	     	                            then (ntye, Just ty)
	                                    else (tye, Nothing)
	     x           -> (tye, Nothing)
	   else (tye, Nothing)

typeCheckInstruction :: TyEnv -> Instruction -> (TyEnv, Maybe Type)
typeCheckInstruction tye i = case i of
  -- Terminators
	Ret pc VoidRet      -> (tye, Just $ TyVoid)
	Ret pc (ValueRet v) -> (tye, Just $ typeValue tye v) 
	Unreachable pc      -> (tye, Just $ TyUndefined) -- Unreachable has no defined semantics 
	Br  pc v t f        -> if typeValue tye v == TyInt 1
		                   then (tye, typeCheckBranchs [t,f])
		                   else (tye, Nothing)
	UBr pc d            -> (tye, typeCheckBranchs [d])
	Switch pc ty v elems -> error "typeCheckInstruction: Switch instruction not supported."
  -- Phi Instructions
 	PHI pc i ty vals -> let (vs,ls) = unzip vals
 	                        tyvs = map (typeValue tye) vs
 	                        tyls = map typeCheckBranch ls
 	                        p1 = all (==ty) tyvs
 	                        p2 = all (\x -> case x of
 	                        	              Just _ -> True
 	                        	              Nothing -> False) tyls
 	                    in if p1 && p2
 	                       then (insert i ty tye, Just ty)
 	                       else (tye, Nothing)
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
 	Alloca pc i ty       align   -> trace (show ty) $ (insert i (TyPointer ty) tye, Just ty)-- Alloca should receive a size integer too  
 	Store  pc   ty v1 v2 align   -> 
 	  case typeValue tye v2 of
 	  	TyPointer ty -> 
 	  	  let tyv2 = typeValue tye v1
 	  	  in if ty == tyv2 && isFstClass ty
 	  	     then (tye, Just $ TyVoid)
 	  	     else (tye, Nothing) 
 	  	x -> error $ "typeCheck Store: " ++ show x
	Load   pc i    v     align   -> 
	  case typeValue tye v of
	  	TyPointer ty -> if isFstClass ty
	  		            then (insert i ty tye, Just ty)
	  		            else (tye, Nothing)
	  	x -> error $ "typeCheck Load: " ++ show x
 	GetElementPtr pc i ty v idxs ->
 	  let ety = typeGetElementPtrConstantExpr tye v idxs
 	  in if ety == ty
      	 then (insert i ty tye, Just ty)
      	 else (tye, Nothing)
  -- Call Operation
  	Call pc i ty callee vs -> typeCheckCall tye i ty callee vs
  -- Selection Operations
  	Select pc i cv vt vf       -> error "select operation not supported."
  	ExtractValue pc i v idxs   -> error "ExtractValue operation not supported"
  	InsertValue pc i v vi idxs -> error "InsertValue operation not supported"
  -- Atomic Operations
  	Cmpxchg   pc i mptr cval nval ord -> error "Cmpxchg operation not supported"
  	AtomicRMW pc i vals op        ord -> error "AtomicRMW operation not supported"


typeCheckCall :: TyEnv -> Identifier -> Type -> Identifier -> Values -> (TyEnv, Maybe Type)
typeCheckCall tye i ty c args = 
	case M.lookup c tye of
		Nothing -> error $ "typeCheckCall: Function " ++ show c ++ " not in env: " ++ show tye
		Just ty -> case ty of
			TyPointer (TyFunction typms tyr) -> 
			  let tyargs = map (typeValue tye) args
			  in if (length tyargs == length typms) && tyr == ty
			  	then if all (\(a,b) -> a == b) $ zip tyargs typms
					 then (insert i ty tye, Just ty)
					 else (tye, Nothing) 
				else (tye, Nothing)
			x -> error $ "typeCheckCall: Function has type: " ++ show x

iTyInf :: TyAnnEnv -> Instruction -> (TyAnn, TyAnnEnv)
iTyInf tyenv (Ret _ VoidRet)      = (T.TyPri T.TyVoid, tyenv)
iTyInf tyenv (Ret _ (ValueRet v)) = vTyInf tyenv v
iTyInf tyenv (Unreachable _)      = (T.TyBot, tyenv) -- Unreachable has no defined semantics 
iTyInf tyenv (Add _ i ty op1 op2) = undefined
-- Conversion Operations
iTyInf tyenv (SExt    _ i v ty)   = convTyInf tyenv i v ty
iTyInf tyenv (BitCast _ i v ty)   = convTyInf tyenv i v ty
-- Memory Operations
-- The pointer of a load must a first class type.
iTyInf tyenv (Load _ i v a)       = let (vty, tye) = vTyInf tyenv v
                                    in (vty, M.insert i vty tye)

-- Auxiliar Function
convTyInf :: TyAnnEnv -> Identifier -> Value -> Type -> (TyAnn, TyAnnEnv)
convTyInf tyenv i v ty = let ity = liftTy ty
                             (vty, tye) = vTyInf tyenv v
                             nty = castTy vty ity
                         in (nty, M.insert i nty tye)
