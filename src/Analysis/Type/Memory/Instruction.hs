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
import Analysis.Type.Memory.TyAnn (TyAnn, TyAnnot)
import Analysis.Type.Standard.Constant
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
  -- Cast Operations
	Trunc    pc i v ty -> tyanCastOp nmdtye c i v ty isAnnInt (>)    -- Truncate integers
	ZExt     pc i v ty -> tyanCastOp nmdtye c i v ty isAnnInt (<)    -- Zero extend integers
	SExt     pc i v ty -> tyanCastOp nmdtye c i v ty isAnnInt (<)    -- Sign extend integers
	FPTrunc  pc i v ty -> tyanCastOp nmdtye c i v ty isAnnFloat (>)  -- Truncate floating point
	FPExt    pc i v ty -> tyanCastOp nmdtye c i v ty isAnnFloat (<=) -- Extend floating point
	FPToUI   pc i v ty -> 
	    let tyv = liftTy $ snd $ fptoint nmdtye (eraseEnv tye) i v ty              -- floating point -> UInt
		in ((ctrs, insert i tyv tye), tyv)
	FPToSI   pc i v ty -> 
		let tyv = liftTy $ snd $ fptoint nmdtye (eraseEnv tye) i v ty              -- floating point -> SInt
		in ((ctrs, insert i tyv tye), tyv)
	UIToFP   pc i v ty -> 
		let tyv = liftTy $ snd $ inttofp nmdtye (eraseEnv tye) i v ty              -- UInt -> floating point
		in ((ctrs, insert i tyv tye), tyv)
	SIToFP   pc i v ty -> 
		let tyv = liftTy $ snd $ inttofp nmdtye (eraseEnv tye) i v ty              -- SInt -> floating point
		in ((ctrs, insert i tyv tye), tyv)
	PtrToInt pc i v ty ->                                           -- Pointer -> Integer
		let tyv = typeUnaryExpression nmdtye (eraseEnv tye) "PtrToInt" 41 v ty
		    tyl = liftTy tyv
	    in ((ctrs, insert i tyl tye), tyl)
	IntToPtr pc i v ty -> 										-- Integer -> Pointer
		let tyv = typeValue nmdtye (eraseEnv tye) v
		    tyl = liftTy tyv
	    in if isInt tyv && isPointer ty 
	       then ((ctrs, insert i tyl tye), tyl)
	       else error $ "IntToPtr: Either type is not pointer or not int: " ++ show [tyv, ty] 
	BitCast  pc i v ty ->                                       -- Type cast
		let tyv = tyanUnaryExpression nmdtye tye "BitCast" 43 v ty
		    vi = case v of
		    	  Id x _ -> x
		    	  _ -> error "Bitcast !!!!!"
		in ((S.fromList [(i,vi),(vi,i)] `S.union` ctrs, insert i tyv tye), tyv)                
  -- Other Operations
 	ICmp pc i cond ty op1 op2 -> 
 	    let tyv = liftTy $ snd $ typeCheckCmp TyClassInt   (eraseEnv tye) i ty (typeValue nmdtye (eraseEnv tye) op1) (typeValue nmdtye (eraseEnv tye) op2)
 	    in ((ctrs, insert i tyv tye), tyv) 
 	FCmp pc i cond ty op1 op2 -> 
 	    let tyv = liftTy $ snd $ typeCheckCmp TyClassFloat (eraseEnv tye) i ty (typeValue nmdtye (eraseEnv tye) op1) (typeValue nmdtye (eraseEnv tye) op2)
 	    in ((ctrs, insert i tyv tye), tyv) 
  -- Memory Operations
 	Alloca pc i ty align -> 
 		let tyv = T.TyDer $ T.TyPtr (liftTyGen ty T.TyRegAddr) T.TyRegAddr
 		in ((ctrs, insert i tyv tye), tyv)-- Alloca should receive a size integer too  
 	Store  pc ty v1 v2 align ->
 		let (tyv2,cv2) = tyanValue nmdtye tye v2
 		    (tyv1,cv1) = tyanValue nmdtye tye v1
 		in case tyv2 of
 			T.TyDer (T.TyPtr tyv T.TyRegAddr) -> 
 				if tyv == tyv1 && isFstClass (erase tyv)
 				then (c, T.TyPri $ T.TyVoid)
 				else error $ "tyanCheckInstruction.Store(1): " ++ show [tyv,tyv1] ++ show [v1,v2] 
 	  		x -> error $ "tyanCheckInstruction Store(2): " ++ show x
	Load   pc i    v     align   ->
		let (tyv,cv) = rtyanValue nmdtye tye i v
		    vi = case v of
		    	  Id x _ -> x
		    	  _ -> error "Load !!!!!"
 		in case tyv of
 			T.TyDer (T.TyPtr ty T.TyRegAddr) -> 
 				if isFstClass (erase ty)
 				then ((S.fromList [(i,vi),(vi,i)] `S.union` ctrs `S.union` cv, insert i ty tye), T.TyPri $ T.TyVoid)
 				else error $ "tyanCheckInstruction.Load: " ++ show ty
 	  		x -> error $ "tyanCheckInstruction Load: " ++ show x
 	GetElementPtr pc i ty v idxs ->
 	  let (ety,lid) = tyanGetElementPtrConstantExpr nmdtye tye v idxs
 	      nc = S.union ctrs $ S.fromList $ map (\j -> (i,j)) lid
 	  in if ety == liftTy ty
      	 then ((nc,insert i ety tye), ety)
      	 else error $ "typeCheckInstruction.GetElementPtr: " ++ show [ety]
  -- Call Operation
  	Call pc i ty callee vs -> tyanCheckCall nmdtye c i ty callee vs
  -- Selection Operations
  	Select pc i cv vt vf       -> error "tyanCheckInstruction: select operation not supported."
  	ExtractValue pc i v idxs   -> 
  	  let ty = typeValue nmdtye (eraseEnv tye) v
  	  in if length idxs > 0
         then if isAgg ty
              then let t = getTypeAgg nmdtye ty idxs
                       tl = liftTy t
                   in ((ctrs, insert i tl tye), tl)
              else error $ "ExtractValue: " ++ show ty ++ " is not aggregate."  
         else error $ "ExtractValue: empty list" 
  	InsertValue pc i v vi idxs -> error "tyanCheckInstruction: InsertValue operation not supported"
  -- Atomic Operations
  	Cmpxchg   pc i mptr cval nval ord ->
  		let (tymptr,cmptr) = rtyanValue nmdtye tye i mptr
  		    (tycval,ccval) = rtyanValue nmdtye tye i cval
  		    (tynval,cnval) = rtyanValue nmdtye tye i nval
  		    nc = ctrs `S.union` cmptr `S.union` ccval `S.union` cnval
  		in case tymptr of
  			T.TyDer (T.TyPtr ty T.TyRegAddr) -> 
  				if ty == tycval && tycval == tynval
  				then ((nc, insert i ty tye), ty)
  				else error $ "Cmpxchg: Types are not equal " ++ show [ty,tycval,tynval]
  			x -> error $ "Cmpxchg: Type of first element is not pointer: " ++ show x
  	AtomicRMW pc i mptr val op ord -> 
  		let (tymptr,cmptr) = rtyanValue nmdtye tye i mptr
  		    (tyval,cval)   = rtyanValue nmdtye tye i val
  		    nc = ctrs `S.union` cmptr `S.union` cval
  		in case tymptr of
  			T.TyDer (T.TyPtr ty T.TyRegAddr) -> 
  			    if ty == tyval
  			    then ((nc, insert i ty tye), ty)
  			    else error $ "AtomicRMW: Types are not equal " ++ show [ty,tyval]
  			x -> error $ "AtomicRMW: Type of first element is not pointer: " ++ show x

tyanCheckCall :: NamedTyEnv -> Context -> Identifier -> Type -> Identifier -> Values -> (Context, TyAnn)
tyanCheckCall nmdtye c@(ctrs,tye) i rfnty ci args = 
	let ty = getFnTyAnn tye ci 
	in case ty of
		T.TyDer (T.TyPtr fnty T.TyRegAddr) ->
			case fnty of 
				T.TyDer (T.TyFun tps rty iv) ->
			  		let (tyargs, cns) = unzip $ map (rtyanValue nmdtye tye i) args
			  		    nc = foldr S.union ctrs cns
			  		    ii = getIdValue args
			  		in if (erase rty) == rfnty
			  	       then if all (\(a,b) -> a == b) $ zip tyargs tps
				 	  		then if iv || length tyargs == length tps
					       		 then case ci of 
					       		 	Global "free"   -> (filterIContext (nc,tye) ii, rty)
					       		 	Global "vfree"   -> (filterIContext (nc,tye) ii, rty)
					       		 	Global "iounmap" -> (filterIContext (nc,tye) ii, rty)
					       		 	_ -> if i == Local "" 
					       		 		 then (c, rty)
					       		 		 else ((nc, insert i rty tye), rty)
					       		 else error $ "tyanCheckCall: length mismatch in " ++ show ci
					  		else error $ "tyanCheckCall: argument type mismatch " ++ show (zip tyargs tps)
				 	   else error $ "tyanCheckCall: return type are different in " ++ show ci ++ "\n" ++ show [rty, ty]
				x -> error $ "tyanCheckCall: Function has type: " ++ show x
		x -> error $ "tyanCheckCall: Function has type: " ++ show x


getIdValue :: [Value] -> Identifier
getIdValue [Id v ty] = v
getIdValue _         = error $ "getIdValue: Args "

ptrtoMem :: TyAnnot -> TyAnn
ptrtoMem a = T.TyDer $ T.TyPtr (T.TyPri $ T.TyInt 8) a

getFnTyAnn :: TyAnnEnv -> Identifier -> TyAnn
getFnTyAnn tye (Global "vmalloc") = T.TyDer (T.TyPtr (T.TyDer (T.TyFun [T.TyPri $ T.TyInt 64] (ptrtoMem T.TyRegAddr) False)) T.TyRegAddr)
getFnTyAnn tye (Global "vfree")   = T.TyDer (T.TyPtr (T.TyDer (T.TyFun [ptrtoMem T.TyRegAddr] (T.TyPri T.TyVoid) False)) T.TyRegAddr)
getFnTyAnn tye (Global "malloc") = T.TyDer (T.TyPtr (T.TyDer (T.TyFun [T.TyPri $ T.TyInt 64] (ptrtoMem T.TyRegAddr) False)) T.TyRegAddr)
getFnTyAnn tye (Global "free")   = T.TyDer (T.TyPtr (T.TyDer (T.TyFun [ptrtoMem T.TyRegAddr] (T.TyPri T.TyVoid) False)) T.TyRegAddr)
getFnTyAnn tye (Global "ioremap") = T.TyDer (T.TyPtr (T.TyDer (T.TyFun [T.TyPri $ T.TyInt 64,T.TyPri $ T.TyInt 64] (ptrtoMem T.TyIOAddr) False)) T.TyRegAddr)
getFnTyAnn tye (Global "iounmap") = T.TyDer (T.TyPtr (T.TyDer (T.TyFun [ptrtoMem T.TyIOAddr] (T.TyPri T.TyVoid) False)) T.TyRegAddr)
getFnTyAnn tye ident@(Global i) =
	case M.lookup ident tye of
		Nothing -> case M.lookup (Local i) tye of
			Nothing -> error $ "getFnTyAnn: Function " ++ show ident ++ " not in env: " ++ show tye
			Just t  -> t
		Just t -> t
getFnTyAnn tye  (Local i) = error "getFnTyAnn: Local Identifier"

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

tyanCastOp :: NamedTyEnv -> Context -> Identifier -> Value -> Type -> (TyAnn -> Bool) -> (TyAnn -> TyAnn -> Bool) -> (Context, TyAnn)
tyanCastOp nmdtye (c,tye) i v ty top op = 
	let tyl = liftTy ty
	    (tyv,cv) = rtyanValue nmdtye tye i v
	    nc = c `S.union` cv
	in if top tyv && top tyl
	   then if op tyv tyl
		    then ((nc,insert i tyl tye), tyl)
		  	else error $ "tyanCastOp: op failed " ++ show [tyv,tyl]
		else error $ "tyanCastOp: not int " ++ show [tyv,tyl]