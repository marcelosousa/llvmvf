module Language.LLVMIR.Converter where
  
import qualified LLVM.FFI.Core as FFI
import LLVM.Core 
import Foreign.C.String

import qualified Language.LLVMIR as LL
import Debug.Trace

-- TODO: overflow support for binary operations (add/sub/mul)
getInst :: FFI.ValueRef -> IO LL.Instruction
getInst v = do opcode <- FFI.instGetOpcode v
               getInstruction' v opcode

type Opcode = Int

getType :: FFI.TypeKind -> IO LL.Type
getType FFI.VoidTypeKind     = return LL.TyVoid
{-getType FFI.FloatTypeKind    = return TDFloat
getType FFI.DoubleTypeKind   = return TDDouble
getType FFI.X86_FP80TypeKind = return "X86_FP80"
getType FFI.FP128TypeKind = return TDFP128
getType FFI.PPC_FP128TypeKind -> return "PPC_FP128"
getType FFI.LabelTypeKind -> return TDLabel
getType FFI.IntegerTypeKind -> do
           n <- FFI.getIntTypeWidth t
           return $ TDInt False (fromIntegral n)
getType FFI.FunctionTypeKind
getType FFI.StructTypeKind -> return "(Struct ...)"
getType FFI.ArrayTypeKind -> do
           n <- FFI.getArrayLength t
           et <- FFI.getElementType t
           etd <- typeDesc2 et
           return $ TDArray (fromIntegral n) etd
getType FFI.PointerTypeKind -> do
           et <- FFI.getElementType t
           etd <- typeDesc2 et
           return $ TDPtr etd
getType FFI.OpaqueTypeKind -> return "Opaque"
getType FFI.VectorTypeKind -> do
getType           n <- FFI.getVectorSize t
getType           et <- FFI.getElementType t
getType           etd <- typeDesc2 et
getType           return $ TDVector (fromIntegral n) etd
getType LLVMMetadataTypeKind,    /**< Metadata */
getType LLVMX86_MMXTypeKind      /**< X86 MMX */
getType _ -> return TDInvalidType
-}

getInstruction' :: FFI.ValueRef -> Opcode -> IO LL.Instruction
getInstruction' v 26 = do cs <- FFI.getValueName v
                          n  <- peekCString cs
                          tyref <- FFI.typeOf v
                          tk <- FFI.getTypeKind tyref
                          ty <- getType tk
                          return $ LL.Alloca (LL.Local n) ty -- 1 (LL.Align 1)
getInstruction' v _  = do (s,inst) <- getInstrDesc v
                                   -- sops <- foldM (\s (v,_) -> return $ v ++ " " ++ s) "" ops
                          return $ LL.Instruction (s ++ "=" ++ show inst)
{-    
    t <- FFI.typeOf v >>= typeDesc2
    -- FIXME: sizeof() does not work for types!
    --tsize <- FFI.typeOf v -- >>= FFI.sizeOf -- >>= FFI.constIntGetZExtValue >>= return . fromIntegral
    tsize <- return 1
    os <- U.getOperands v >>= mapM getArgDesc
    os0 <- if length os > 0 then return $ os !! 0 else return AE
    os1 <- if length os > 1 then return $ os !! 1 else return AE
    t2 <- (if not (null os) && (opcode >= 30 || opcode <= 41)
            then U.getOperands v >>= return . snd . head >>= FFI.typeOf >>= typeDesc2
            else return TDVoid)
    p <- if opcode `elem` [42, 43] then FFI.cmpInstGetPredicate v else return 0
    let instr =
            (if opcode >= 8 && opcode <= 25 -- binary arithmetic
             then IDBinOp (getBinOp opcode) t os0 os1
             else if opcode >= 30 && opcode <= 41 -- conversion
                  then (getConvOp opcode) t2 t os0
                  else case opcode of
                         { 1 -> if null os then IDRetVoid else IDRet t os0;
                           2 -> if length os == 1 then IDBrUncond os0 else IDBrCond os0 (os !! 2) os1;
                           3 -> IDSwitch $ toPairs os;
                           -- TODO (can skip for now)
                           -- 4 -> IndirectBr ; 5 -> Invoke ;
                           6 -> IDUnwind; 7 -> IDUnreachable;
                           26 -> IDAlloca (getPtrType t) tsize (getImmInt os0);
                           27 -> IDLoad t os0; 28 -> IDStore t os0 os1;
                           29 -> IDGetElementPtr t os;
                           42 -> IDICmp (toIntPredicate p) os0 os1;
                           43 -> IDFCmp (toFPPredicate p) os0 os1;
                           44 -> IDPhi t $ toPairs os;
                           -- FIXME: getelementptr arguments are not handled
                           45 -> IDCall t (last os) (init os);
                           46 -> IDSelect t os0 os1;
                           -- TODO (can skip for now)
                           -- 47 -> UserOp1 ; 48 -> UserOp2 ; 49 -> VAArg ;
                           -- 50 -> ExtractElement ; 51 -> InsertElement ; 52 -> ShuffleVector ;
                           -- 53 -> ExtractValue ; 54 -> InsertValue ;
                           _ -> IDInvalidOp })
    return (valueName, instr)
    --if instr /= InvalidOp then return instr else fail $ "Invalid opcode: " ++ show opcode
        where getBinOp o = fromList [(8, BOAdd), (9, BOFAdd), (10, BOSub), (11, BOFSub),
                                     (12, BOMul), (13, BOFMul), (14, BOUDiv), (15, BOSDiv),
                                     (16, BOFDiv), (17, BOURem), (18, BOSRem), (19, BOFRem),
                                     (20, BOShL), (21, BOLShR), (22, BOAShR), (23, BOAnd),
                                     (24, BOOr), (25, BOXor)] ! o
              getConvOp o = fromList [(30, IDTrunc), (31, IDZExt), (32, IDSExt), (33, IDFPtoUI),
                                      (34, IDFPtoSI), (35, IDUItoFP), (36, IDSItoFP), (37, IDFPTrunc),
                                      (38, IDFPExt), (39, IDPtrToInt), (40, IDIntToPtr), (41, IDBitcast)] ! o
              toPairs xs = zip (stride 2 xs) (stride 2 (drop 1 xs))
              stride _ [] = []
              stride n (x:xs) = x : stride n (drop (n-1) xs)
              getPtrType (TDPtr t) = t
              getPtrType _ = TDVoid
              getImmInt (AI i) = i
              getImmInt _ = 0

-}