module Language.LLVMIR.Converter where
  
import qualified LLVM.FFI.Core as FFI
import LLVM.Core hiding (Value)

import Foreign.C.String

import qualified Language.LLVMIR as LL

type Type     = FFI.TypeRef
type Value    = FFI.ValueRef 
type TypeKind = FFI.TypeKind

type Opcode = Int

convertLinkage :: FFI.Linkage -> LL.Linkage
convertLinkage FFI.ExternalLinkage                 = LL.ExternalLinkage  
convertLinkage FFI.AvailableExternallyLinkage      = LL.AvailableExternallyLinkage
convertLinkage FFI.LinkOnceAnyLinkage              = LL.LinkOnceAnyLinkage             
convertLinkage FFI.LinkOnceODRLinkage              = LL.LinkOnceODRLinkage             
convertLinkage FFI.WeakAnyLinkage                  = LL.WeakAnyLinkage                 
convertLinkage FFI.WeakODRLinkage                  = LL.WeakODRLinkage                 
convertLinkage FFI.AppendingLinkage                = LL.AppendingLinkage               
convertLinkage FFI.InternalLinkage                 = LL.InternalLinkage                
convertLinkage FFI.PrivateLinkage                  = LL.PrivateLinkage                 
convertLinkage FFI.DLLImportLinkage                = LL.DLLImportLinkage               
convertLinkage FFI.DLLExportLinkage                = LL.DLLExportLinkage               
convertLinkage FFI.ExternalWeakLinkage             = LL.ExternalWeakLinkage            
convertLinkage FFI.GhostLinkage                    = LL.GhostLinkage                   
convertLinkage FFI.CommonLinkage                   = LL.CommonLinkage                  
convertLinkage FFI.LinkerPrivateLinkage            = LL.LinkerPrivateLinkage           
convertLinkage FFI.LinkerPrivateWeakLinkage        = LL.LinkerPrivateWeakLinkage       
convertLinkage FFI.LinkerPrivateWeakDefAutoLinkage = LL.LinkerPrivateWeakDefAutoLinkage


{- TODO
FFI.FunctionTypeKind
FFI.StructTypeKind
FFI.MetadataTypeKind 
FFI.X86_MMXTypeKind
-}
getType :: Type -> IO LL.Type
getType ty = do tyk <- FFI.getTypeKind ty
                getTypeWithKind ty tyk

getTypeWithKind :: Type -> TypeKind -> IO LL.Type
getTypeWithKind ty FFI.VoidTypeKind      = return LL.TyVoid
getTypeWithKind ty FFI.FloatTypeKind     = return $ LL.TyFloatPoint LL.TyFloat
getTypeWithKind ty FFI.DoubleTypeKind    = return $ LL.TyFloatPoint LL.TyDouble
getTypeWithKind ty FFI.X86_FP80TypeKind  = return $ LL.TyFloatPoint LL.Tyx86FP80
getTypeWithKind ty FFI.FP128TypeKind     = return $ LL.TyFloatPoint LL.TyFP128
getTypeWithKind ty FFI.PPC_FP128TypeKind = return $ LL.TyFloatPoint LL.TyPPCFP128
getTypeWithKind ty FFI.LabelTypeKind     = return LL.TyLabel
getTypeWithKind ty FFI.OpaqueTypeKind    = return LL.TyOpaque
getTypeWithKind ty FFI.IntegerTypeKind   = do n <- FFI.getIntTypeWidth ty
                                              return $ LL.TyInt $ fromIntegral n
getTypeWithKind ty FFI.PointerTypeKind   = do et  <- FFI.getElementType ty
                                              etd <- getType et 
                                              return $ LL.TyPointer etd
getTypeWithKind ty FFI.ArrayTypeKind     = do n   <- FFI.getArrayLength ty
                                              et  <- FFI.getElementType ty
                                              etd <- getType et 
                                              return $ LL.TyArray (fromIntegral n) etd
getTypeWithKind ty FFI.VectorTypeKind    = do n   <- FFI.getVectorSize ty
                                              et  <- FFI.getElementType ty
                                              etd <- getType et 
                                              return $ LL.TyVector (fromIntegral n) etd
getTypeWithKind ty FFI.StructTypeKind    = do s <- (FFI.getStructName ty) >>= peekCString
                                              return $ LL.TyStruct s
getTypeWithKind ty _  = return LL.TyUnsupported

getInst :: Value -> IO LL.Instruction
getInst v = do opcode <- FFI.instGetOpcode v
               getInstruction' v opcode

getInstruction' :: Value -> Opcode -> IO LL.Instruction
--getInstruction' v 26 = do cs <- FFI.getValueName v
--                          n  <- peekCString cs
--                          tyref <- FFI.typeOf v
--                          tk <- FFI.getTypeKind tyref
--                          ty <- getType tyref tk
--                          a  <- FFI.getAlignment v
--                          return $ LL.Alloca (LL.Local n) ty 1 -- (fromIntegral a)
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
