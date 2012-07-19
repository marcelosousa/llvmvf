{-#LANGUAGE DoAndIfThenElse #-}
module Language.LLVMIR.Converter where
  
import qualified LLVM.FFI.Core as FFI
import LLVM.Core hiding (Value)

import Foreign.C.String

import qualified Language.LLVMIR as LL
import Language.LLVMIR.Util

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
               pInstruction v opcode

getValue :: (String, Value) -> IO LL.Value
getValue (n, v) = do isC <- (FFI.isConstant v)
                     if (cInt2Bool isC)
                     then getConstantValue v
                     else return $ LL.Id $ LL.Local n 

getConstantValue :: Value -> IO LL.Value
getConstantValue v = do ty <- (FFI.typeOf v) >>= getType
                        case ty of
                             LL.TyInt _ -> do av <- FFI.constIntGetSExtValue v
                                              return $ LL.Const $ LL.IntC (fromIntegral av) ty
                             _          -> do return $ LL.Const $ LL.UndefC 

getICmpOps :: Value -> IO (LL.Value, LL.Value)
getICmpOps v = do ops <- (getOperands v) >>= mapM getValue
                  if (length ops == 2)
                  then return (head ops, last ops)
                  else error "icmp ops length is != 2"

pInstruction :: Value -> Opcode -> IO LL.Instruction
pInstruction ival 1  = do n <- FFI.returnInstGetNumSuccessors ival
                          mv <- FFI.returnInstHasReturnValue ival
                          if cInt2Bool mv
                          then return $ LL.Ret $ LL.Const $ LL.UndefC
                          else return $ LL.Ret $ LL.Const $ LL.NullC LL.TyVoid
--                          v <- FFI.returnInstGetReturnValue ival
pInstruction ival 2  = do isCond <- FFI.brInstIsConditional ival
                          ops    <- (getOperands ival) >>= mapM getValue 
                          if (cInt2Bool isCond)
                          then return $ LL.Br (ops!!0) (ops!!1) (ops!!2)
                          else return $ LL.UBr (ops!!0)

pInstruction ival 3  = return $ LL.Instruction "switch"
pInstruction ival 4  = return $ LL.Instruction "indirectbr"
pInstruction ival 5  = return $ LL.Instruction "invoke"
-- removed 6 due to API changes
pInstruction ival 7  = return $ LL.Instruction "unreachable"
-- standard binary operators
pInstruction ival 8  = return $ LL.Instruction "add"
pInstruction ival 9  = return $ LL.Instruction "fadd"
pInstruction ival 10 = return $ LL.Instruction "sub"
pInstruction ival 11 = return $ LL.Instruction "fsub"
pInstruction ival 12 = return $ LL.Instruction "mul"
pInstruction ival 13 = return $ LL.Instruction "fmul"
pInstruction ival 14 = return $ LL.Instruction "udiv"
pInstruction ival 15 = return $ LL.Instruction "sdiv"
pInstruction ival 16 = return $ LL.Instruction "fdiv"
pInstruction ival 17 = return $ LL.Instruction "urem"
pInstruction ival 18 = return $ LL.Instruction "srem"
pInstruction ival 19 = return $ LL.Instruction "frem"
-- logical operators
pInstruction ival 20 = return $ LL.Instruction "shl"
pInstruction ival 21 = return $ LL.Instruction "lshr"
pInstruction ival 22 = return $ LL.Instruction "ashr"
pInstruction ival 23 = return $ LL.Instruction "and"
pInstruction ival 24 = return $ LL.Instruction "or"
pInstruction ival 25 = return $ LL.Instruction "xor"
-- memory operators
pInstruction ival 26 = do ident <- (FFI.getValueName ival) >>= peekCString
                          ty <- (FFI.allocaGetAllocatedType ival) >>= getType
                          a  <- FFI.allocaGetAlignment ival
                          return $ LL.Alloca (LL.Local ident) ty (LL.Align $ fromIntegral a)
pInstruction ival 27 = return $ LL.Instruction "load"
pInstruction ival 28 = return $ LL.Instruction "store"
pInstruction ival 29 = return $ LL.Instruction "getelementptr"
-- cast operators
pInstruction ival 30 = return $ LL.Instruction "trunc"
pInstruction ival 31 = return $ LL.Instruction "zext"
pInstruction ival 32 = return $ LL.Instruction "sext"
pInstruction ival 33 = return $ LL.Instruction "FPToUI"
pInstruction ival 34 = return $ LL.Instruction "FPToSI"
pInstruction ival 35 = return $ LL.Instruction "UIToFP"
pInstruction ival 36 = return $ LL.Instruction "SIToFP"
pInstruction ival 37 = return $ LL.Instruction "FPTrunc"
pInstruction ival 38 = return $ LL.Instruction "TFext"
pInstruction ival 39 = return $ LL.Instruction "PtrToInt"
pInstruction ival 40 = return $ LL.Instruction "IntToPtr"
pInstruction ival 41 = return $ LL.Instruction "bitcast"
-- other operators
pInstruction ival 42 = do ident <- (FFI.getValueName ival) >>= peekCString
                          cond  <- FFI.cmpInstGetPredicate ival >>= (return . fromEnum)
                          ty    <- (FFI.typeOf ival) >>= getType
                          (op1,op2) <- getICmpOps ival 
                          return $ LL.ICmp (LL.Local ident) (toIntPredicate cond) ty op1 op2
pInstruction ival 43 = return $ LL.Instruction "fcmp"
pInstruction ival 44 = return $ LL.Instruction "phi"
pInstruction ival 45 = return $ LL.Instruction "call"
pInstruction ival 46 = return $ LL.Instruction "select"
pInstruction ival 47 = return $ LL.Instruction "userop1"
pInstruction ival 48 = return $ LL.Instruction "userop2"
pInstruction ival 49 = return $ LL.Instruction "aarg"
pInstruction ival 50 = return $ LL.Instruction "extractelement"
pInstruction ival 51 = return $ LL.Instruction "insertelement"
pInstruction ival 52 = return $ LL.Instruction "shufflevector"
pInstruction ival 53 = return $ LL.Instruction "extractvalue"
pInstruction ival 54 = return $ LL.Instruction "insertvalue"
-- atomic operators
pInstruction ival 55 = return $ LL.Instruction "fence"
pInstruction ival 56 = return $ LL.Instruction "atomiccmpxchg"
pInstruction ival 57 = return $ LL.Instruction "atomicrmw"
-- exception handling operators
pInstruction ival 58 = return $ LL.Instruction "resume"
pInstruction ival 59 = return $ LL.Instruction "landingpad"

toIntPredicate :: Int -> LL.IntPredicate
toIntPredicate 32 = LL.IntEQ 
toIntPredicate 33 = LL.IntNE 
toIntPredicate 34 = LL.IntUGT
toIntPredicate 35 = LL.IntUGE
toIntPredicate 36 = LL.IntULT
toIntPredicate 37 = LL.IntULE
toIntPredicate 38 = LL.IntSGT
toIntPredicate 39 = LL.IntSGE
toIntPredicate 40 = LL.IntSLT
toIntPredicate 41 = LL.IntSLE

