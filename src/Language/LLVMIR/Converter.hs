{-#LANGUAGE DoAndIfThenElse #-}
module Language.LLVMIR.Converter where
  
import qualified LLVM.FFI.Core as FFI
import LLVM.Core hiding (Value)

import Foreign.C.String

import qualified Language.LLVMIR as LL
import Language.LLVMIR.Util
import Debug.Trace

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
getTypeWithKind ty x  = trace (show x) $ return LL.TyUnsupported

getInst :: Value -> IO LL.Instruction
getInst v = do opcode <- FFI.instGetOpcode v
               --oname <- (FFI.instGetOpcodeName v) >>= peekCString
               --print (oname, opcode)
               pInstruction v opcode

getValue :: (String, Value) -> IO LL.Value
getValue (n, v) = do isC <- (FFI.isConstant v)
                     if cInt2Bool isC
                     then getConstantValue v
                     else getIdentValue n v 

getIdent :: Value -> IO String
getIdent v = (FFI.getValueName v) >>= peekCString

getIdentValue :: String -> Value -> IO LL.Value
getIdentValue n v = do ty <- (FFI.typeOf v) >>= getType
                       return $ LL.Id (LL.Local n) ty

getCallArgs :: [(String,Value)] -> IO (LL.Identifier, [LL.Value])
getCallArgs [] = return (LL.Global "", [])
getCallArgs l = let x = last l
                    y = init l
                in do v <- getIdent $ snd x
                      a <- mapM getValue y
                      return (LL.Global v,a)

getElemPtrArgs :: [(String,Value)] -> IO (LL.Value, [LL.Value])
getElemPtrArgs [] = return (LL.UndefC, [])
getElemPtrArgs (x:y) = do v <- uncurry getIdentValue x
                          a <- mapM getValue y
                          return (v,a)

getConstantValue :: Value -> IO LL.Value
getConstantValue v = do ty <- (FFI.typeOf v) >>= getType
                        case ty of
                             LL.TyInt _     -> do av <- FFI.constIntGetSExtValue v
                                                  return $ LL.IntC (fromIntegral av) ty
                             LL.TyPointer _ -> do (struct, args) <- (getOperands v) >>= getElemPtrArgs
                                                  return $ LL.Pointer ty struct args 
                             _              -> do return $ LL.UndefC 

getICmpOps :: Value -> IO (LL.Value, LL.Value)
getICmpOps v = do ops <- (getOperands v) >>= mapM getValue
                  if (length ops == 2)
                  then return (head ops, last ops)
                  else error "icmp ops length is != 2"

pInstruction :: Value -> Opcode -> IO LL.Instruction
pInstruction ival 1  = do n <- FFI.returnInstGetNumSuccessors ival
                          mv <- FFI.returnInstHasReturnValue ival
                          ops <- (getOperands ival) >>= mapM getValue
                          if cInt2Bool mv
                          then return $ LL.Ret (ops!!0)
                          else return $ LL.Ret $ LL.NullC LL.TyVoid
pInstruction ival 2  = do isCond <- FFI.brInstIsConditional ival
                          ops    <- (getOperands ival) >>= mapM getValue 
                          if (cInt2Bool isCond)
                          then return $ LL.Br (ops!!0) (ops!!2) (ops!!1)
                          else return $ LL.UBr (ops!!0)
pInstruction ival 3  = return $ LL.Instruction "switch"
pInstruction ival 4  = return $ LL.Instruction "indirectbr"
pInstruction ival 5  = return $ LL.Instruction "invoke"
pInstruction ival 7  = return $ LL.Unreachable
-- standard binary operators
pInstruction ival 8  = binOps ival LL.Add
pInstruction ival 9  = binOps ival LL.FAdd 
pInstruction ival 10 = binOps ival LL.Sub  
pInstruction ival 11 = binOps ival LL.FSub 
pInstruction ival 12 = binOps ival LL.Mul  
pInstruction ival 13 = binOps ival LL.FMul 
pInstruction ival 14 = binOps ival LL.UDiv 
pInstruction ival 15 = binOps ival LL.SDiv 
pInstruction ival 16 = binOps ival LL.FDiv 
pInstruction ival 17 = binOps ival LL.URem 
pInstruction ival 18 = binOps ival LL.SRem 
pInstruction ival 19 = binOps ival LL.FRem 
-- logical operators
pInstruction ival 20 = bitbinOps ival LL.Shl
pInstruction ival 21 = bitbinOps ival LL.LShr
pInstruction ival 22 = bitbinOps ival LL.AShr
pInstruction ival 23 = bitbinOps ival LL.And 
pInstruction ival 24 = bitbinOps ival LL.Or  
pInstruction ival 25 = bitbinOps ival LL.Xor 
-- memory operators
pInstruction ival 26 = do ident <- getIdent ival
                          ty <- (FFI.allocaGetAllocatedType ival) >>= getType
                          a  <- FFI.allocaGetAlignment ival
                          return $ LL.Alloca (LL.Local ident) ty (LL.Align $ fromIntegral a)
pInstruction ival 27 = do ident <- getIdent ival
                          ops <- (getOperands ival) >>= mapM getValue 
                          a <- FFI.loadGetAlignment ival
                          return $ LL.Load (LL.Local ident) (ops!!0) (LL.Align $ fromIntegral a)
pInstruction ival 28 = do ty <- FFI.typeOf ival >>= getType
                          ops <- (getOperands ival) >>= mapM getValue
                          a <- FFI.storeGetAlignment ival
                          return $ LL.Store ty (ops!!0) (ops!!1) (LL.Align $ fromIntegral a)
pInstruction ival 29 = do ident <- getIdent ival
                          ty <- FFI.typeOf ival >>= getType
                          ops <- (getOperands ival) >>= mapM getValue  
                          return $ LL.GetElementPtr (LL.Local ident) ty (head ops) (tail ops)
-- atomic operators
pInstruction ival 30 = return $ LL.Instruction "fence"
pInstruction ival 31 = return $ LL.Instruction "atomicCmpXchg"
pInstruction ival 32 = return $ LL.Instruction "atomicRMW"
-- cast operators
pInstruction ival 33 = convOps ival LL.Trunc   
pInstruction ival 34 = convOps ival LL.ZExt    
pInstruction ival 35 = convOps ival LL.SExt    
pInstruction ival 36 = convOps ival LL.FPToUI  
pInstruction ival 37 = convOps ival LL.FPToSI  
pInstruction ival 38 = convOps ival LL.UIToFP  
pInstruction ival 39 = convOps ival LL.SIToFP  
pInstruction ival 40 = convOps ival LL.FPTrunc 
pInstruction ival 41 = convOps ival LL.FPExt   
pInstruction ival 42 = convOps ival LL.PtrToInt
pInstruction ival 43 = convOps ival LL.IntToPtr
pInstruction ival 44 = convOps ival LL.BitCast
-- other operators
pInstruction ival 45 = do ident <- getIdent ival
                          cond  <- FFI.cmpInstGetPredicate ival >>= (return . fromEnum)
                          ty    <- (FFI.typeOf ival) >>= getType
                          (op1,op2) <- getICmpOps ival 
                          return $ LL.ICmp (LL.Local ident) (toIntPredicate cond) ty op1 op2
pInstruction ival 46 = do ident <- getIdent ival
                          cond  <- FFI.cmpInstGetPredicate ival >>= (return . fromEnum)
                          ty    <- (FFI.typeOf ival) >>= getType
                          (op1,op2) <- getICmpOps ival 
                          return $ LL.FCmp (LL.Local ident) (toRealPredicate cond) ty op1 op2
pInstruction ival 47 = return $ LL.Instruction "phi"
pInstruction ival 48 = do ident <- getIdent ival
                          ty <- (FFI.typeOf ival) >>= getType
                          (callee, args) <- (getOperands ival) >>= getCallArgs
                          return $ LL.Call Nothing ty callee args
pInstruction ival 49 = return $ LL.Instruction "select"
pInstruction ival 50 = return $ LL.Instruction "userop1"
pInstruction ival 51 = return $ LL.Instruction "userop2"
pInstruction ival 52 = return $ LL.Instruction "aarg"
pInstruction ival 53 = return $ LL.Instruction "extractelement"
pInstruction ival 54 = return $ LL.Instruction "insertelement"
pInstruction ival 55 = return $ LL.Instruction "shufflevector"
pInstruction ival 56 = return $ LL.Instruction "extractvalue"
pInstruction ival 57 = return $ LL.Instruction "insertvalue"
-- exception handling operators
pInstruction ival 6  = return $ LL.Instruction "resume"
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

toRealPredicate :: Int -> LL.RealPredicate
toRealPredicate 0 = LL.LLVMRealPredicateFalse
toRealPredicate 1 = LL.LLVMRealOEQ           
toRealPredicate 2 = LL.LLVMRealOGT           
toRealPredicate 3 = LL.LLVMRealOGE           
toRealPredicate 4 = LL.LLVMRealOLT           
toRealPredicate 5 = LL.LLVMRealOLE           
toRealPredicate 6 = LL.LLVMRealONE           
toRealPredicate 7 = LL.LLVMRealORD           
toRealPredicate 8 = LL.LLVMRealUNO           
toRealPredicate 9 = LL.LLVMRealUEQ           
toRealPredicate 10 = LL.LLVMRealUGT           
toRealPredicate 11 = LL.LLVMRealUGE           
toRealPredicate 12 = LL.LLVMRealULT           
toRealPredicate 13 = LL.LLVMRealULE           
toRealPredicate 14 = LL.LLVMRealUNE           
toRealPredicate 15 = LL.LLVMRealPredicateTrue 

convOps :: Value -> (LL.Identifier -> LL.Value -> LL.Type -> b) -> IO b
convOps ival c = do ident <- getIdent ival
                    ty <- (FFI.typeOf ival) >>= getType
                    ops <- (getOperands ival) >>= mapM getValue
                    if length ops == 0
                    then error "'convOps': operand list is empty"
                    else return $ c (LL.Local ident) (ops!!0) ty

binOps ival c = do ident <- getIdent ival
                   ty <- (FFI.typeOf ival) >>= getType
                   ops <- (getOperands ival) >>= mapM getValue
                   if length ops /= 2
                   then error "'binOps': operand list is broken"
                   else return $ c (LL.Local ident) ty (ops!!0) (ops!!1)

bitbinOps ival c = do ident <- getIdent ival
                      ty <- (FFI.typeOf ival) >>= getType
                      ops <- (getOperands ival) >>= mapM getValue
                      if length ops /= 2
                      then error "'bitbinOps': operand list is broken"
                      else return $ c (LL.Local ident) ty (ops!!0) (ops!!1)

