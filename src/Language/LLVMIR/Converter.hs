{-#LANGUAGE DoAndIfThenElse #-}
module Language.LLVMIR.Converter where
  
import qualified LLVM.FFI.Core as FFI
import LLVM.Core hiding (Value)

import Foreign.C.String

import qualified Language.LLVMIR as LL
import Language.LLVMIR.Util
import Language.LLVMIR.Opcode
import Language.LLVMIR.Predicate

import Control.Monad.Reader

import Debug.Trace

type Type     = FFI.TypeRef
type Value    = FFI.ValueRef 
type TypeKind = FFI.TypeKind

--type Opcode = Int

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

-- | Get Instruction from a LLVM Value
getInstruction :: ReaderT Value IO LL.Instruction
getInstruction = do v <- ask 
                    opcode <- liftIO $ FFI.instGetOpcode v
                   -- oname <- liftIO $ (FFI.instGetOpcodeName v) >>= peekCString
                    let op = toOpcode opcode
                   -- liftIO $ print (oname, opcode, op, opcodeClass opcode)
                    case opcodeClass opcode of
                         Terminator -> getTerminatorOp op 
                         Binary     -> getBinaryOp     op
                         Logical    -> getLogicalOp    op
                         Memory     -> getMemoryOp     op
                         Cast       -> getCastOp       op
                         Other      -> getOtherOp      op

-- | Get Terminator Instruction
getTerminatorOp :: Opcode -> ReaderT Value IO LL.Instruction
getTerminatorOp Ret          = do ival <- ask
                                  n    <- liftIO $ FFI.returnInstGetNumSuccessors ival
                                  mv   <- liftIO $ FFI.returnInstHasReturnValue ival
                                  ops  <- liftIO $ (getOperands ival) >>= mapM getValue
                                  if cInt2Bool mv
                                  then return $ LL.Ret (ops!!0)
                                  else return $ LL.Ret $ LL.NullC LL.TyVoid
getTerminatorOp Br           = do ival <- ask
                                  isCond <- liftIO $ FFI.brInstIsConditional ival
                                  ops    <- liftIO $ (getOperands ival) >>= mapM getValue 
                                  if (cInt2Bool isCond)
                                  then return $ LL.Br (ops!!0) (ops!!2) (ops!!1)
                                  else return $ LL.UBr (ops!!0)
getTerminatorOp Switch       = error $ "TODO switch"
getTerminatorOp IndirectBr   = error $ "TODO indirectbr"
getTerminatorOp Invoke       = error $ "TODO invoke"
getTerminatorOp Resume       = error $ "TODO resume"
getTerminatorOp Unreachable  = return $ LL.Unreachable

-- | Get Standard Binary Instruction
getBinaryOp  :: Opcode -> ReaderT Value IO LL.Instruction
getBinaryOp Add  = binOps LL.Add
getBinaryOp FAdd = binOps LL.FAdd 
getBinaryOp Sub  = binOps LL.Sub  
getBinaryOp FSub = binOps LL.FSub 
getBinaryOp Mul  = binOps LL.Mul  
getBinaryOp FMul = binOps LL.FMul 
getBinaryOp UDiv = binOps LL.UDiv 
getBinaryOp SDiv = binOps LL.SDiv 
getBinaryOp FDiv = binOps LL.FDiv 
getBinaryOp URem = binOps LL.URem 
getBinaryOp SRem = binOps LL.SRem 
getBinaryOp FRem = binOps LL.FRem

-- | Get Logical Instruction
getLogicalOp :: Opcode -> ReaderT Value IO LL.Instruction
getLogicalOp Shl  = bitbinOps LL.Shl
getLogicalOp LShr = bitbinOps LL.LShr
getLogicalOp AShr = bitbinOps LL.AShr
getLogicalOp And  = bitbinOps LL.And 
getLogicalOp Or   = bitbinOps LL.Or  
getLogicalOp Xor  = bitbinOps LL.Xor 

-- | Get Memory Instruction
getMemoryOp  :: Opcode -> ReaderT Value IO LL.Instruction
getMemoryOp Alloca = do ival  <- ask
                        ident <- liftIO $ getIdent ival
                        ty    <- liftIO $ (FFI.allocaGetAllocatedType ival) >>= getType
                        a     <- liftIO $ FFI.allocaGetAlignment ival
                        return $ LL.Alloca (LL.Local ident) ty (LL.Align $ fromIntegral a)
getMemoryOp Load   = do ival  <- ask 
                        ident <- liftIO $ getIdent ival
                        ops   <- liftIO $ (getOperands ival) >>= mapM getValue
                        a     <- liftIO $ FFI.loadGetAlignment ival
                        return $ LL.Load (LL.Local ident) (ops!!0) (LL.Align $ fromIntegral a)
getMemoryOp Store  = do ival  <- ask 
                        ty    <- liftIO $ FFI.typeOf ival >>= getType
                        ops   <- liftIO $ (getOperands ival) >>= mapM getValue
                        align <- liftIO $ FFI.storeGetAlignment ival
                        return $ LL.Store ty (ops!!0) (ops!!1) (LL.Align $ fromIntegral align)
getMemoryOp GetElementPtr = do ival  <- ask
                               ident <- liftIO $ getIdent ival
                               ty    <- liftIO $ FFI.typeOf ival >>= getType
                               ops   <- liftIO $ (getOperands ival) >>= mapM getValue
                               return $ LL.GetElementPtr (LL.Local ident) ty (head ops) (tail ops)
-- atomic operators
getMemoryOp Fence         = error $ "TODO fence"
getMemoryOp AtomicCmpXchg = error $ "TODO atomicCmpXchg"
getMemoryOp AtomicRMW     = error $ "TODO atomicRMW"

-- | Get Cast Instruction
getCastOp    :: Opcode -> ReaderT Value IO LL.Instruction
getCastOp Trunc    = convOps LL.Trunc   
getCastOp ZExt     = convOps LL.ZExt    
getCastOp SExt     = convOps LL.SExt    
getCastOp FPToUI   = convOps LL.FPToUI  
getCastOp FPToSI   = convOps LL.FPToSI  
getCastOp UIToFP   = convOps LL.UIToFP  
getCastOp SIToFP   = convOps LL.SIToFP  
getCastOp FPTrunc  = convOps LL.FPTrunc 
getCastOp FPExt    = convOps LL.FPExt   
getCastOp PtrToInt = convOps LL.PtrToInt
getCastOp IntToPtr = convOps LL.IntToPtr
getCastOp BitCast  = convOps LL.BitCast

-- | Get Other Instruction
getOtherOp   :: Opcode -> ReaderT Value IO LL.Instruction
getOtherOp ICmp = do ival  <- ask
                     ident <- liftIO $ getIdent ival
                     cond  <- liftIO $ FFI.cmpInstGetPredicate ival >>= (return . fromEnum)
                     ty    <- liftIO $ (FFI.typeOf ival) >>= getType
                     (op1,op2) <- liftIO $ getICmpOps ival 
                     return $ LL.ICmp (LL.Local ident) (toIntPredicate cond) ty op1 op2
getOtherOp FCmp = do ival  <- ask
                     ident <- liftIO $ getIdent ival
                     cond  <- liftIO $ FFI.cmpInstGetPredicate ival >>= (return . fromEnum)
                     ty    <- liftIO $ (FFI.typeOf ival) >>= getType
                     (op1,op2) <- liftIO $ getICmpOps ival 
                     return $ LL.FCmp (LL.Local ident) (toRealPredicate cond) ty op1 op2
getOtherOp PHI  = error $ "TODO phi"
getOtherOp Call = do ival  <- ask
                     ident <- liftIO $ getIdent ival
                     ty    <- liftIO $ (FFI.typeOf ival) >>= getType
                     (callee, args) <- liftIO $ (getOperands ival) >>= getCallArgs
                     return $ LL.Call Nothing ty callee args
getOtherOp Select         = error $ "TODO select"
getOtherOp UserOp1        = error $ "TODO userop1"
getOtherOp UserOp2        = error $ "TODO userop2"
getOtherOp VAArg          = error $ "TODO aarg"
getOtherOp ExtractElement = error $ "TODO extractelement"
getOtherOp InsertElement  = error $ "TODO insertelement"
getOtherOp ShuffleVector  = error $ "TODO shufflevector"
getOtherOp ExtractValue   = error $ "TODO extractvalue"
getOtherOp InsertValue    = error $ "TODO insertvalue"
-- exception handling operators
getOtherOp LandingPad     = error $ "TODO landingpad"

--convOps :: (LL.Identifier -> LL.Value -> LL.Type -> b) -> ReaderT Value IO b
convOps c = do ival  <- ask
               ident <- liftIO $ getIdent ival
               ty    <- liftIO $ (FFI.typeOf ival) >>= getType
               ops   <- liftIO $ (getOperands ival) >>= mapM getValue
               if length ops == 0
               then error "'convOps': operand list is empty"
               else return $ c (LL.Local ident) (ops!!0) ty

--binOps :: (LL.Identifier -> LL.Type -> LL.Value -> b) -> ReaderT Value IO b
binOps c = do ival  <- ask
              ident <- liftIO $ getIdent ival
              ty    <- liftIO $ (FFI.typeOf ival) >>= getType
              ops   <- liftIO $ (getOperands ival) >>= mapM getValue
              if length ops /= 2
              then error "'binOps': operand list is broken"
              else return $ c (LL.Local ident) ty (ops!!0) (ops!!1)

--bitbinOps :: (LL.Identifier -> LL.Type -> LL.Value -> b) -> ReaderT Value IO b
bitbinOps c = do ival  <- ask
                 ident <- liftIO $ getIdent ival
                 ty    <- liftIO $ (FFI.typeOf ival) >>= getType
                 ops   <- liftIO $ (getOperands ival) >>= mapM getValue
                 if length ops /= 2
                 then error "'bitbinOps': operand list is broken"
                 else return $ c (LL.Local ident) ty (ops!!0) (ops!!1)

