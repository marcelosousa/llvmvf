{-#LANGUAGE DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Instruction
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Instruction where
  
import qualified LLVM.FFI.Core as FFI
import LLVM.Core hiding (Value)

import Foreign.C.String

import qualified Language.LLVMIR as LL

import Language.LLVMIR.Extractor.Util
import Language.LLVMIR.Extractor.Opcode
import Language.LLVMIR.Extractor.Predicate
import Language.LLVMIR.Extractor.Value
import Language.LLVMIR.Extractor.Ident
import Language.LLVMIR.Extractor.Type

import Control.Monad.Reader

getPHIArgs :: Value -> IO [(LL.Value, LL.Value)]
getPHIArgs ii = do num <- FFI.countIncoming ii
                   oloop ii 0 num
  where oloop instr number total = if number >= total 
                                   then return [] 
                                   else do rv <- FFI.getIncomingValue instr number
                                           vn <- getIdent rv
                                           v  <- getValue(vn,rv)
                                           rb <- FFI.getIncomingBlock instr number 
                                           bn <- getIdent rb
                                           b  <- getValue(bn,rb)
                                           os <- oloop instr (number + 1) total
                                           return ((v,b) : os)

-- | Retrieve Arguments to Call Instruction
getCallArgs :: [(String,Value)] -> IO (LL.Identifier, [LL.Value])
getCallArgs [] = return (LL.Global "", [])
getCallArgs l = let x = last l
                    y = init l
                in do v <- getIdent $ snd x
                      a <- mapM getValue y
                      return (LL.Global v,a)

getElemPtrArgs :: [(String,Value)] -> IO (LL.Value, [LL.Value])
getElemPtrArgs [] = error "'getElemPtrArgs': empty list" -- return (LL.UndefC, [])
getElemPtrArgs (x:y) = do v <- uncurry getIdentValue x
                          a <- mapM getValue y
                          return (v,a)

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
                                  else return $ LL.Ret $ LL.Constant LL.UndefValue 
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
getOtherOp PHI  = do ival  <- ask
                     ident <- liftIO $ getIdent ival
                     ty    <- liftIO $ (FFI.typeOf ival) >>= getType
                     args  <- liftIO $ getPHIArgs ival
                     return $ LL.PHI (LL.Local ident) ty args
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

