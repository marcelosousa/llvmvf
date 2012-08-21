{-#LANGUAGE DoAndIfThenElse, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Instruction
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Instruction where
  
import qualified LLVM.FFI.Core as FFI
import LLVM.Core hiding (Value, getOperands)

import Control.Monad(forM) 
                     
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.C.String

import qualified Language.LLVMIR as LL

import Data.Maybe (fromMaybe)

import Language.LLVMIR.Extractor.Util
import Language.LLVMIR.Extractor.Opcode
import Language.LLVMIR.Extractor.Predicate
import Language.LLVMIR.Extractor.Value
import Language.LLVMIR.Extractor.Ident
import Language.LLVMIR.Extractor.Type
import Language.LLVMIR.Extractor.Context
import Language.LLVMIR.Extractor.Atomic

import Control.Monad.IO.Class (liftIO)

getPHIArgs :: Value -> Context IO [(LL.Value, LL.Value)]
getPHIArgs ii = do num <- liftIO $ FFI.countIncoming ii
                   oloop ii 0 num
  where oloop instr number total = if number >= total 
                                   then return [] 
                                   else do rv <- liftIO $ FFI.getIncomingValue instr number
                                           vn <- getIdent rv
                                           v  <- getValue(vn,rv)
                                           rb <- liftIO $ FFI.getIncomingBlock instr number 
                                           bn <- getIdent rb
                                           b  <- getValue(bn,rb)
                                           os <- oloop instr (number + 1) total
                                           return ((v,b) : os)

-- | Retrieve Arguments to Call Instruction
getCallArgs :: [(String,Value)] -> Context IO (LL.Id, [LL.Value])
getCallArgs [] = error "'getCallArgs': empty list" -- return (LL.Global "", [])
getCallArgs l = let x = last l
                    y = init l
                in do v <- getIdent $ snd x
                      a <- mapM getValue y
                      return (v,a)

getElemPtrArgs :: [(String,Value)] -> Context IO (LL.Value, [LL.Value])
getElemPtrArgs [] = error "'getElemPtrArgs': empty list" -- return (LL.UndefC, [])
getElemPtrArgs (x:y) = do v <- uncurry getIdentValue x
                          a <- mapM getValue y
                          return (v,a)

getICmpOps :: Value -> Context IO (LL.Value, LL.Value)
getICmpOps v = do ops <- getOperands v >>= mapM getValue
                  if length ops == 2
                  then return (head ops, last ops)
                  else error "icmp ops length is != 2"

getInstructionValue :: Context IO Value
getInstructionValue = do e@Env{..} <- getEnv
                         return $ fromMaybe (error "'getInstructionValue': No Value") instr

getPC :: Context IO LL.PC
getPC = do e@Env{..} <- getEnv
           let opc = pc
           putEnv $ e {pc = opc + 1}
           return opc

-- | Get Instruction from a LLVM Value
getInstruction :: Context IO LL.Instruction
getInstruction = do v <- getInstructionValue 
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
getTerminatorOp :: Opcode -> Context IO LL.Instruction
getTerminatorOp Ret          = do ival <- getInstructionValue
                                  pc   <- getPC
                                  n    <- liftIO $ FFI.returnInstGetNumSuccessors ival
                                  mv   <- liftIO $ FFI.returnInstHasReturnValue ival
                                  ops  <- getOperands ival >>= mapM getValue
                                  if cInt2Bool mv
                                  then return $ LL.Ret pc $ LL.ValueRet (ops!!0)
                                  else return $ LL.Ret pc LL.VoidRet 
getTerminatorOp Br           = do ival <- getInstructionValue
                                  pc   <- getPC
                                  isCond <- liftIO $ FFI.brInstIsConditional ival
                                  ops    <- getOperands ival >>= mapM getValue 
                                  if (cInt2Bool isCond)
                                  then return $ LL.Br  pc (ops!!0) (ops!!2) (ops!!1)
                                  else return $ LL.UBr pc (ops!!0)
getTerminatorOp Switch       = error $ "TODO switch"
getTerminatorOp IndirectBr   = error $ "TODO indirectbr"
getTerminatorOp Invoke       = error $ "TODO invoke"
getTerminatorOp Resume       = error $ "TODO resume"
getTerminatorOp Unreachable  = do pc <- getPC
                                  return $ LL.Unreachable pc

-- | Get Standard Binary Instruction
getBinaryOp  :: Opcode -> Context IO LL.Instruction
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
getLogicalOp :: Opcode -> Context IO LL.Instruction
getLogicalOp Shl  = bitbinOps LL.Shl
getLogicalOp LShr = bitbinOps LL.LShr
getLogicalOp AShr = bitbinOps LL.AShr
getLogicalOp And  = bitbinOps LL.And 
getLogicalOp Or   = bitbinOps LL.Or  
getLogicalOp Xor  = bitbinOps LL.Xor 

-- | Get Memory Instruction
getMemoryOp  :: Opcode -> Context IO LL.Instruction
getMemoryOp Alloca = do ival  <- getInstructionValue
                        pc    <- getPC
                        ident <- getIdent ival
                        ty    <- (liftIO $ FFI.allocaGetAllocatedType ival) >>= getType
                        a     <- liftIO $ FFI.allocaGetAlignment ival
                        return $ LL.Alloca pc (LL.Local ident) ty (LL.Align $ fromIntegral a)
getMemoryOp Load   = do ival  <- getInstructionValue 
                        pc    <- getPC
                        ident <- getIdent ival
                        ops   <- getOperands ival >>= mapM getValue
                        a     <- liftIO $ FFI.loadGetAlignment ival
                        return $ LL.Load pc (LL.Local ident) (ops!!0) (LL.Align $ fromIntegral a)
getMemoryOp Store  = do ival  <- getInstructionValue 
                        pc    <- getPC
                        ty    <- typeOf ival 
                        ops   <- getOperands ival >>= mapM getValue
                        align <- liftIO $ FFI.storeGetAlignment ival
                        return $ LL.Store pc ty (ops!!0) (ops!!1) (LL.Align $ fromIntegral align)
getMemoryOp GetElementPtr = do ival  <- getInstructionValue
                               pc    <- getPC
                               ident <- getIdent ival
                               ty    <- typeOf ival 
                               ops   <- getOperands ival >>= mapM getValue
                               return $ LL.GetElementPtr pc (LL.Local ident) ty (head ops) (tail ops)
-- atomic operators
getMemoryOp Fence         = error $ "TODO fence"
getMemoryOp AtomicCmpXchg = error $ "TODO atomicCmpXchg"
getMemoryOp AtomicRMW     = do ival  <- getInstructionValue
                               pc    <- getPC
                               ident <- getIdent ival
                               ops   <- getOperands ival >>= mapM getValue
                               op    <- liftIO $ FFI.atomicRMWGetOperation ival >>= return . fromIntegral
                               ord   <- liftIO $ FFI.atomicRMWGetOrdering ival >>= return . fromIntegral
                               return $ LL.AtomicRMW pc (LL.Local ident) ops (toBinOp op) (toAtomicOrdering ord)
-- error $ "TODO atomicRMW"

-- | Get Cast Instruction
getCastOp    :: Opcode -> Context IO LL.Instruction
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
getOtherOp   :: Opcode -> Context IO LL.Instruction
getOtherOp ICmp = do ival  <- getInstructionValue
                     pc    <- getPC
                     ident <- getIdent ival
                     cond  <- liftIO $ FFI.cmpInstGetPredicate ival >>= (return . fromEnum)
                     ty    <- typeOf ival 
                     (op1,op2) <- getICmpOps ival 
                     return $ LL.ICmp pc (LL.Local ident) (toIntPredicate cond) ty op1 op2
getOtherOp FCmp = do ival  <- getInstructionValue
                     pc    <- getPC
                     ident <- getIdent ival
                     cond  <- liftIO $ FFI.cmpInstGetPredicate ival >>= (return . fromEnum)
                     ty    <- typeOf ival 
                     (op1,op2) <- getICmpOps ival 
                     return $ LL.FCmp pc (LL.Local ident) (toRealPredicate cond) ty op1 op2
getOtherOp PHI  = do ival  <- getInstructionValue
                     pc    <- getPC
                     ident <- getIdent ival
                     ty    <- typeOf ival 
                     args  <- getPHIArgs ival
                     return $ LL.PHI pc (LL.Local ident) ty args
getOtherOp Call = do ival  <- getInstructionValue
                     pc    <- getPC
                     ident <- getIdent ival
                     ty    <- typeOf ival 
                     (callee, args) <- getOperands ival >>= getCallArgs
                     return $ LL.Call pc (LL.Local ident) ty callee args
getOtherOp Select         = do ival <- getInstructionValue
                               pc   <- getPC
                               ident <- getIdent ival
                               cond  <- (liftIO $ FFI.selectGetCondition ival)  >>= \i -> getValue (ident,i)
                               valt  <- (liftIO $ FFI.selectGetTrueValue ival)  >>= \i -> getValue (ident,i)
                               valf  <- (liftIO $ FFI.selectGetFalseValue ival) >>= \i -> getValue (ident,i)
                               return $ LL.Select pc (LL.Local ident) cond valt valf
getOtherOp UserOp1        = error $ "TODO userop1"
getOtherOp UserOp2        = error $ "TODO userop2"
getOtherOp VAArg          = error $ "TODO aarg"
getOtherOp ExtractElement = error $ "TODO extractelement"                              
getOtherOp InsertElement  = error $ "TODO insertelement"
getOtherOp ShuffleVector  = error $ "TODO shufflevector"
getOtherOp ExtractValue   = do ival  <- getInstructionValue
                               pc    <- getPC
                               ident <- getIdent ival
                               ops   <- getOperands ival >>= mapM getValue
                               n     <- liftIO $ FFI.extractValueGetNumIndices ival >>= return . fromIntegral
                               idxs' <- liftIO $ allocaArray n $ \ args -> do
                                           FFI.extractValueGetIndices ival args
                                           peekArray n args
                               idxs <- forM idxs' (return . fromIntegral)
                               return $ LL.ExtractValue pc (LL.Local ident) (ops!!0) idxs 
getOtherOp InsertValue    = error $ "TODO insertvalue"
-- exception handling operators
getOtherOp LandingPad     = error $ "TODO landingpad"

--convOps :: (LL.Identifier -> LL.Value -> LL.Type -> b) -> Context IO b
convOps c = do ival  <- getInstructionValue
               pc    <- getPC
               ident <- getIdent ival
               ty    <- typeOf ival
               ops   <- getOperands ival >>= mapM getValue
               if length ops == 0
               then error "'convOps': operand list is empty"
               else return $ c pc (LL.Local ident) (ops!!0) ty

--binOps :: (LL.Identifier -> LL.Type -> LL.Value -> b) -> Context IO b
binOps c = do ival  <- getInstructionValue
              pc    <- getPC
              ident <- getIdent ival
              ty    <- typeOf ival 
              ops   <- getOperands ival >>= mapM getValue
              if length ops /= 2
              then error "'binOps': operand list is broken"
              else return $ c pc (LL.Local ident) ty (ops!!0) (ops!!1)

--bitbinOps :: (LL.Identifier -> LL.Type -> LL.Value -> b) -> Context IO b
bitbinOps c = do ival  <- getInstructionValue
                 pc    <- getPC
                 ident <- getIdent ival
                 ty    <- typeOf ival 
                 ops   <- getOperands ival >>= mapM getValue
                 if length ops /= 2
                 then error "'bitbinOps': operand list is broken"
                 else return $ c pc (LL.Local ident) ty (ops!!0) (ops!!1)
