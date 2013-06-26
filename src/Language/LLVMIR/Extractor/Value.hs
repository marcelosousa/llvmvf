{-#LANGUAGE DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Value
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Value where

import qualified LLVM.FFI.Core as FFI
import LLVM.Core hiding (Value,getOperands)

import qualified Language.LLVMIR as LL

import Control.Monad(forM)
import Control.Monad.IO.Class(liftIO)

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array (allocaArray, peekArray)

import Language.LLVMIR.Extractor.Type
import Language.LLVMIR.Extractor.Util
import Language.LLVMIR.Extractor.Ident
import Language.LLVMIR.Extractor.Opcode
import Language.LLVMIR.Extractor.ConstantClass 
import Language.LLVMIR.Extractor.Predicate

import Language.LLVMIR.Extractor.Context

getValue :: (String, Value) -> Context IO LL.Value
getValue (n, v) = do isC <- liftIO $ FFI.isConstant v
                     if cInt2Bool isC
                     then getConstantValue v >>= return . LL.Constant
                     else getIdentValue n v

getIdentValue :: String -> Value -> Context IO LL.Value
getIdentValue n v = do ty <- typeOf v 
                       return $ LL.Id (LL.Local n) ty

-- Constants
getConstantValue :: Value -> Context IO LL.Constant
getConstantValue v = do vc <- liftIO $ FFI.getConstantClass v
                        case toConstantClass vc of
                             BlockAddr              -> getBlockAddr v
                             ConstantAggregateZero  -> getConstantAggregateZero v
                             ConstantArray          -> getConstantArray v
                             ConstantDataSequential -> getConstantDataSequential v >>= return . LL.CmpConst . LL.ConstantDataSequential
                             ConstantExpr           -> getConstantExpr v
                             ConstantFP             -> getConstantFP v >>= return . LL.SmpConst . LL.ConstantFP
                             ConstantInt            -> getConstantInt v
                             ConstantPointerNull    -> getConstantPointerNull v
                             ConstantStruct         -> getConstantStruct v
                             ConstantVector         -> getConstantVector v
                             GlobalValue            -> getGlobalValue v >>= return . LL.GlobalValue
                             UndefValue             -> return LL.UndefValue
                        

getBlockAddr :: Value -> Context IO LL.Constant
getBlockAddr = error "TODO getBlockAddr"

getConstantAggregateZero :: Value -> Context IO LL.Constant
getConstantAggregateZero v = do ty <- typeOf v
                                return $ LL.CmpConst $ LL.ConstantAggregateZero ty 

getConstantArrayElem :: (String,Value) -> Context IO (LL.Type, LL.Value)
getConstantArrayElem elem@(_,v) = do ty  <- typeOf v
                                     val <- getValue elem
                                     return (ty,val)

getConstantArray :: Value -> Context IO LL.Constant
getConstantArray v = do aty  <- liftIO $ FFI.constantArrayGetType v
                        nels <- liftIO $ FFI.arrayTypeGetNumElements aty >>= (return . fromIntegral)
                        eles <- getOperands v >>= mapM getConstantArrayElem
                        let (tys,vals) = unzip eles
                            ty = LL.TyArray nels (head tys)
                        return $ LL.CmpConst $ LL.ConstantArray ty vals

getConstantDataSequential :: Value -> Context IO LL.ConstantDataSequential
getConstantDataSequential v = do vc  <- liftIO $ FFI.getConstantDataSequentialClass v
                                 val <- getConstantDataSequentialData v
                                 num <- liftIO $ FFI.constantValueGetNumElem v >>= (return . fromEnum)
                                 ety <- liftIO $ FFI.constantValueGetElemType v
                                 ty' <- getType ety
                                 case toConstantDataSequentialClass vc of
                                   ConstantDataArray  -> return $ LL.ConstantDataArray  (LL.TyArray  num ty') val 
                                   ConstantDataVector -> return $ LL.ConstantDataVector (LL.TyVector num ty') val

getConstantDataSequentialData :: Value -> Context IO String
getConstantDataSequentialData v = do 
  isCString <- liftIO $ FFI.constantValueIsString v
  if cUInt2Bool isCString
  then liftIO $ FFI.constantValueGetAsString v >>= peekCString
  else do
    val <- liftIO $ FFI.constantValueGetRawDataValues v >>= peekCString
    liftIO $ print $ "getConstantDataSequentialData " ++ val
    return val

getConstantExpr :: Value -> Context IO LL.Constant
getConstantExpr v = do opcode <- liftIO $ FFI.constGetOpcode v 
                       let op = toOpcode opcode
                           expr = case opcodeClass opcode of
                              Terminator -> error $ "'getConstantExpr': Terminator Class" 
                              Binary     -> error $ "'getConstantExpr': TODO Binary" -- return LL.BinaryConstantExpr
                              Logical    -> error $ "'getConstantExpr': TODO Binary" -- return LL.BinaryConstantExpr 
                              Memory     -> case op of
                                GetElementPtr -> getElementPtrConstant v
                                _             -> error $ "'getConstantExpr': Memory Class" 
                              Cast       -> unaryConstantExpr v op  
                              Other      -> case op of
                                ICmp           -> compareConstantExpr v ICmp >>= return . LL.CompareConstantExpr
                                FCmp           -> compareConstantExpr v FCmp >>= return . LL.CompareConstantExpr
                                Select         -> error $ "'getConstantExpr': TODO Select" -- return LL.SelectConstantExpr
                                ExtractElement -> error $ "'getConstantExpr': TODO ExtractE" -- return LL.ExtractElementConstantExpr
                                InsertElement  -> error $ "'getConstantExpr': TODO InsertE" -- return LL.InsertElementConstantExpr
                                ShuffleVector  -> error $ "'getConstantExpr': TODO ShuffleV" -- return LL.ShuffleVectorConstantExpr
                                ExtractValue   -> error $ "'getConstantExpr': TODO ExtractV" -- return LL.ExtractValueConstantExpr
                                InsertValue    -> error $ "'getConstantExpr': TODO InsertV" -- return LL.InsertValueConstantExpr 
                                _              -> error $ "'getConstantExpr': Other Class"
                       expr >>= (return . LL.ConstantExpr) -- $ error "TODO getConstantExpr"


getConstOperands :: Value -> Context IO [Value]
getConstOperands c = do num <- liftIO $ FFI.constGetNumOperands c
                        let oloop instr number total = if number >= total then return [] else do
                                o <- FFI.constGetOperand instr number
                                os <- oloop instr (number + 1) total
                                return (o : os)
                        liftIO $ oloop c 0 num

compareConstantExpr :: Value -> Opcode -> Context IO LL.CompareConstantExpr 
compareConstantExpr v op = do ops <- getOperands v >>= mapM getValue
                              cond <- liftIO $ FFI.constCompareGetPredicate v >>= (return . fromEnum)
                              ty <- typeOf v
                              case op of 
                                ICmp -> return $ LL.ICmpExpr (toIntPredicate cond) ty (ops!!0) (ops!!1)
                                FCmp -> return $ LL.FCmpExpr (toRealPredicate cond) ty (ops!!0) (ops!!1)

unaryConstantExpr :: Value -> Opcode -> Context IO LL.ConstantExpr
unaryConstantExpr v c = do ty  <- typeOf v
                           ops <- getOperands v >>= mapM getValue
                           if length ops == 0
                           then error "'convOps': operand list is empty"
                           else return $ LL.UnaryConstantExpr (show c) (fromEnum c) (ops!!0) ty

-- Seems fine
getElementPtrConstant :: Value -> Context IO LL.ConstantExpr
getElementPtrConstant v = do ty <- typeOf v
                             op <- getOperands v >>= mapM getValue
                             return $ LL.GetElementPtrConstantExpr ty (head op) (tail op)

getConstantFP :: Value -> Context IO LL.ConstantFP
getConstantFP v = do vc <- liftIO $ FFI.getConstantFPClass v
                     ty <- typeOf v
                     case toConstantFPClass vc of
                       FloatValue  -> do val <- liftIO $ FFI.getFPValueFloat v >>= return . realToFrac
                                         return $ LL.ConstantFPFloat val ty 
                       DoubleValue -> do val <- liftIO $ FFI.getFPValueDouble v >>= return . realToFrac 
                                         return $ LL.ConstantFPFloat val ty 

getConstantInt :: Value -> Context IO LL.Constant
getConstantInt v = do ty <- typeOf v 
                      av <- liftIO $ FFI.constIntGetSExtValue v
                      return $ LL.SmpConst $ LL.ConstantInt (fromIntegral av) ty

getConstantPointerNull :: Value -> Context IO LL.Constant
getConstantPointerNull v = do ty <- typeOf v
                              return $ LL.SmpConst $ LL.ConstantPointerNull ty 

-- Seems fine
getConstantStruct :: Value -> Context IO LL.Constant
getConstantStruct v = do aty  <- liftIO $ FFI.constantStructGetType v 
                         n    <- liftIO $ FFI.structTypeGetNumElements aty >>= (return . fromIntegral)
                         vals <- getOperands v >>= mapM getValue
                         pars <- liftIO $ allocaArray n $ \ args -> do
                                   FFI.getStructElementTypes aty args 
                                   peekArray n args
                         elems <- forM pars getType
                         let ty = LL.TyStruct "" n elems
                         return $ LL.CmpConst $ LL.ConstantStruct ty vals

getConstantVector :: Value -> Context IO LL.Constant
getConstantVector = error "TODO getConstantVector"

-- | Retrieves a Constant Global Value
getGlobalValue :: Value -> Context IO LL.GlobalValue
getGlobalValue v = do i  <- getIdent v
                      ty <- typeOf v
                      gb <- liftIO $ FFI.getGlobalValueClass v
                      case toGlobalValueClass gb of
                           FunctionValue  -> return $ LL.FunctionValue (LL.Global i) ty
                           GlobalAlias    -> error "TODO GlobalAlias"
                           GlobalVariable -> return $ LL.GlobalVariable (LL.Global i) ty

