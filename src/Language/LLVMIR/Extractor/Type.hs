{-#LANGUAGE DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Type
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Type (Type, getType, typeOf) where
  
import qualified LLVM.FFI.Core as FFI

import Foreign.C.String

import qualified Language.LLVMIR as LL
import Language.LLVMIR.Extractor.Util

import Debug.Trace (trace)

{- TODO
FFI.FunctionTypeKind
FFI.StructTypeKind
FFI.MetadataTypeKind 
FFI.X86_MMXTypeKind
-}

type Type     = FFI.TypeRef
type TypeKind = FFI.TypeKind

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

typeOf :: Value -> IO LL.Type
typeOf v = (FFI.typeOf v) >>= getType

