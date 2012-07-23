{-#LANGUAGE DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Type
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Type (Type, getType, typeOf) where

import Control.Monad(forM)
import Foreign.C.String
import Foreign.Marshal.Array (allocaArray, peekArray)

import qualified LLVM.FFI.Core as FFI
import LLVM.Core hiding (Value)

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
                                              n <- FFI.countStructElementTypes ty >>= (return . fromIntegral)
                                              pars <- allocaArray n $ \ args -> do
                                                        FFI.getStructElementTypes ty args
                                                        peekArray n args
                                              elems <- forM pars getType 
                                              return $ LL.TyStruct s n elems
getTypeWithKind ty FFI.FunctionTypeKind  = do retty <- (FFI.getReturnType ty) >>= getType
                                              n     <- FFI.countParamTypes ty >>= (return . fromIntegral)
                                              pars <- allocaArray n $ \ args -> do
                                                        FFI.getParamTypes ty args
                                                        peekArray n args
                                              party <- forM pars getType 
                                              return $ LL.TyFunction party retty
getTypeWithKind ty x  = error $ "'getTypeWithKind': Type " ++ (show x) ++ " not suppported" -- trace (show x) $ return LL.TyUnsupported

typeOf :: Value -> IO LL.Type
typeOf v = (FFI.typeOf v) >>= getType

