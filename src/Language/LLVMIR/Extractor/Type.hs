{-#LANGUAGE DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Type
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Type (Type, getType, typeOf,getTypeWithKind) where

import Control.Monad(forM)
import Control.Monad.IO.Class(liftIO)

import Foreign.C.String
import Foreign.Marshal.Array (allocaArray, peekArray)

import qualified LLVM.FFI.Core as FFI
import LLVM.Core hiding (Value)

import qualified Language.LLVMIR as LL
import Language.LLVMIR.Extractor.Util
import Language.LLVMIR.Extractor.Context

import Debug.Trace (trace)

{- TODO
FFI.MetadataTypeKind 
FFI.X86_MMXTypeKind
-}

type Type     = FFI.TypeRef
type TypeKind = FFI.TypeKind

getType :: Type -> Context IO LL.Type
getType ty = do tyk <- liftIO $ FFI.getTypeKind ty
                getTypeWithKind ty tyk

getTypeWithKind :: Type -> TypeKind -> Context IO LL.Type
getTypeWithKind ty FFI.VoidTypeKind      = return LL.TyVoid
getTypeWithKind ty FFI.FloatTypeKind     = return $ LL.TyFloatPoint LL.TyFloat
getTypeWithKind ty FFI.DoubleTypeKind    = return $ LL.TyFloatPoint LL.TyDouble
getTypeWithKind ty FFI.X86_FP80TypeKind  = return $ LL.TyFloatPoint LL.Tyx86FP80
getTypeWithKind ty FFI.FP128TypeKind     = return $ LL.TyFloatPoint LL.TyFP128
getTypeWithKind ty FFI.PPC_FP128TypeKind = return $ LL.TyFloatPoint LL.TyPPCFP128
getTypeWithKind ty FFI.LabelTypeKind     = return LL.TyLabel
getTypeWithKind ty FFI.IntegerTypeKind   = do n <- liftIO $ FFI.getIntTypeWidth ty
                                              return $ LL.TyInt $ fromIntegral n
getTypeWithKind ty FFI.PointerTypeKind   = do et  <- liftIO $ FFI.getElementType ty
                                              etd <- getType et
                                              return $ LL.TyPointer etd
getTypeWithKind ty FFI.ArrayTypeKind     = do n   <- liftIO $ FFI.getArrayLength ty
                                              et  <- liftIO $ FFI.getElementType ty
                                              etd <- getType et
                                              return $ LL.TyArray (fromIntegral n) etd
getTypeWithKind ty FFI.VectorTypeKind    = do n   <- liftIO $ FFI.getVectorSize ty
                                              et  <- liftIO $ FFI.getElementType ty
                                              etd <- getType et
                                              return $ LL.TyVector (fromIntegral n) etd
getTypeWithKind ty FFI.StructTypeKind    = do hnS <- liftIO $ FFI.hasNameStruct ty >>= (return . cInt2Bool) -- * THE ONE TO CHANGE
                                              c <- liftIO $ FFI.countStructElementTypes ty >>= (return . fromIntegral)
                                              if hnS 
                                                 then do n <- liftIO $ FFI.getStructName ty >>= peekCString
                                                         return $ LL.TyStruct n c []
                                                 else do pars <- liftIO $ allocaArray c $ \ args -> do
                                                           FFI.getStructElementTypes ty args
                                                           peekArray c args
                                                         elems <- forM pars getType 
                                                         return $ LL.TyStruct "" c elems
getTypeWithKind ty FFI.FunctionTypeKind  = do retty <- (liftIO $ FFI.getReturnType ty) >>= getType
                                              n     <- liftIO $ FFI.countParamTypes ty >>= (return . fromIntegral)
                                              pars <- liftIO $ allocaArray n $ \ args -> do
                                                        FFI.getParamTypes ty args
                                                        peekArray n args
                                              party <- forM pars getType 
                                              return $ LL.TyFunction party retty
getTypeWithKind ty x  = error $ "'getTypeWithKind': Type " ++ (show x) ++ " not suppported" 

typeOf :: Value -> Context IO LL.Type
typeOf v = do ty <- liftIO $ FFI.typeOf v
              getType ty
