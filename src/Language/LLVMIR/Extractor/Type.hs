{-#LANGUAGE DoAndIfThenElse, RecordWildCards #-}
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

import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace (trace)

{- TODO
FFI.MetadataTypeKind 
FFI.X86_MMXTypeKind
-}

type Type     = FFI.TypeRef
type TypeKind = FFI.TypeKind

typeOf :: Value -> Context IO LL.Type
typeOf v = do ty <- liftIO $ FFI.typeOf v
              getType ty

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
getTypeWithKind ty FFI.StructTypeKind    = getStructType ty
getTypeWithKind ty FFI.FunctionTypeKind  = do retty <- (liftIO $ FFI.getReturnType ty) >>= getType
                                              n     <- liftIO $ FFI.countParamTypes ty >>= return . fromIntegral
                                              pars <- liftIO $ allocaArray n $ \ args -> do
                                                        FFI.getParamTypes ty args
                                                        peekArray n args
                                              party <- forM pars getType 
                                              return $ LL.TyFunction party retty
getTypeWithKind ty x  = error $ "'getTypeWithKind': Type " ++ (show x) ++ " not suppported" 

{-
getTypeName :: Type -> Context IO (Maybe String)
getTypeName ty = do tyk <- liftIO $ FFI.getTypeKind ty
                    getTypeNameWithKind ty tyk
-}

getTypeNameWithKind :: Type -> TypeKind -> Context IO (Maybe String)
getTypeNameWithKind ty FFI.StructTypeKind = do hasName <- liftIO $ FFI.hasNameStruct ty >>= return . cInt2Bool
                                               if hasName
                                               then liftIO $ FFI.getStructName ty >>= peekCString >>= return . Just
                                               else return Nothing
getTypeNameWithKind ty _                  = return Nothing

getStructTypeParams :: Type -> Int -> IO [Type]
getStructTypeParams ty c = allocaArray c $ \ args -> do
                             FFI.getStructElementTypes ty args
                             peekArray c args 

getStructType :: Type -> Context IO LL.Type
getStructType ty = do e@Env{..} <- getEnv
                      do c       <- liftIO $ FFI.countStructElementTypes ty >>= return . fromIntegral
                         hasName <- getTypeNameWithKind ty FFI.StructTypeKind
                         pars <- liftIO $ getStructTypeParams ty c
                         case hasName of
                           Nothing   -> do elems <- forM pars getType 
                                           return $ LL.TyStruct "" c elems           
                           Just name -> case Map.lookup name nmdtys of
                                          Nothing -> do let nmdtys' = Map.insert name LL.TyUndefined nmdtys
                                                        putEnv e{nmdtys = nmdtys'}
                                                        pars' <- forM pars getType
                                                        e'@Env{..} <- getEnv
                                                        let sTy = LL.TyStruct name c pars'
                                                            fnmdtys = Map.adjust (const sTy) name nmdtys
                                                        putEnv e'{nmdtys = fnmdtys}
                                                        return $ LL.TyStruct name c []  --return sTy
                                          Just t  -> return $ LL.TyStruct name c []     --return t
