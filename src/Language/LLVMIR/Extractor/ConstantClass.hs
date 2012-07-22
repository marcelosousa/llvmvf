-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.ConstantClass
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.ConstantClass where

import Foreign.C.Types

data ConstantClass =
    BlockAddr
  | ConstantAggregateZero
  | ConstantArray
  | ConstantDataSequential
  | ConstantExpr
  | ConstantFP
  | ConstantInt
  | ConstantPointerNull
  | ConstantStruct
  | ConstantVector
  | GlobalValue
  | UndefValue
  deriving (Show,Eq,Enum,Ord)

data ConstantDataSequentialClass =
    ConstantDataArray
  | ConstantDataVector
  deriving (Show,Eq,Enum,Ord)

data GlobalValueClass = 
    FunctionValue
  | GlobalAlias
  | GlobalVariable
  deriving (Show,Eq,Enum,Ord)

toConstantClass :: CUInt -> ConstantClass
toConstantClass = toEnum . fromIntegral 

toGlobalValueClass :: CUInt -> GlobalValueClass
toGlobalValueClass = toEnum . fromIntegral 

toConstantDataSequentialClass :: CUInt -> ConstantDataSequentialClass
toConstantDataSequentialClass = toEnum . fromIntegral 

