module Language.LLVMIR.ConstantClass where

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

