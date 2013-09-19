{-# LANGUAGE TypeOperators, UnicodeSyntax, KindSignatures, GADTs, RankNTypes, ImpredicativeTypes, DataKinds, TypeFamilies #-}
-------------------------------------------------------------------------------
-- Module    :  Language.FeatherIR.Base.Syntax
-- Copyright :  (c) 2013 Marcelo Sousa
-- Featherweight Intermediate Representation Base
-------------------------------------------------------------------------------

module Language.FeatherIR.Base.Syntax where

import Language.FeatherIR.Base.Type
import Data.Map (Map)
import qualified Data.Map as Map

type Op = String
type IdGlobal = String
type IdNamedType = String
type Register = String
type BBs = Map Register BB
type Values = [Value]

data Id = GlobalId  IdGlobal
        | Register  Register

data Module = 
	Module 
	{ 
		module_name ∷ String
	  , named_types ∷ Map IdNamedType Type
	  , global_vars ∷ Map IdGlobal    Global
	  , functions   ∷ Map IdGlobal    Function 
	}

newtype Global = Global (Type, Maybe Constant)

newtype Function = Function (Σ, BBs)


newtype Σ = Σ ([(Register,Type)], Type)

-- Basic Block
newtype BB = BB ([Φ], [Instruction], Terminator)

-- φ-instruction
newtype Φ = Φ (Register, Type, [(Value, Register)])

-- instruction
data Instruction =
      Cast   Register Value Type
	| BinOp  Register Type Op Value Value
	| Cmp    Register Type Op Value Value
    | Call   (Maybe Register) Type Id Values  
    | Alloca Register Type Int -- V?
    | Load   Register Type
    | Store  Value Value
    | Gep    Value [Int]

-- terminator
newtype Terminator = Return Values

-- value
data Value = 
	  Local Register Type
	| Constant Constant

-- constant
data Constant = 
	  GlobalConstant  		 IdGlobal
	| ConstantInt            Int Type
    | ConstantFP             Type
    | ConstantPointerNull    Type
	| ConstantAggregate      Type Values
    | ConstantDataSequential Type String	  
