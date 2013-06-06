-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Opcode
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Opcode where

data Opcode = 
    Ret        
  | Br         
  | Switch     
  | IndirectBr 
  | Invoke     
  | Resume     
  | Unreachable
  | Add  
  | FAdd 
  | Sub  
  | FSub 
  | Mul  
  | FMul 
  | UDiv 
  | SDiv 
  | FDiv 
  | URem 
  | SRem 
  | FRem 
  | Shl 
  | LShr
  | AShr
  | And 
  | Or  
  | Xor
  | Alloca
  | Load  
  | Store 
  | GetElementPtr
  | Fence 
  | AtomicCmpXchg
  | AtomicRMW 
  | Trunc   
  | ZExt    
  | SExt    
  | FPToUI  
  | FPToSI  
  | UIToFP  
  | SIToFP  
  | FPTrunc 
  | FPExt   
  | PtrToInt
  | IntToPtr
  | BitCast
  | ICmp   
  | FCmp 
  | Phi      
  | Call   
  | Select 
  | UserOp1
  | UserOp2
  | VAArg
  | ExtractElement
  | InsertElement
  | ShuffleVector
  | ExtractValue
  | InsertValue 
  | LandingPad
  deriving (Show, Eq, Ord, Enum, Read)

toOpcode :: Int -> Opcode
toOpcode x = toEnum (x-1)

data InstClass = Terminator
               | Binary
               | Logical
               | Memory
               | Cast
               | Other
               | PHI
   deriving (Show, Eq, Ord, Enum, Read)

(<>) :: (Ord a) => a -> (a,a) -> Bool
x <> (l,u) = x >= l && x <= u

opcodeClass :: Int -> InstClass
opcodeClass x | x <> (1,7)   = Terminator
              | x <> (8,19)  = Binary
              | x <> (20,25) = Logical
              | x <> (26,32) = Memory
              | x <> (33,44) = Cast
              | x == 47      = PHI
              | x <> (45, 46) || x <> (48, 58) = Other
              | otherwise    = error "'opcodeClass': No class for opcode" 
