-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Atomic
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Atomic where

import Language.LLVMIR

toAtomicOrdering :: Int -> AtomicOrdering
toAtomicOrdering 0 = NotAtomic
toAtomicOrdering 1 = Unordered
toAtomicOrdering 2 = Monotonic
--toAtomicOrdering 3 = Consume 
toAtomicOrdering 4 = Acquire
toAtomicOrdering 5 = Release
toAtomicOrdering 6 = AcquireRelease
toAtomicOrdering 7 = SequentiallyConsistent

toBinOp :: Int -> BinOp
toBinOp 0 =  OpXchg
toBinOp 1 =  OpAdd
toBinOp 2 =  OpSub
toBinOp 3 =  OpAnd
toBinOp 4 =  OpNand
toBinOp 5 =  OpOr
toBinOp 6 =  OpXor
toBinOp 7 =  OpMax
toBinOp 8 =  OpMin
toBinOp 9 =  OpUMax
toBinOp 10 = OpUMin

