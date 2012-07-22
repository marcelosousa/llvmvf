-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Predicate
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Predicate where

import Language.LLVMIR

toIntPredicate :: Int -> IntPredicate
toIntPredicate 32 = IntEQ
toIntPredicate 33 = IntNE
toIntPredicate 34 = IntUGT
toIntPredicate 35 = IntUGE
toIntPredicate 36 = IntULT
toIntPredicate 37 = IntULE
toIntPredicate 38 = IntSGT
toIntPredicate 39 = IntSGE
toIntPredicate 40 = IntSLT
toIntPredicate 41 = IntSLE

toRealPredicate :: Int -> RealPredicate
toRealPredicate 0 = LLVMRealPredicateFalse
toRealPredicate 1 = LLVMRealOEQ
toRealPredicate 2 = LLVMRealOGT
toRealPredicate 3 = LLVMRealOGE
toRealPredicate 4 = LLVMRealOLT
toRealPredicate 5 = LLVMRealOLE
toRealPredicate 6 = LLVMRealONE
toRealPredicate 7 = LLVMRealORD
toRealPredicate 8 = LLVMRealUNO
toRealPredicate 9 = LLVMRealUEQ
toRealPredicate 10 = LLVMRealUGT
toRealPredicate 11 = LLVMRealUGE
toRealPredicate 12 = LLVMRealULT
toRealPredicate 13 = LLVMRealULE
toRealPredicate 14 = LLVMRealUNE
toRealPredicate 15 = LLVMRealPredicateTrue
