{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Initial
-- Copyright :  (c) 2013 Marcelo Sousa
-- Initial Type Constraints
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Initial (iτℂ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Language.LLVMIR hiding (Type(..),Id, NamedTypes)
import Analysis.Type.Inference.Base
import Analysis.Type.Memory.TyAnn

cFn ∷ ℂ → ℂ
cFn λτ = ℂp λτ AnyAddr

cPtr ∷ Τα → Ταρ → ℂ
cPtr τ τα = ℂτ $ TyDer $ TyPtr τ τα

cI ∷ Int → ℂ
cI x = ℂτ $ i x

cVoid ∷ ℂ
cVoid = ℂτ $ TyPri TyVoid 

(=:) ∷ String → ℂ → Τℂ
name =: τℂ = 
  let nℂ = ℂπ (Global name)
  in  nℂ :=: τℂ

copy_from_user_N n = cFn $ ℂλ [cPtr (i 8) KernelAddr, cPtr (i 8) UserAddr, cI 32] (cI n)
copy_to_user_N n = cFn $ ℂλ [cPtr (i 8) UserAddr, cPtr (i 8) KernelAddr, cI 32] (cI n)
strncpy_from_user = cFn $ ℂλ [cPtr (i 8) KernelAddr, cPtr (i 8) UserAddr, cI 64] (cI 64)
x_user = cFn $ ℂλ [cPtr (i 8) UserAddr, cI 64] (cI 64)
memcmp = cFn $ ℂλ [cPtr (i 8) KernelAddr, cPtr (i 8) KernelAddr, cI 64] (cI 32)
memcpy = cFn $ ℂλ [cPtr (i 8) KernelAddr, cPtr (i 8) KernelAddr, cI 64] (cPtr (i 8) KernelAddr)
llvmmemcpy n = cFn $ ℂλ [cPtr (i 8) KernelAddr, cPtr (i 8) KernelAddr, cI n, cI 32, cI 1] cVoid
wil_memcpy_x = cFn $ ℂλ [cPtr (i 8) KernelAddr, cPtr (i 8) KernelAddr, cI 64] cVoid
llvmmemset n = cFn $ ℂλ [cPtr (i 8) KernelAddr, cI 8, cI n, cI 32, cI 1] cVoid
memset = cFn $ ℂλ [cPtr (i 8) KernelAddr, cI 32, cI 64] (cPtr (i 8) KernelAddr)
memchr = cFn $ ℂλ [cPtr (i 8) KernelAddr, cI 32, cI 64] (cPtr (i 8) KernelAddr)
memscan = cFn $ ℂλ [cPtr (i 8) KernelAddr, cI 32, cI 64] (cPtr (i 8) KernelAddr)
strcpy = cFn $ ℂλ [cPtr (i 8) KernelAddr, cPtr (i 8) KernelAddr] (cPtr (i 8) KernelAddr)
strncpy = cFn $ ℂλ [cPtr (i 8) KernelAddr, cPtr (i 8) KernelAddr, cI 64] (cPtr (i 8) KernelAddr)
strchr = cFn $ ℂλ [cPtr (i 8) KernelAddr, cI 32] (cPtr (i 8) KernelAddr)
strnchr = cFn $ ℂλ [cPtr (i 8) KernelAddr, cI 64, cI 32] (cPtr (i 8) KernelAddr)
strlen = cFn $ ℂλ [cPtr (i 8) KernelAddr] (cI 64)
strnlen = cFn $ ℂλ [cPtr (i 8) KernelAddr, cI 64] (cI 64)
kmalloc = cFn $ ℂλ [cI 64, cI 32] (cPtr (i 8) KernelAddr)
kfree = cFn $ ℂλ [cPtr (i 8) KernelAddr] cVoid

iτℂ ∷ S.Set Τℂ
iτℂ = S.fromList $ 
  [ "_copy_from_user" =: (copy_from_user_N 64)
  , "_copy_from_user_nmi" =: (copy_from_user_N 32)
  , "_copy_from_user_io" =: (copy_from_user_N 32)
  , "_copy_to_user"   =: (copy_to_user_N 64)
  , "_copy_to_user_fromio"   =: (copy_to_user_N 32)
  , "strncpy_from_user" =: strncpy_from_user
  , "strnlen_user" =: x_user
  , "clear_user" =: x_user
  , "__clear_user" =: x_user
  , "memcmp" =: memcmp
  , "__memcpy" =: memcpy
  , "llvm.memcpy.p0i8.p0i8.i32" =: (llvmmemcpy 32)
  , "llvm.memcpy.p0i8.p0i8.i64" =: (llvmmemcpy 64)
  , "wil_memcpy_fromio_32" =: wil_memcpy_x 
  , "wil_memcpy_toio_32" =: wil_memcpy_x
  , "llvm.memset.p0i8.i32" =: (llvmmemset 32)
  , "llvm.memset.p0i8.i64" =: (llvmmemset 64)
  , "memset" =: memset
  , "llvm.memmove.p0i8.p0i8.i32" =: (llvmmemcpy 32) 
  , "llvm.memmove.p0i8.p0i8.i64" =: (llvmmemcpy 64)
  , "memmove" =: memcpy
  , "strcpy" =: strcpy 
  , "strncpy" =: strncpy 
  , "strcat" =: strcpy
  , "strncat" =: strncpy
  , "strchr" =: strchr
  , "strrchr" =: strchr
  , "strnchr" =: strnchr
  , "strlen" =: strlen
  , "strnlen" =: strnlen
  , "memchr" =: memchr
  , "memchr_inv" =: memchr
  , "strstr" =: strcpy
  , "strnstr" =: strncpy
  , "memscan" =: memscan 
  , "__kmalloc" =: kmalloc
  , "kfree" =: kfree
  ]
