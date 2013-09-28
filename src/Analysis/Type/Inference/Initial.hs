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

sys_Xint = cFn $ ℂλ [cPtr (i 32) UserAddr] cVoid
_memcpy = cFn $ ℂλ [cPtr (i 8) KernelAddr, cPtr (i 8) KernelAddr, cI 64] (cPtr (i 8) KernelAddr)
copy_from_user = cFn $ ℂλ [cPtr (i 8) KernelAddr, cPtr (i 8) UserAddr, cI 64] (cPtr (i 8) KernelAddr)
copy_to_user = cFn $ ℂλ [cPtr (i 8) UserAddr, cPtr (i 8) KernelAddr, cI 64] (cPtr (i 8) UserAddr)

iτℂ ∷ S.Set Τℂ
iτℂ = S.fromList $ 
  [ "sys_getint" =: sys_Xint
  , "sys_setint" =: sys_Xint
  , "_memcpy" =: _memcpy
  , "copy_from_user" =: copy_from_user
  , "copy_to_user" =: copy_to_user
  --, "x" =: ℂp (cI 32) UserAddr
  ]
