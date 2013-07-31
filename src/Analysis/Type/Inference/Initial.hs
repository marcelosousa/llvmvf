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
cFn λτ = ℂp λτ anyRegAddr

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

ioremap ∷ ℂ
ioremap = cFn $ ℂλ [cI 64, cI 64] $ cPtr (i 8) TyIOAddr

iounmap ∷ ℂ        
iounmap = cFn $ ℂλ [cPtr (i 8) TyIOAddr] cVoid

kmalloc ∷ ℂ
kmalloc = cFn $ ℂλ [cI 64, cI 32] $ cPtr (i 8) kLogAddr
          
kfree ∷ ℂ
kfree = cFn $ ℂλ [cPtr (i 8) kLogAddr] cVoid

valloc ∷ ℂ
valloc = cFn $ ℂλ [cI 64] $ cPtr (i 8) kVirAddr

vfree ∷ ℂ
vfree = cFn $ ℂλ [cPtr (i 8) kVirAddr] cVoid

{-
errorf ∷ Τℂ
errorf = let nℂ = ℂπ (Global "e1000_probe2")
         in nℂ :=: ℂp (ℂλ [ℂτ $ TyDer $ TyPtr (i 32) TyIOAddr, ℂτ $ i 32, ℂτ $ i 32, ℂτ $ TyDer $ TyPtr (i 32) TyIOAddr] (ℂτ $ i 32)) TyRegAddr
-}

--iτℂ = ioremap ∘ (ioremap_cache ∘ (iounmap ∘ (kmalloc ∘ (kfree ∘ (errorf ∘ ε)))))
iτℂ ∷ S.Set Τℂ
iτℂ = S.fromList $ 
  [ "ioremap" =: ioremap
  , "ioremap_nocache" =: ioremap
  , "iounmap" =: iounmap
  , "__kmalloc" =: kmalloc
  , "kfree"   =: kfree
  , "vzalloc" =: valloc
  , "vfree"   =: vfree
  ]
