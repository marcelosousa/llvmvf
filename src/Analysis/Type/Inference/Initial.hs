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
import Analysis.Type.TypeQual

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

--ioremap, iounmap, kmalloc, kfree, valloc, vfree, copyFromUser ∷ ℂ
-- Memory mapped IO API
ioremap = cFn $ ℂλ [cI 64, cI 64] $ cPtr (i 8) TyIOAddr
iounmap = cFn $ ℂλ [cPtr (i 8) TyIOAddr] cVoid
ioread  = cFn $ ℂλ [cPtr (i 8) TyIOAddr] $ cI 32
iowrite n = cFn $ ℂλ [cI n, cPtr (i 8) TyIOAddr] $ cVoid


ioOpRep = cFn $ ℂλ [cPtr (i 8) TyIOAddr, cPtr (i 8) anyRegAddr, cI 64] $ cVoid
-- Allocation on logical addresses
kmalloc = cFn $ ℂλ [cI 64, cI 32] $ cPtr (i 8) anyRegAddr
kfree = cFn $ ℂλ [cPtr (i 8) anyRegAddr] cVoid
-- Allocation on virtual addresses 
valloc = cFn $ ℂλ [cI 64] $ cPtr (i 8) anyRegAddr
vfree = cFn $ ℂλ [cPtr (i 8) anyRegAddr] cVoid
putPage = cFn $ ℂλ [cPtr (TyDer $ TyAgg $ TyStr "struct.page" 5 []) anyRegAddr] $ cVoid
-- Kernel to User Analysis
copyFromUser = cFn $ ℂλ [cPtr (i 8) anyRegAddr, cPtr (i 8) anyRegAddr, cI 32] $ cI 64
copyToUser = cFn $ ℂλ [cPtr (i 8) anyRegAddr, cPtr (i 8) anyRegAddr, cI 32] $ cI 64
mightSleep = cFn $ ℂλ [cPtr (i 8) anyRegAddr, cI 32, cI 32] $ cVoid

f = cFn $ ℂλ [cPtr (i 8) kVirAddr] $ cPtr (i 8) kLogAddr
g = cFn $ ℂλ [cPtr (i 8) kLogAddr] $ cPtr (i 8) kVirAddr
m = cFn $ ℂλ [cPtr (i 8) kVirAddr] $ cPtr (i 8) kVirAddr
{-
ioOpRep = cFn $ ℂλ [cPtr (i 8) TyIOAddr, cPtr (i 8) kLogAddr, cI 64] $ cVoid
-- Allocation on logical addresses
kmalloc = cFn $ ℂλ [cI 64, cI 32] $ cPtr (i 8) kLogAddr
kfree = cFn $ ℂλ [cPtr (i 8) kLogAddr] cVoid
-- Allocation on virtual addresses 
valloc = cFn $ ℂλ [cI 64] $ cPtr (i 8) kVirAddr
vfree = cFn $ ℂλ [cPtr (i 8) kVirAddr] cVoid
putPage = cFn $ ℂλ [cPtr (TyDer $ TyAgg $ TyStr "struct.page" 5 []) kVirAddr] $ cVoid
-- Kernel to User Analysis
copyFromUser = cFn $ ℂλ [cPtr (i 8) kVirAddr, cPtr (i 8) uVirAddr, cI 32] $ cI 64
copyToUser = cFn $ ℂλ [cPtr (i 8) uVirAddr, cPtr (i 8) kVirAddr, cI 32] $ cI 64
mightSleep = cFn $ ℂλ [cPtr (i 8) kVirAddr, cI 32, cI 32] $ cVoid
-}

{-

errorf ∷ Τℂ
errorf = let nℂ = ℂπ (Global "e1000_probe2")
         in nℂ :=: ℂp (ℂλ [ℂτ $ TyDer $ TyPtr (i 32) TyIOAddr, ℂτ $ i 32, ℂτ $ i 32, ℂτ $ TyDer $ TyPtr (i 32) TyIOAddr] (ℂτ $ i 32)) TyRegAddr
-}

iτℂ ∷ S.Set Τℂ
iτℂ = S.fromList $ 
  [ 
{-    "ioremap" =: ioremap
  , "ioremap_nocache" =: ioremap
  , "iounmap" =: iounmap
  , "ioread8" =: ioread
  , "ioread16" =: ioread
  , "ioread32" =: ioread
  , "iowrite8" =: iowrite 8
  , "iowrite16" =: iowrite 16
  , "iowrite32" =: iowrite 32
  , "ioread8_rep" =: ioOpRep
  , "ioread16_rep" =: ioOpRep
  , "ioread32_rep" =: ioOpRep
  , "iowrite8_rep" =: ioOpRep
  , "iowrite16_rep" =: ioOpRep
  , "iowrite32_rep" =: ioOpRep
  , "__kmalloc" =: kmalloc
  , "kfree"   =: kfree
  , "vzalloc" =: valloc
  , "vfree"   =: vfree
  , "_copy_to_user" =: copyToUser
  , "_copy_from_user" =: copyFromUser
  , "put_page" =: putPage
  , "__might_sleep" =: mightSleep-}
    "f" =: f 
  , "g" =: g 
  , "m" =: m   
--  , "f" =: ioremap
--  , "g" =: kmalloc
  ]
