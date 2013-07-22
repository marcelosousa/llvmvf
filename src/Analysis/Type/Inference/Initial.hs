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

ioremap ∷ Τℂ
ioremap = let nℂ = ℂπ (Global "ioremap") 
              τℂr =  ℂτ $ TyDer $ TyPtr (i 8) TyIOAddr
              τℂ = ℂp (ℂλ [ℂτ (i 64), ℂτ (i 64)] τℂr) TyRegAddr
          in nℂ :=: τℂ

ioremap_cache ∷ Τℂ
ioremap_cache = let nℂ = ℂπ (Global "ioremap_nocache") 
                    τℂr =  ℂτ $ TyDer $ TyPtr (i 8) TyIOAddr
                    τℂ = ℂp (ℂλ [ℂτ (i 64), ℂτ (i 64)] τℂr) TyRegAddr
                in nℂ :=: τℂ

iounmap ∷ Τℂ        
iounmap = let nℂ = ℂπ (Global "iounmap") 
              τℂr =  ℂτ $ TyPri TyVoid 
              τℂ = ℂp (ℂλ [ℂτ $ TyDer $ TyPtr (i 8) TyIOAddr] τℂr) TyRegAddr
          in nℂ :=: τℂ

kmalloc ∷ Τℂ
kmalloc = let nℂ = ℂπ (Global "kmalloc") 
              τℂr =  ℂτ $ TyDer $ TyPtr (i 8) TyRegAddr
              τℂ = ℂp (ℂλ [ℂτ (i 64), ℂτ (i 64)] τℂr) TyRegAddr
          in nℂ :=: τℂ
          
kfree ∷ Τℂ        
kfree = let nℂ = ℂπ (Global "kfree") 
            τℂr =  ℂτ $ TyPri TyVoid 
            τℂ = ℂp (ℂλ [ℂτ $ TyDer $ TyPtr (i 8) TyRegAddr] τℂr) TyRegAddr
        in nℂ :=: τℂ

errorf ∷ Τℂ
errorf = let nℂ = ℂπ (Global "e1000_clean_jumbo_rx_irq0")
         in nℂ :=: ℂp (ℂλ [ℂτ $ TyDer $ TyPtr (i 64) TyIOAddr] (ℂτ $ i 64)) TyRegAddr
    
iτℂ ∷ S.Set Τℂ
--iτℂ = ioremap ∘ (ioremap_cache ∘ (iounmap ∘ (kmalloc ∘ (kfree ∘ (errorf ∘ ε)))))
iτℂ = ioremap ∘ (ioremap_cache ∘ (iounmap ∘ (kmalloc ∘ (kfree ∘ ε))))
--iτℂ = ioremap ∘ (ioremap_cache ∘ (iounmap ∘ (kfree ∘ ε)))
