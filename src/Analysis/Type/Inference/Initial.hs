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
import Language.LLVMIR.Util (variadicFns)
import Analysis.Type.Inference.Base
import Analysis.Type.Inference.Global
import Analysis.Type.Inference.Function
import Analysis.Type.Inference.Solver
import Analysis.Type.Memory.TyAnn

import Control.Monad
import Control.Monad.State
import Prelude.Unicode ((⧺))

ioremap ∷ Τℂ
ioremap = let nℂ = ℂπ (Global "ioremap") 
              τℂr =  ℂτ $ TyDer $ TyPtr (i 8) TyIOAddr
              τℂ = ℂp (ℂλ [ℂτ (i 64), ℂτ (i 64)] τℂr) TyRegAddr
          in nℂ :=: τℂ

ioremap_cache ∷ Τℂ
ioremap_cache = let nℂ = ℂπ (Global "ioremap_cache") 
                    τℂr =  ℂτ $ TyDer $ TyPtr (i 8) TyIOAddr
                    τℂ = ℂp (ℂλ [ℂτ (i 64), ℂτ (i 64)] τℂr) TyRegAddr
                in nℂ :=: τℂ

iounmap ∷ Τℂ        
iounmap = let nℂ = ℂπ (Global "iounmap") 
              τℂr =  ℂτ $ TyPri TyVoid 
              τℂ = ℂp (ℂλ [ℂτ $ TyDer $ TyPtr (i 8) TyIOAddr] τℂr) TyRegAddr
          in nℂ :=: τℂ

kfree ∷ Τℂ        
kfree = let nℂ = ℂπ (Global "kfree") 
            τℂr =  ℂτ $ TyPri TyVoid 
            τℂ = ℂp (ℂλ [ℂτ $ TyDer $ TyPtr (i 8) TyRegAddr] τℂr) TyRegAddr
        in nℂ :=: τℂ


iτℂ ∷ S.Set Τℂ
iτℂ = ioremap ∘ (ioremap_cache ∘ (iounmap ∘ (kfree ∘ ε)))
