{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.Inference.Worklist
-- Copyright :  (c) 2013 Marcelo Sousa
-- Worklist algorithm
-------------------------------------------------------------------------------

module Analysis.Type.Inference.Worklist where

import Language.LLVMIR hiding (Id)
import Analysis.Type.Inference.Base
import Analysis.Type.Memory.Util
import Analysis.Type.Memory.TyAnn as T

import Analysis.Type.Inference.Value
import Control.Monad.State
import Analysis.Type.Util

import qualified Data.Set as S

