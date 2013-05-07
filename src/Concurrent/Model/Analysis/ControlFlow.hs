-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.ControlFlow
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.ControlFlow where

import Language.LLVMIR hiding (Switch)
import UU.PPrint

data Flow = Intra  Int Int
          | Inter  Int Int
          | Switch Int Int

type ControlFlow = [Flow]

eCFG :: ControlFlow 
eCFG = []

instance Pretty Flow where
  pretty (Intra  a b) = int a <+> text "->" <+> int b <> char ';'
  pretty (Inter  a b) = int a <+> text "->" <+> int b <> char ';'
  pretty (Switch a b) = int a <+> text "->" <+> int b <> char ';'
