-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.ControlFlow
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.ControlFlow where

import Language.LLVMIR

data Flow = Intra  Int Int
          | Inter  Int Int
          | Switch Int Int

type ControlFlow = [Flow]

eCFG :: ControlFlow 
eCFG = []