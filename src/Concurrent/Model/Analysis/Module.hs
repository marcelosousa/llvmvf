-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.Module where

import Language.LLVMIR
import Concurrent.Model.Analysis.ControlFlow
import Concurrent.Model.Analysis.DataFlow
import Concurrent.Model.Analysis.Context

-- Use the State Monad
analyseModule :: String -> Module -> (Module, ControlFlow, DataFlow)
analyseModule ep (Module id layout target gvars funs nmdtys) =
  let env = Env eCore eCore eCFG eDF
      oenv  = evalContext analyseModule' env
      (Core tys vars fs) = coreout oenv
      fcfg  = ccfg oenv
      fdf   = df oenv 
      m     = Module id layout target vars fs tys
  in (m, fcfg, fdf)

analyseModule' :: Context ()
analyseModule' = return ()
	

