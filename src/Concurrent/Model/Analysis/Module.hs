-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.Module where

import Language.LLVMIR
import Concurrent.Model.Analysis.ControlFlow
import Concurrent.Model.Analysis.Instruction
import Concurrent.Model.Analysis.DataFlow
import Concurrent.Model.Analysis.Context
import Concurrent.Model.Analysis.Util
import qualified Data.Map   as M
import qualified Data.Maybe as MB

analyseModule :: String -> Module -> (Module, ControlFlow, DataFlow)
analyseModule ep (Module id layout target gvars funs nmdtys) =
  let fn = MB.fromMaybe (errorMsg ep $ M.keys funs) $ M.lookup ep funs 
      bb = MB.fromMaybe (errorMsg ep fn) $ entryBBFunction fn 
      pc = MB.fromMaybe (errorMsg (show bb) fn) $ entryPCFunction fn 
      iLoc  = Location ep bb pc True
      iCore = Core nmdtys gvars funs
      env = Env iCore eCore eCFG eDF iLoc [] []
      oenv  = evalContext (analyseFunction fn) env
      Core tys vars fs = coreout oenv
      fcfg  = ccfg oenv
      fdf   = df oenv 
      m     = Module id layout target vars fs tys
  in (m, fcfg, fdf)

errorMsg :: (Show b) => String -> b -> a
errorMsg msg b = error $ "analyseModule: " ++ msg ++ " " ++ show b

analyseFunction :: Function -> Context ()
analyseFunction fn = case fn of
  FunctionDecl name _ rty iv pms -> return ()
  FunctionDef  name _ rty iv pms body -> analyseBB $ head body

-- This is going to give me a context
-- Then I need to decide if I should go somewhere or not.
analyseBB :: BasicBlock -> Context ()
analyseBB (BasicBlock i instrs) = mapM_ analyseInstr instrs