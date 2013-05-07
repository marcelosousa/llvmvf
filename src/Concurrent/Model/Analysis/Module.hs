-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.Module where

import Language.LLVMIR
import Concurrent.Model.Analysis.ControlFlow
import Concurrent.Model.Analysis.DataFlow
import Concurrent.Model.Analysis.Context
import Concurrent.Model.Analysis.Util
import qualified Data.Map   as M
import qualified Data.Maybe as MB

-- Use the State Monad
analyseModule :: String -> Module -> (Module, ControlFlow, DataFlow)
analyseModule ep (Module id layout target gvars funs nmdtys) =
  let fn = MB.fromMaybe (errorMsg ep funs) $ M.lookup ep funs 
      iCore = Core nmdtys gvars funs
      env = Env iCore eCore eCFG eDF (-1)
      oenv  = evalContext (analyseFunction fn) env
      Core tys vars fs = coreout oenv
      fcfg  = ccfg oenv
      fdf   = df oenv 
      m     = Module id layout target vars fs tys
  in (m, fcfg, fdf)

errorMsg :: String -> Functions -> Function
errorMsg msg funs = error $ "analyseModule: " ++ msg ++ " " ++ show (M.keys funs)

analyseFunction :: Function -> Context ()
analyseFunction fn = case fn of
  FunctionDecl name _ rty iv pms -> return ()
  FunctionDef  name _ rty iv pms body -> return ()-- analyseBBs body

getEntryLabel :: Function -> Maybe PC
getEntryLabel fn = case fn of
  FunctionDecl name _ rty iv pms -> Nothing
  FunctionDef  name _ rty iv pms bbs -> 
    Just $ instrpc $ head $ instrs_BasicBlock_BasicBlock $ head bbs