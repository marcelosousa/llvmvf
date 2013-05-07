{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.Module where

import Language.LLVMIR hiding (Switch)
import Concurrent.Model.Analysis.ControlFlow
import Concurrent.Model.Analysis.Instruction
import Concurrent.Model.Analysis.DataFlow
import Concurrent.Model.Analysis.Context
import Concurrent.Model.Analysis.Util
import qualified Data.Map   as M
import qualified Data.Maybe as MB

analyseModule :: String -> Module -> (Module, ControlFlow, DataFlow)
analyseModule ep (Module id layout target gvars funs nmdtys) =
  let fname = Global ep
      fn = MB.fromMaybe (errorMsg ep $ M.keys funs) $ M.lookup fname funs 
      bb = MB.fromMaybe (errorMsg ep fn) $ entryBBFunction fn 
      pc = MB.fromMaybe (errorMsg (show bb) fn) $ entryPCFunction fn 
      iLoc  = Location fname bb pc True
      iCore = Core nmdtys gvars funs
      env = Env iCore eCore eCFG eDF iLoc M.empty M.empty
      oenv  = evalContext (analyseFunction fn) env
      Core tys vars fs = coreout oenv
      fcfg  = ccfg oenv
      fdf   = df oenv 
      m     = Module id layout target vars fs tys
  in (m, fcfg, fdf)

errorMsg :: (Show b) => String -> b -> a
errorMsg msg b = error $ "analyseModule: " ++ msg ++ " " ++ show b

-- Check if it was seen
analyseFunction :: Function -> Context ()
analyseFunction fn = case fn of
  FunctionDecl name _ rty iv pms -> return ()
  FunctionDef  name _ rty iv pms body -> do 
      analyseBB $ head body
      e@Env{..} <- getEnv
      mapM_ analyseLoc undefined 

-- Analyse a special location
-- VIF
analyseLoc :: Loc -> Context ()
analyseLoc loc = do 
    e@Env{..} <- getEnv
    let ci@Core{..} = corein
    case loc of
        SyncLoc l@Location{..} i -> do  -- WaitThread
            let fi = MB.fromJust $ M.lookup i funs -- Retrieve which function its waiting for
            analyseFunction fi -- Analyze it !! Improve this
            o@Env{..} <- getEnv -- Retrieve the new env
            let p = getThreadExits i efloc
                c = foldr (\l1 r -> (Switch l1 lpc):r) ccfg p 
            putEnv $ o {ccfg = c}
        ExitLoc l@Location{..} w -> case w of
            _ -> return ()
            
-- Add to seen
analyseBB :: BasicBlock -> Context ()
analyseBB (BasicBlock i instrs) = mapM_ analyseInstr instrs

         