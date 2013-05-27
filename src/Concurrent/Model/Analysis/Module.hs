{-#LANGUAGE RecordWildCards, DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.Module
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.Module where

import Language.LLVMIR hiding (Switch)
import Language.LLVMIR.Util

import Concurrent.Model.Analysis.ControlFlow
import Concurrent.Model.Analysis.Instruction
import Concurrent.Model.Analysis.DataFlow
import Concurrent.Model.Analysis.Context
import Concurrent.Model.Analysis.Util

import qualified Data.Map   as M
import qualified Data.Maybe as MB

import Debug.Trace (trace)

analyseModule :: String -> Module -> (Module, ControlFlow, DataFlow)
analyseModule ep (Module id layout target gvars funs nmdtys) =
  let fname = Global ep
      fn = MB.fromMaybe (errorMsg "analyseModule" ep $ M.keys funs) $ M.lookup fname funs 
      bb = MB.fromMaybe (errorMsg "analyseModule" ep fn) $ entryBBFunction fn 
      pc = MB.fromMaybe (errorMsg "analyseModule" (show bb) fn) $ entryPCFunction fn 
      iLoc  = Location fname bb pc True
      iCore = Core nmdtys gvars funs
      env = Env iCore eCore eCFG eDF iLoc M.empty M.empty M.empty
      oenv  = evalContext (analyseFunction fn) env
      Core tys vars fs = coreout oenv
      fcfg  = ccfg oenv
      fdf   = df oenv 
      m     = Module id layout target vars fs tys
  in (m, fcfg, fdf)

errorMsg :: (Show b) => String -> String -> b -> a
errorMsg s msg b = error $ s ++ ": " ++ msg ++ " " ++ show b

-- Check if it was seen
analyseFunction :: Function -> Context ()
analyseFunction fn = case fn of
  FunctionDecl name _ rty iv pms -> return ()
  FunctionDef  name _ rty iv pms body -> trace ("Analyzing Function " ++ show name) $ do
      e@Env{..} <- getEnv
      let coreo = addFnToCore fn coreout
      if fnWasAnalyzed name corein seen
      then return ()
      else do putEnv $ e {coreout = coreo}
              analyseBB $ head body

-- Analyse a special location
-- VIF
analyseLoc :: Loc -> Context ()
analyseLoc loc = do 
    e@Env{..} <- getEnv
    let ci@Core{..} = corein
        ploc@Location{..} = ploc
    case loc of
        SyncLoc l@Location{..} tni -> do  -- WaitThread
            let th = MB.fromMaybe (error $ "SycnLoc " ++ show tni ++ show funs) $ M.lookup tni funs -- Retrieve which function its waiting for
            case entryBBFunction th of
              Nothing -> return ()
              Just bb -> do 
                let pc = MB.fromMaybe (errorMsg "SycnLoc " (show bb) fn) $ entryPCFunction th
                    iLoc  = Location tni bb pc True
                    e' = e {ploc = iLoc}
                putEnv e'
                analyseFunction th
                o@Env{..} <- getEnv -- Retrieve the new env
                let p = getThreadExits tni efloc
                    c = foldr (\l1 r -> (Switch l1 lpc):r) ccfg p 
                putEnv $ o {ccfg = c}
        ExitLoc l@Location{..} w -> case w of
            EndFn   -> return () -- Its the job of the caller to
            EndTh _ -> return () -- findout the exit locations of the callee
            BBLoc bbi -> do let fnf = MB.fromMaybe (error $ "ExitLoc " ++ show fn ++ show funs) $ M.lookup fn funs
                                bba = findBasicBlock bbi fnf
                                pc = entryPCBB bba
                                iLoc = Location fn bbi pc True
                                c  = flow pc l ccfg
                                e' = e {ccfg = c, ploc = iLoc}
                            putEnv e'
                            analyseBB bba
            FnLoc fni -> do let fn = MB.fromMaybe (errorMsg "FnLoc" (show fni) $ M.keys funs) $ M.lookup fni funs
                            case entryBBFunction fn of
                              Nothing -> return ()
                              Just bb -> do
                                let pc = MB.fromMaybe (errorMsg "FnLoc" (show bb) fn) $ entryPCFunction fn 
                                    iLoc  = Location fni bb pc True
                                    c = iflow pc l ccfg
                                    e' = e {ccfg = c, ploc = iLoc}
                                putEnv e'
                                analyseFunction fn
                                o@Env{..} <- getEnv -- Retrieve the new env
                                let p = getFunctionExits fni efloc
                                    c' = foldr (\l1 r -> (Inter l1 lpc):r) ccfg p 
                                putEnv $ o {ccfg = c'}
            ThLoc tni -> do let th = MB.fromMaybe (errorMsg "ThLoc" (show tni) $ M.keys funs) $ M.lookup tni funs 
                            case entryBBFunction th of
                              Nothing -> return ()
                              Just bb -> do 
                                let pc = MB.fromMaybe (errorMsg "ThLoc" (show bb) fn) $ entryPCFunction th
                                    iLoc  = Location tni bb pc True
                                    c = tflow pc l ccfg
                                    e' = e {ccfg = c, ploc = iLoc}
                                putEnv e'
                                analyseFunction th
                          
-- Add to seen
analyseBB :: BasicBlock -> Context ()
analyseBB (BasicBlock i instrs) = do
    e@Env{..} <- getEnv
    let fni = fn ploc
    trace ("Analyzing BB " ++ show i ++ " of function " ++ show fni) $ if bbWasAnalyzed fni i seen
    then return ()
    else do mapM_ analyseInstr instrs
            o@Env{..} <- getEnv
            let seen' = addToSeen fni i seen
                locs = getLocs ploc efloc
            putEnv $ o {seen = seen'} 
            trace ("LOCS : " ++ show locs) $ mapM_ analyseLoc locs
