{-#LANGUAGE RecordWildCards, DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.EncoderSC
-- Copyright :  (c) 2012 Marcelo Sousa
-- Incomplete SystemC Encoder
-------------------------------------------------------------------------------

module Concurrent.Model.EncoderSC (encode) where

import Concurrent.Model                    hiding (State)

import Concurrent.Model.Encoder.Model (encType, encGlobalVars, preEncoder, wrap, sAnd, sOr, sFn, encodeMain)
import Concurrent.Model.Encoder.Threads (encodeThreads)
import Concurrent.Model.Analysis.ControlFlow (ControlFlow(..))

import Language.LLVMIR

import Language.SMTLib2.Base
import Language.SMTLib2.Builder

import qualified Data.IntMap as IM
import qualified Data.Map as Map

import Data.Maybe
import Data.List (nub)

import Debug.Trace (trace)

import Control.Monad.State

-- TODO: Remove this SystemC junk to a better place. 
encodeSysC :: (SCModel t) => Model t -> Int -> SMod
encodeSysC m@Model{..} k = let ccfg@ControlFlow{..} = controlflow m
                               tvs = Map.map (\pci -> ThreadState pci Map.empty) $ Map.delete "undefined" cte  -- ^ Set the initial PC for each thread
                               s0  = GlobalState Map.empty (-1) Map.empty tvs                                  -- ^ Initial state
                               (smod, sf) = runState (encSysCModel m k) s0                             
                           in smod --trace (show s0 ++ show sf) $ smod

-- | Main encode function.
encSysCModel :: (SCModel t) => Model t -> Int -> State GlobalState SMod
encSysCModel m k = do --tyenc <- encNmdTys  m   -- ^ Encode Named Types
                      --gvenc <- encGlobals m   -- ^ Encode Global Variables
                      menc  <- encSCFunctions m k  -- ^ Encode Functions
                      return $ nub $ preamble ++ menc ++ final

encSCFunctions :: (SCModel t) => Model t -> Bound -> State GlobalState SExpressions
encSCFunctions m@Model{..} k = do gs@GlobalState{..} <- get
                                  let ccfg@ControlFlow {..} = controlflow m 
                                      fs = toFunctions procs 
                                      (s,p)  = preEncoder fs defsorts decls
                                      se = preEncode p
                                      l = Map.empty
                                      --(l, pcs, sexprs) = encodeMain    (unProc mainf) p decls
                                      --(cpcs, csexprs)  = encodeThreads (toFunctions procs)  k p l (Map.delete "main" cte) $ Map.delete "main" cfg 
                                  trace (show $ fails p) $ return $ s ++ se -- ++ cpcs ++ [ assert csexprs ] 
                                --trace ("----\n" ++ show p ++ "--- ---\n" ++ show l) $ return $ s ++ se ++ pcs ++ [ assert $ wrap sAnd sexprs ] ++ cpcs ++ [ assert csexprs ]