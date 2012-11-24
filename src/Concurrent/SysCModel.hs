{-#LANGUAGE EmptyDataDecls, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.SysCModel
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.SysCModel where

import qualified Data.IntMap as IM
import qualified Data.Map as Map

import Language.LLVMIR
import Language.LLVMIR.Printer.Module

import Language.SMTLib2.Base

import Concurrent.Model.Analysis.ControlFlow (cflowfs, ControlFlow(..))
import Concurrent.Model.Analysis.DataFlow    (dflowfs, DataFlow(..))
--import Concurrent.Model.SystemC
--import Concurrent.Model

import UU.PPrint
import Debug.Trace

-- Synchronous (Interleaved) Concurrent Models
-- At one given time, there only one atomic instruction
-- being executed. Scheduler specification is required.
{-
class SCModel t where
  model       :: Module  -> Model t
  controlflow :: Model t -> ControlFlow
  controlflow m@Model{..} = cflowfs $ getFs mainf procs 
  dataflow    :: Model t -> DataFlow
  dataflow    m@Model{..} = dflowfs $ getFs mainf procs
 
getFs :: Process -> Processes -> Functions
getFs p ps = let pss = IM.elems ps
                 fs  = foldr (\p' r -> Map.insert (ident p') (unProc p') r) Map.empty pss 
                 fs' = Map.insert (ident p) (unProc p) fs
             in fs'

toFunctions :: Processes -> Functions
toFunctions ps = let pss = IM.elems ps
                 in  foldr (\p' r -> Map.insert (ident p') (unProc p') r) Map.empty pss 
-}

-- add scheduler type
data SysModel = SysC [ScMod]
data ScMod = ScMod { mid     :: ScMId
                   , mvars   :: MVars
                   , scprocs :: MProcs
                   }

data MVars = Vars (IM.IntMap ScVar)
data ScVar = ScEvent 
           | ScVar Type

data MProcs = Procs (IM.IntMap ScProc)

data ScProc = ScMethod { metid :: String, unMet :: Function }
            | ScThread { mthid :: String, unThr :: Function }

type ScMId = String

{-
type Bound = Int
type TypeEnv   = Map.Map Type (SSortExpr, SSort)
-- 
data PreEncoder = PreEncoder { argToPar :: Map.Map (PC,Int,Value) Id               -- Map an argument to a parameter -- Do not support calling the same function twice. New fresh variables
                             , fStore   :: Map.Map Id (Type, [PC])             -- Map a global variable to a list of program counter that store a new value
                             , mutexes  :: [Id]
                             , sortEnv  :: TypeEnv                     -- Map all the types to a sort expression and a sort name
                             , locals   :: Map.Map Id Type             -- Map all identifiers to a type
                             , fails    :: [PC]                        -- List of program counters that call assert_fail
                             }

instance Show PreEncoder where
  show (PreEncoder a fs m s l f) = "PreEncoder\n" ++ "-------------\n" 
                             ++ show a ++ "\n--------------\n" 
                             ++ show fs ++ "\n--------------\n" 
                             ++ show m ++ "\n--------------\n" 
                             ++ show s ++ "\n-----------------\n"
                             ++ show l ++ "\n-----------------\n"
                             ++ show f
-}
