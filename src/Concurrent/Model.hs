{-#LANGUAGE EmptyDataDecls, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model where

import qualified Data.IntMap as IM
import qualified Data.Map    as M

import Language.LLVMIR
import Language.LLVMIR.Util
import Language.LLVMIR.Printer.Module

import Language.SMTLib2.Base

import Concurrent.Model.Analysis.ControlFlow (cflowfs, ControlFlow(..))
import Concurrent.Model.Analysis.DataFlow    (dflowfs, DataFlow(..))

import UU.PPrint
import Debug.Trace

-- Synchronous (Interleaved) Concurrent Models
-- At one given time, there only one atomic instruction
-- being executed. Scheduler specification is required.
class SCModel t where
  model       :: Module  -> Model t
  prune       :: String  -> Model t -> (Model t, ControlFlow, DataFlow)
--  controlflow :: Model t -> ControlFlow
--  controlflow m@Model{..} = cflowfs $ getFs mainf procs 
--  dataflow    :: Model t -> DataFlow
--  dataflow    m@Model{..} = dflowfs $ getFs mainf procs
 
-- Monad Kind
-- if possible a type definition would be better
data Model t = Model Module

---------------------------------------------------
-- REMOVE THIS PART FROM THIS FILE
type Bound = Int
type TypeEnv   = M.Map Type (SSortExpr, SSort)

emptyPreEncoder :: PreEncoder
emptyPreEncoder = PreEncoder M.empty M.empty [] M.empty M.empty []
-- 
data PreEncoder = PreEncoder { argToPar :: M.Map (PC,Int,Value) Id   -- Map an argument to a parameter -- Do not support calling the same function twice. New fresh variables
                             , fStore   :: M.Map Id (Type, [PC])     -- Map a global variable to a list of program counter that store a new value
                             , mutexes  :: [Id]
                             , sortEnv  :: TypeEnv                     -- Map all the types to a sort expression and a sort name
                             , locals   :: M.Map Id Type             -- Map all identifiers to a type
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

type Valuation = M.Map Id (Either Id Value)

-- GlobalState of a Concurrent System
-- type GlobalState = (M.Map String (PC, M.Map Id Value), M.Map Id Value, PC)
nullGlobalState :: GlobalState
nullGlobalState = GlobalState M.empty (-1) M.empty M.empty

data GlobalState = GlobalState { defsorts  :: TypeEnv
                               , currentpc :: PC
                               , gvals     :: Valuation
                               , ti        :: M.Map Identifier ThreadState
                               }
  deriving Show

data ThreadState = ThreadState { tipc  :: PC
                               , lvals :: Valuation
                               }
  deriving Show

type Transitions = [Transition]
type Transition = (PC, Bool, GlobalState -> (GlobalState, SExpressions, ISExpr), PC) 

-- Intermediate SMT Expression
data ISExpr = ISEmpty
            | ISExpr     SExpr
            | ISFunction (ISExpr -> ISExpr)

fromISExpr :: ISExpr -> SExpr
fromISExpr (ISExpr s)     = s
fromISExpr ISEmpty        = error "ISEmpty"
fromISExpr (ISFunction f) = error "ISFunction"

instance Show (Model t) where
  show (Model mod) = show mod