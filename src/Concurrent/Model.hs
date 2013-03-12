{-#LANGUAGE EmptyDataDecls, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model where

import qualified Data.IntMap as IM
import qualified Data.Map as Map

import Language.LLVMIR hiding (emptyFunction)
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

-- Program P has a set of threads and a set of shared variables
-- V  - Global variables (gvars)
-- add scheduler type
data Model t = Model { nmdtys :: NamedTypes -- This information should not be here
                     , gvars  :: Globals
                     , mainf  :: Process
                     , procs  :: Processes
                     , decls  :: Declarations -- This information should be in the dataflow
                     } 

data Process = Process { ident :: String, unProc :: Function }

emptyProcess :: Process
emptyProcess = Process "undefined" emptyFunction

type Declarations = Map.Map String (Type, Parameters) 
type Processes = IM.IntMap Process

type Bound = Int
type TypeEnv   = Map.Map Type (SSortExpr, SSort)

emptyPreEncoder :: PreEncoder
emptyPreEncoder = PreEncoder Map.empty Map.empty [] Map.empty Map.empty []
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

type Valuation = Map.Map Id (Either Id Value)

-- GlobalState of a Concurrent System
-- type GlobalState = (Map.Map String (PC, Map.Map Id Value), Map.Map Id Value, PC)
nullGlobalState :: GlobalState
nullGlobalState = GlobalState Map.empty (-1) Map.empty Map.empty

data GlobalState = GlobalState { defsorts  :: TypeEnv
                               , currentpc :: PC
                               , gvals     :: Valuation
                               , ti        :: Map.Map String ThreadState
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
  show (Model nmdtys gvars mainf procs decls) = show mainf ++ "\n" ++ show procs ++ "\n" ++ show decls

instance Pretty Function where
    pretty f = pp_Syn_Function $ wrap_Function (sem_Function f) $ Inh_Function {}

instance Show Process where
  show (Process i f) = show $ pretty f