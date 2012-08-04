{-#LANGUAGE EmptyDataDecls #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model where

import qualified Data.IntMap as IM
import qualified Data.Map as Map

import Language.LLVMIR
import Language.LLVMIR.Printer.Module

import Concurrent.Model.Analysis.ControlFlow (cflowfs)

import UU.PPrint
import Debug.Trace

type ControlFlow = [(Int,Int)]

-- Synchronous (Interleaved) Concurrent Models
-- At one given time, there only one atomic instruction
-- being executed. Scheduler specification is required.
class SCModel t where
  model       :: Module  -> Model t
  controlflow :: Model t -> ControlFlow
  controlflow (Model _ _ mainf procs _) = cflowfs $ getFs mainf procs 
--  scheduler :: [(State, PCi)] -> TTransition
 
getFs :: Process -> Processes -> Functions
getFs p ps = let pss = IM.elems ps
                 fs  = foldr (\p' r -> Map.insert (ident p') (unProc p') r) Map.empty pss 
                 fs' = Map.insert (ident p) (unProc p) fs
             in fs'

-- Program P has a set of threads and a set of shared variables
-- V  - Global variables (gvars)
data Model t = Model { nmdtys :: NamedTypes
                     , gvars  :: Globals
                     , mainf  :: Process
                     , procs  :: Processes
                     , decls  :: Declarations
                     } 

data Process = Process { ident :: String, unProc :: Function }
type Declarations = Map.Map String Function
type Processes = IM.IntMap Process

-- Mi (1 <= i <= n) - Thread Model (Control + Data Flow)
-- Vi - Set of local variables of Mi
data TModel = TModel { vi  :: [Identifier]
                     , cfg :: CFG
                     , ci  :: [ControlState]
                     , dfg :: DFG
                     , code :: Function
                     }

data CFG
data DFG

type ControlState = Int

-- A state is a valuation of all global and local variables
type State = Map.Map Identifier Value

-- VLi - set of tuple of values for local data variables in Vi
-- VG  - set of tuple of values for global data variables

-- Global State = ([(Ci,VLi)], VG)

---- Interleaving (Operational) Semantics

-- Thread transition t = <c, g, u, c'>
-- c - current control state
-- c' - next control state
-- g - enabling guard
-- u - update assignments
type TTransition = (ControlState, Guard, Instructions, ControlState)

-- A guard is a state predicate
type Guard = State -> Bool

-- next(v) - Next state update of variable v 

-- pci - program counter of thread i
type PCi = PC

-- isEnabled 
isEnabled :: PCi -> TTransition -> State -> Bool
isEnabled pci (c,g,u,c') s = c == pci && g s

-- enabled(s) gives the set of all transitions enabled at state s
-- The interleaving semantics is a model where only one local transition
-- is scheduled to execute from a state!

update :: PCi -> State -> State
update pci s = let t = getOneEnable pci s
               in fire s t

getOneEnable :: PCi -> State -> TTransition
getOneEnable = undefined 

fire :: State -> TTransition -> State
fire = undefined 

-- Schedule of P is an interleaving sequence 
-- of thread transitions 
type Schedule = [TTransition]

-- An event e occurs when a unique transition t
-- is fired and e is called the generator of t.
-- t = gen(P,e)
data Event

-- A run of P is an ordered sequence of events
-- where each event e_i corresponds to firing of 
-- a unique transition t_i = gen(P, e_i)
type Run = [Event]

begin :: TTransition -> ControlState
begin (c,_,_,_) = c

end :: TTransition -> ControlState
end (_,_,_,c') = c'

-- tid(t) - thread id of the transition t
tid :: TTransition -> Int
tid = undefined

-- Each transition is atomic and has at most one memory access.

-- A transaction is an uninterrupted sequence of transitions
-- of a particular thread.
type Transaction = [TTransition]

-- Atomic Transaction
-- Happens-before



-- Printing
instance Show (Model t) where
  show (Model nmdtys gvars mainf procs decls) = show mainf ++ "\n" ++ show procs ++ "\n" ++ show decls

instance Pretty Function where
    pretty f = pp_Syn_Function $ wrap_Function (sem_Function f) $ Inh_Function {}

instance Show Process where
  show (Process i f) = show $ pretty f

