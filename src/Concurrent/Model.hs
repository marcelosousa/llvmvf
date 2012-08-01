-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model where

import qualified Data.IntMap as IM
import qualified Data.Map as Map

import Language.LLVMIR
import Language.LLVMIR.Printer.Module

import Concurrent.Model.Analysis.Flow (flowfs)

import UU.PPrint
import Debug.Trace

type Flow = [(Int,Int)]

-- Asynchronous (Interleaved) Concurrent Models
-- At one given time, there only one atomic instruction
-- being executed. Scheduler specification is required.
class ACModel t where
  model     :: Module  -> Model t
  flow      :: Model t -> Flow
  flow (Model _ _ mainf procs) = flowfs $ getFs mainf procs -- foldr (++) (flowp mainf) $ map flowp procs
--  scheduler :: Model t -> Model t
--  create
--  constructs
 
getFs :: Process -> Processes -> Functions
getFs p ps = let pss = IM.elems ps
                 fs  = foldr (\p' r -> Map.insert (ident p') (unProc p') r) Map.empty pss 
                 fs' = Map.insert (ident p) (unProc p) fs
             in fs'
data Model t = Model { nmdtys :: NamedTypes
                     , gvars  :: Globals
                     , mainf  :: Process
                     , procs  :: Processes
                     } 

instance Show (Model t) where
  show (Model nmdtys gvars mainf procs) = show mainf ++ "\n" ++ show procs 

data Process = Process { ident :: String, unProc :: Function }

instance Pretty Function where
    pretty f = pp_Syn_Function $ wrap_Function (sem_Function f) $ Inh_Function {}

instance Show Process where
  show (Process i f) = show $ pretty f

type Processes = IM.IntMap Process
