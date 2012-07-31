-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model where

import Data.IntMap 

import Language.LLVMIR

-- Asynchronous (Interleaved) Concurrent Models
-- At one given time, there only one atomic instruction
-- being executed. Scheduler specification is required.
class ACModel t where
  model     :: Module  -> Model t
--  scheduler :: Model t -> Model t
--  create
--  constructs
 
data Model t = Model { nmdtys :: NamedTypes
                     , gvars  :: Globals
                     , procs  :: Processes
                     } 
  deriving Show

-- data Main = Main Function 

data Process   = Process Function
  deriving Show

type Processes = IntMap Process
