-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model where

import Prelude              hiding (sequence)
import Data.Char            (chr)
import qualified Data.Map as Data.Map
import qualified Data.Map as Map
import Data.Set

import Language.LLVMIR

class CModel t where
  model     :: Module  -> Model t
  scheduler :: Model t -> Model t
 
data Model t = Model { mtype  :: t
                     , mainf  :: Main
                     , nmdtys :: NamedTypes
                     , procs  :: Processes
                     } 

data Main = Main Function 

data Process   = Process Function
type Processes = Set Process

data PThread

{-
bmc :: CModel t => Module -> Concurrent SMTLib2 t
bmc mdl _ = do i <- model mdl
    
                            
-}
