{-#LANGUAGE EmptyDataDecls, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model where

import qualified Data.IntMap as IM
import qualified Data.Map    as M

import Language.LLVMIR
import Language.LLVMIR.Printer.Module

import Language.SMTLib2.Base

import UU.PPrint
import Debug.Trace

-- Synchronous (Interleaved) Concurrent Models
-- At one given time, there only one atomic instruction
-- being executed. Scheduler specification is required.
class SCModel t where
  model :: Module  -> Model t
 
-- Monad Kind
-- if possible a type definition would be better
data Model t = Model Module

instance Show (Model t) where
  show (Model mod) = show mod

---------------------------------------------------
-- REMOVE THIS PART FROM THIS FILE
type Bound = Int

{-
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
-}