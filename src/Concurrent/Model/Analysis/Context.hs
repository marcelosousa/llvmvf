{-#LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.Context
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.Context where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad.State

import qualified Data.Map as M

import Language.LLVMIR
import Concurrent.Model.Analysis.ControlFlow
import Concurrent.Model.Analysis.DataFlow

data Location = Location 
  { fn  :: String
  , bb  :: Identifier
  , lpc :: PC
  , ise :: Bool
  }

type Locations = [Location]

data Where = BBLoc Identifier
           | FnLoc Identifier
           | ThLoc Identifier
           | End

data ExitLoc = ExitLoc Location Where

type ExitLocs = [ExitLoc]

data Core = Core
  { nmdtys :: NamedTypes
  , vars   :: Globals
  , funs   :: Functions
  }

eCore :: Core
eCore = Core M.empty [] M.empty

data Env = Env
  { corein  :: Core
  , coreout :: Core
  , ccfg    :: ControlFlow
  , df      :: DataFlow
  , ploc    :: Location
  , pfloc   :: Locations
  , eloc    :: ExitLocs
  }

newtype Context a = Context { unContext :: State Env a }
    deriving (Monad, Functor, Applicative)

evalContext :: Context a -> Env -> Env
evalContext = execState . unContext

getEnv :: Context Env
getEnv = Context get

putEnv :: Env -> Context ()
putEnv = Context . put