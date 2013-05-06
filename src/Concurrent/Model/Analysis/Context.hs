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

import Language.LLVMIR
import Concurrent.Model.Analysis.ControlFlow
import Concurrent.Model.Analysis.DataFlow

data Core = Core
  { nmdtys :: NamedTypes
  , vars   :: Globals
  , funs   :: Functions
  }

eCore :: Core
eCore = undefined

data Env = Env
  { corein  :: Core
  , coreout :: Core
  , ccfg    :: ControlFlow
  , df      :: DataFlow
  }

newtype Context a = Context { unContext :: State Env a }
    deriving (Monad, Functor, Applicative)

evalContext :: Context a -> Env -> Env
evalContext = execState . unContext

getEnv :: Context Env
getEnv = Context get

putEnv :: Env -> Context ()
putEnv = Context . put