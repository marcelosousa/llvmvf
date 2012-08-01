{-#LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Context
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Context where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import qualified Language.LLVMIR as LL

import Language.LLVMIR.Extractor.Util

import qualified LLVM.Core as Core

data Env = Env
  { mdl    :: Core.Module
  , nmdtys :: LL.NamedTypes 
  , instr  :: Maybe Value
  , pc     :: LL.PC
  }

newtype Context m a = Context { unContext :: StateT Env m a }
    deriving (Monad, MonadIO, MonadPlus, Functor, Applicative, MonadTrans)

runContext :: (Monad m) => Context m a -> Env -> m a
runContext = evalStateT . unContext

withContext :: (Env -> m (a, Env)) -> Context m a
withContext = Context . StateT

getEnv :: (Monad m) => Context m Env
getEnv = Context get

putEnv :: (Monad m) => Env -> Context m ()
putEnv = Context . put

getOperands :: Value -> Context IO [(String, Value)]
getOperands = liftIO . Core.getOperands
