{-#LANGUAGE EmptyDataDecls, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.SysCModel.CIModelFZ
-- Model Example for CODES+ISSS Zero (Final)
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.SysCModel.CIModelFZ where

import qualified Data.IntMap as IM
import qualified Data.Map as Map

import Language.LLVMIR
import Language.LLVMIR.Printer.Module

import Concurrent.SysCModel
import Concurrent.SysCModel.CIModelZ

m1mdl = SysC [m1]

m1 = ScMod "M1" m1vars m1procs

m1vars = Vars $ IM.insert 0 ScEvent $ IM.insert 1 (ScVar $ TyInt 8) $ IM.singleton 2 (ScVar $ TyInt 8) 
m1procs = p

