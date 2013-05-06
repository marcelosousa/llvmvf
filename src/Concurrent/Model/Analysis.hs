-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model where

import Language.LLVMIR

import Concurrent.Model
import Concurrent.Model.Analysis.ControlFlow
import Concurrent.Model.Analysis.DataFlow
import Concurrent.Model.Analysis.Module

analyse :: (SCModel t) => String  -> Model t -> (Model t, ControlFlow, DataFlow)
analyse ep (Model m) = 
	let (m', cf, df) = analyseModule ep m
	in (Model m', cf, df)
