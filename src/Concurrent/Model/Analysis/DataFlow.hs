-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.DataFlow
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.DataFlow where

import Language.LLVMIR

import qualified Data.Map    as M

type TypeEnv   = M.Map Type () --(SSortExpr, SSort)

{-
data CS = ReadS
        | WriteS
        | Wait
-}

type DataFlow = M.Map String (M.Map Identifier Type)

eDF :: DataFlow
eDF = M.empty

emptyPreEncoder :: PreEncoder
emptyPreEncoder = PreEncoder M.empty M.empty [] M.empty M.empty []
-- 
data PreEncoder = PreEncoder { argToPar :: M.Map (PC,Int,Value) Id   -- Map an argument to a parameter -- Do not support calling the same function twice. New fresh variables
                             , fStore   :: M.Map Id (Type, [PC])     -- Map a global variable to a list of program counter that store a new value
                             , mutexes  :: [Id]
                             , sortEnv  :: TypeEnv                     -- Map all the types to a sort expression and a sort name
                             , locals   :: M.Map Id Type             -- Map all identifiers to a type
                             , fails    :: [PC]                        -- List of program counters that call assert_fail
                             }

instance Show PreEncoder where
  show (PreEncoder a fs m s l f) = "PreEncoder\n" ++ "-------------\n" 
                             ++ show a ++ "\n--------------\n" 
                             ++ show fs ++ "\n--------------\n" 
                             ++ show m ++ "\n--------------\n" 
                             ++ show s ++ "\n-----------------\n"
                             ++ show l ++ "\n-----------------\n"
                             ++ show f
