-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.ControlFlow
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.ControlFlow where

import Language.LLVMIR

import qualified Data.Map as M

-- TODO: Join back the threads to the function that called them
type CF = [(Int,Int)]

{-
data CS = ReadS
        | WriteS
        | Wait
-}

data ControlFlow = 
	ControlFlow { cte :: M.Map Identifier Int    -- ^ Entry Point for Ti
                , cfg :: M.Map Identifier CF     -- ^ Control Flow for Ti
            --  , cgv  :: Map.Map Id ([Int], [Int]) --  ^ Data Flow Points where we use global variables - The pair ([Int],[Int]) is for Read and Write
                }

eCFG :: ControlFlow 
eCFG = ControlFlow M.empty M.empty