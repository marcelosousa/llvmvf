{-#LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.ControlFlow
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.ControlFlow where

import qualified Data.Map as M

import Language.LLVMIR
import UU.PPrint

data Flow = Intra   Identifier Int Int
          | Inter   Identifier Int Int
          | CSwitch Identifier Int Int
  deriving Show
  
type ControlFlow = [Flow]
--data ControlFlow = 
--	ControlFlow { cte ∷ M.Map Identifier Int    -- Entry label for a function
--	            , cfg ∷ M.Map Identifier [Flow] -- Intra-procedural cfg
--	            }

eCFG ∷ ControlFlow 
eCFG = []

outFlow ∷ Flow → Int
outFlow (Intra   _ _ pc) = pc
outFlow (Inter   _ _ pc) = pc
outFlow (CSwitch _ _ pc) = pc

nextpc :: PC -> ControlFlow -> [PC]
nextpc i = Prelude.map outFlow . Prelude.filter (nextpcStep i)

nextpcStep ∷ PC → Flow → Bool
nextpcStep i f = case f of
  Intra _ a b   -> step (a,b)
  Inter _ a b   -> step (a,b)
  CSwitch _ a b -> step (a,b)
  where step (p,r) = p == i && r /= -1

instance Pretty Flow where
  pretty (Intra   _ a b) = int a <+> text "->" <+> int b <> char ';'
  pretty (Inter   _ a b) = int a <+> text "->" <+> int b <> char ';'
  pretty (CSwitch _ a b) = int a <+> text "->" <+> int b <> char ';'
