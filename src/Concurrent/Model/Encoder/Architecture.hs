-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder.Architecture
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Encoder.Architecture where

import Concurrent.Model.Analysis.SystemC
import Language.SMTLib2.Base
import Language.SMTLib2.Builder

encodeSCarch :: SCArch -> [SExpression]
encodeSCarch (SCArch mods comm) = concatMap encodeSCmod mods

encodeSCmod :: SCMod -> [SExpression]
encodeSCmod (SCMod n _ _ vars _) = let sn = "struct."++n
                                   in concatMap (encodeSCvar sn) $ zip [1..] vars

encodeSCvar :: String -> (Int, SCModVar) -> [SExpression]
encodeSCvar sc (n, v) = let scn = SimpleSym $ sc ++ "_" ++ show n
      
