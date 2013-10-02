{-# LANGUAGE UnicodeSyntax, RecordWildCards, FlexibleInstances, DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Metadata.Call
-- Copyright :  (c) 2013 Marcelo Sousa
-- Collect frequent function calls
-------------------------------------------------------------------------------

module Analysis.Metadata.Call where

import Language.LLVMIR hiding (Id)
import qualified Language.LLVMIR as IR
import UU.PPrint
import Language.LLVMIR.Printer

import qualified Data.Map as M
import Data.List
import Data.Ord

type Info = M.Map Identifier Int


debugInfo :: Info -> Module -> Info
debugInfo r m@(Module id layout target gvs fns nmdtys) = M.fold debugInfoFunction r fns 

debugInfoFunction :: Function -> Info -> Info
debugInfoFunction fn r = case fn of
	FunctionDecl name linkage retty isVar params     → r
	FunctionDef  name linkage retty isVar params bbs →
		foldr debugInfoBasicBlock r bbs

debugInfoBasicBlock :: BasicBlock -> Info -> Info
debugInfoBasicBlock bb r = case bb of
	BasicBlock label phis instrs tmn → 
		foldr debugInfoInstruction r instrs

debugInfoInstruction :: Instruction -> Info -> Info
debugInfoInstruction i r = case i of
	Call _ _ _ n _ -> M.alter inc n r
        _ -> r

inc :: Maybe Int -> Maybe Int
inc Nothing = Just 1
inc (Just i) = Just $ i + 1

infoToString :: Int -> Info -> String
infoToString n m = let s = foldr aux "" $ sortBy (comparing snd) $ M.toAscList m
	   	   in s ++ (show $ statistics m)
  where aux (k,s) r = if isTmp k || s < n 
                      then r
                      else show (pretty k) ++ ": " ++ show s ++ "\n" ++ r

statistics :: Info -> (Int,Int)
statistics m = foldr auxS (0,0) $ M.toList m
 
auxS (k,s) (a,b) = 
	if isTmp k
	then (a+s,b)
	else (a,s+b)

isTmp :: Identifier -> Bool
isTmp (Global k) = take 3 k == "tmp"
isTmp _ = error "isTmp"
