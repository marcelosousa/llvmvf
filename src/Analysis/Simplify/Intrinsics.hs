{-# LANGUAGE UnicodeSyntax, RecordWildCards, FlexibleInstances, DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Asm.Lift
-- Copyright :  (c) 2013 Marcelo Sousa
-- Remove intrinsic debug calls to simplify
-------------------------------------------------------------------------------

module Analysis.Simplify.Intrinsics(liftDebug) where

import Language.LLVMIR hiding (Id)
import qualified Language.LLVMIR as IR

import qualified Data.Map as M

class LiftDebug α where
	liftDebug ∷ α → α

instance LiftDebug Module where
	liftDebug m@(Module id layout target gvs fns nmdtys) = 
		let fns' = M.map liftDebug fns
		in Module id layout target gvs fns' nmdtys

instance LiftDebug Function where
	liftDebug fn = case fn of
		FunctionDecl name linkage retty isVar params     → fn
		FunctionDef  name linkage retty isVar params bbs →
			let bbs' = map liftDebug bbs
			in FunctionDef name linkage retty isVar params bbs'

instance LiftDebug BasicBlock where
	liftDebug bb = case bb of
		BasicBlock label phis instrs tmn → 
			let instrs' = foldr liftDebugI [] instrs
			in BasicBlock label phis instrs' tmn

liftDebugI ∷ Instruction → Instructions → Instructions
liftDebugI (Call _ _ _ (Global "llvm.dbg.value") _) is = is
liftDebugI (Call _ _ _ (Global "llvm.dbg.declare") _) is = is
liftDebugI i is = i:is 