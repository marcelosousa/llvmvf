{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Asm.Lift
-- Copyright :  (c) 2013 Marcelo Sousa
-- Inline Asm 
-------------------------------------------------------------------------------

module Analysis.Asm.Lift(liftAsm) where

import Language.LLVMIR
import Language.LLVMIR.Util
import Language.Asm

import qualified Data.Map as M
import qualified Data.Set as S
import Prelude.Unicode ((⧺),(≡))

import Control.Monad
import Control.Applicative
import Control.Monad.State

(↣) ∷ (Monad m) ⇒ a → m a
(↣) = return

type Id = Identifier

type ΕState α = State Ε α

-- Environment
data Ε = Ε 
	{ 
	  fn ∷ (Id, Int)   -- Current Function
	, names ∷ S.Set Id -- Names
	, asmfn ∷ Functions -- Created Functions
  	}

-- update the function in the
-- environment
νfn ∷ (Id,Int) → ΕState ()
νfn n = do γ@Ε{..} ← get
           put γ{fn = n}

δfn ∷ ΕState (Id,Int)
δfn = do γ@Ε{..} ← get
         (↣) fn

δnames ∷ ΕState (S.Set Id)
δnames = do γ@Ε{..} ← get
         (↣) names

freshName ∷ ΕState Id
freshName = undefined

εΕ ∷ Module → Ε
εΕ m = E (Global "",0) (getNames m) M.empty

-------------------------------------------------------------------------------
liftAsm ∷ Module → Module
liftAsm mod@(Module id layout target gvs fns nmdtys) = 
	let fns' = M.fromList $ evalState (lift $ M.toList) (εΕ m)
	in Module id layout target gvs fns' nmdtys

class Assembly α where
	lift ∷ α → ΛState

liftList ∷ (Assembly α) ⇒ [(Identifier,Function)] → [α] → ΛState
liftList = foldM liftElem

liftElem ∷ (Assembly α) ⇒ [(Identifier,Function)] → α → ΛState
liftElem β α = do α' ← lift α
                  (↣) $ α' ⧺ β

instance Assembly [(Identifier,Function)] where
	lift = liftList []

instance Assembly (Identifier,Function) where
	lift (i,fn) = case fn of
		FunctionDecl name linkage retty isVar params     → (↣) [(i,fn)]
		FunctionDef  name linkage retty isVar params bbs → do

			let asmbbs = map (liftAsmBB name) bbs
		    	(bbs',asmfns) = unzip asmbbs
		    	fn' = FunctionDef  name linkage retty isVar params bbs'
			in [(i,fn')] ⧺ concat asmfns

liftAsmBB ∷ Identifier → BasicBlock → (BasicBlock,[(Identifier,Function)])
liftAsmBB name bb = case bb of
	BasicBlock label phis instrs tmn → 
		let asminstr = map (liftAsmInstr name) instrs
		    (instrs',asmis) = unzip asminstr
		    bb' = BasicBlock label phis instrs' tmn
		in (bb',concat asmis)

liftAsmInstr ∷ Identifier → Instruction → (Instruction,[(Identifier,Function)])
liftAsmInstr name i = case i of
	InlineAsm pc α τ _ _ _ asm constr args → 
		let fname = freshName undefined
		    fncall = Call pc α τ fname args
		    fn = function fname τ asm args
		in (fncall,[(fname,fn)])
	_ → (i,[])

