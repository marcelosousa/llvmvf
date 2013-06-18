{-# LANGUAGE UnicodeSyntax, RecordWildCards, FlexibleInstances #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Asm.Lift
-- Copyright :  (c) 2013 Marcelo Sousa
-- Inline Asm 
-------------------------------------------------------------------------------

module Analysis.Asm.Lift(liftAsm) where

import Language.LLVMIR hiding (Id)
import Language.LLVMIR.Util
import Language.Asm

import qualified Data.Map as M
import qualified Data.Set as S
import Prelude.Unicode ((⧺),(≡))

import Control.Monad
import Control.Applicative
import Control.Monad.State hiding (lift)

(↣) ∷ (Monad m) ⇒ a → m a
(↣) = return

-- A bit of unicode non-sense
(∪) ∷ Ord κ ⇒ M.Map κ α → M.Map κ α → M.Map κ α
(∪) = M.union

ε ∷ Ord κ ⇒ M.Map κ α
ε = M.empty

type Id = Identifier

type ΕState α = State Ε α

-- Environment
data Ε = Ε 
	{ fn ∷ (Id, Int)   -- Current Function
	, names ∷ S.Set Id -- Names
	, asmfn ∷ Functions -- Created Functions
  	}

-- update the function in the
-- environment
νfn ∷ (Id,Int) → ΕState ()
νfn n = do γ@Ε{..} ← get
           put γ{fn = n}

νasmfn ∷ (Id,Function) → ΕState ()
νasmfn (i,f) = do γ@Ε{..} ← get
                  let fns = M.insert i f asmfn
                  put γ{asmfn = fns}

δfn ∷ ΕState (Id,Int)
δfn = do γ@Ε{..} ← get
         (↣) fn

δnames ∷ ΕState (S.Set Id)
δnames = do γ@Ε{..} ← get
            (↣) names

freshName ∷ ΕState Id
freshName = undefined

εΕ ∷ Module → Ε
εΕ m = Ε (Global "",0) (getNames m) ε

-------------------------------------------------------------------------------
liftAsm ∷ Module → Module
liftAsm m@(Module id layout target gvs fns nmdtys) = 
	let (f,α@Ε{..}) = runState (liftList [] $ M.toList fns) $ εΕ m
	    fns' = asmfn ∪ M.fromList f
	in Module id layout target gvs fns' nmdtys

class Assembly α where
	lift ∷ α → ΕState α

liftList ∷ Assembly α ⇒ [α] → [α] → ΕState [α]
liftList = foldM liftElem

liftElem ∷ Assembly α ⇒ [α] → α → ΕState [α]
liftElem β α = do α' ← lift α
                  (↣) $ α':β

instance Assembly (Identifier,Function) where
	lift (i,fn) = case fn of
		FunctionDecl name linkage retty isVar params     → (↣) (i,fn)
		FunctionDef  name linkage retty isVar params bbs → do
			νfn (i,0)
			bbs' ← mapM lift bbs
			let fn' = FunctionDef name linkage retty isVar params bbs'
			(↣) (i,fn')

instance Assembly BasicBlock where
	lift bb = case bb of
		BasicBlock label phis instrs tmn → do
			instrs' ← mapM lift instrs
			(↣) $ BasicBlock label phis instrs' tmn

instance Assembly Instruction where
	lift i = case i of
		InlineAsm pc α τ _ _ _ asm constr args → do
			fname ← freshName		    
			let fn = buildFn fname τ asm args
			νasmfn (fname,fn)
			(↣) $ Call pc α τ fname args
		_ → (↣) i

buildFn ∷ Id → Type → Asm → Values → Function
buildFn = undefined