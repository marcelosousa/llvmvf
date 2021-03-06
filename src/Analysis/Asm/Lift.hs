{-# LANGUAGE UnicodeSyntax, RecordWildCards, FlexibleInstances, DoAndIfThenElse #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Asm.Lift
-- Copyright :  (c) 2013 Marcelo Sousa
-- Inline Asm 
-------------------------------------------------------------------------------

module Analysis.Asm.Lift(liftAsm) where

import Language.LLVMIR hiding (Id)
import qualified Language.LLVMIR as IR
import Language.LLVMIR.Util
import qualified Language.Asm as AS

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Prelude.Unicode ((⧺),(≡))

import Control.Monad
import Control.Applicative
import Control.Monad.State hiding (lift)

(↣) ∷ (Monad m) ⇒ a → m a
(↣) = return

(∘) :: Ord α ⇒ α → S.Set α → S.Set α
(∘) = S.insert

(∈) ∷ Ord α ⇒ α → S.Set α → Bool
(∈) = S.member

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

νname ∷ Id → ΕState ()
νname i = do γ@Ε{..} ← get
             let n = i ∘ names
             put γ{names=n}

δfn ∷ ΕState (Id,Int)
δfn = do γ@Ε{..} ← get
         (↣) fn

δnames ∷ ΕState (S.Set Id)
δnames = do γ@Ε{..} ← get
            (↣) names

buildName ∷ Id → Int → Id
buildName (Global s) n = Global $ s ⧺ show n
buildName (Local _) _ = error "buildName: Local given"

freshName ∷ ΕState Id
freshName = do γ@Ε{..} ← get
               let n = uncurry buildName fn
                   fn' = (fst fn,(snd fn) + 1)
               νfn fn'
               if n ∈ names
               then freshName
               else do νname n
                       (↣) n

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

-- Try to Lift and change to False if cant
instance Assembly Instruction where
	lift i = case i of
		InlineAsm pc α τ True _ _ _ asm constr args → do
			fname ← freshName		    
			let fn = buildFn fname τ asm constr args
			νasmfn (fname,fn)
			(↣) $ Call pc α τ fname args
		_ → (↣) i

-------------------------------------------------------------------------------
analyzeConstr ∷ AS.AsmCs → (M.Map String Int, [Int], [Int])
analyzeConstr s = let (m,l,i,_) = foldl analyzeC (M.empty,[],[],0) s
                  in (m,l,i)

analyzeC ∷ (M.Map String Int,[Int],[Int],Int) → AS.AsmC → (M.Map String Int, [Int], [Int],Int)
analyzeC r (AS.IC gasC) = analyzeGasC gasC r 
analyzeC r (AS.OC gasC) = analyzeOutC gasC r
analyzeC r (AS.FC _)    = r

analyzeOutC ∷ AS.GasC → (M.Map String Int, [Int], [Int], Int) → (M.Map String Int, [Int], [Int], Int)
analyzeOutC gasc (mri,lpos,imr,n) = case gasc of
	AS.MemC → (mri,(lpos⧺[n]),imr,n+1)
	AS.PosC p  → if p `elem` lpos
		         then let lpos' = delete p lpos
		              in (mri,lpos'⧺[p],imr,n)
		         else (mri,lpos⧺[p],imr,n)
	AS.CRegC s → (M.insert s n mri,lpos,imr,n+1)
	_ → (mri,lpos,imr,n+1)

analyzeGasC ∷ AS.GasC → (M.Map String Int, [Int], [Int], Int) → (M.Map String Int, [Int], [Int], Int)
analyzeGasC gasc (mri,lpos,imr,n) = case gasc of
	AS.PosC p  → if p `elem` lpos
		         then let lpos' = delete p lpos
		              in (mri,lpos'⧺[p],imr,n)
		         else (mri,lpos⧺[p],imr,n)
	AS.CRegC s → (M.insert s n mri,lpos,imr,n+1)
	AS.IRegC → (mri,(lpos⧺[n]),n:imr,n+1)
	c       → (mri,(lpos⧺[n]),imr,n+1)

data Γ = Γ {
	  vars    ∷ M.Map Id Value
	, lastVar ∷ Maybe Value
	, counter ∷ Int -- Num of bbs
	, locals  ∷ S.Set Id
	, mri     ∷ M.Map String Int
	, imr     ∷ [Int]
}

εΓ ∷ Parameters → M.Map String Int → [Int] → Γ
εΓ p mri imr = let ip = S.fromList $ map (\(Parameter i _ ) → i) p
                   vars = foldr (\(Parameter i τ) m → M.insert i (IR.Id i τ) m) ε p
               in Γ vars Nothing 0 ip mri imr

freshLocal ∷ State Γ Id
freshLocal = do
	γ@Γ{..} ← get
	let tmp = Local $ "tmp" ⧺ (show $ S.size locals)
	    locals' = tmp ∘ locals
	put γ{locals = locals'}
	(↣) tmp

buildFn ∷ Id → Type → AS.Asm → AS.AsmCs → Values → Function
buildFn n τ asm constr vals = 
	let (mri,lpos,imr) = analyzeConstr constr
	    params = if length lpos ≡ length vals
	    	     then map buildParam $ zip vals lpos
	    	     else error $ "buildFn: length of lists is different " ⧺ show lpos ⧺ show vals
	    bbs = evalState (buildBody asm) $ εΓ params mri imr
	in FunctionDef n PrivateLinkage τ False params bbs

buildParam ∷ (Value,Int) → Parameter
buildParam (v,i) = Parameter (Local $ show i) $ typeOf v

buildBody ∷ AS.Asm → State Γ BasicBlocks
buildBody (_,[]) = (↣) $ [BasicBlock (Local "bb") [] [] (Ret 0 VoidRet)]
buildBody (_,[(Nothing,[AS.Bug])]) = (↣) $ [BasicBlock (Local "bb") [] [] (Unreachable 0)]
buildBody (_,sections) = mapM (buildBB . snd) sections

buildBB ∷ [AS.GAS] → State Γ BasicBlock
buildBB instr = do
	bbname ← buildBBName
	instrs ← foldM buildInstruction [] instr
	tmn ← buildTerminator
	(↣) $ BasicBlock bbname [] instrs tmn

buildBBName ∷ State Γ Id
buildBBName = do 
    γ@Γ{..} ← get
    let name = if counter ≡ 0
    	       then Local "bb"
    	       else Local $ "bb" ⧺ show counter
        c = counter + 1
    put γ{counter=c}
    (↣) name

buildTerminator ∷ State Γ Terminator
buildTerminator = do 
	γ@Γ{..} ← get
	case lastVar of
		Nothing → (↣) $ Ret 0 VoidRet
		Just α  → (↣) $ Ret 0 $ ValueRet α
  
buildInstruction ∷ Instructions → AS.GAS → State Γ Instructions
buildInstruction is i = 
 case i of
	AS.Add τ' α β → do
		ιs ← buildBinOp Add (τGas2τ τ') α β
		(↣) $ ιs⧺is
	AS.Sub τ' α β → do
		ιs ← buildBinOp Sub (τGas2τ τ') α β
		(↣) $ ιs⧺is
	AS.Mov τ' α β → do
		γ@Γ{..} ← get
		let τ = τGas2τ τ'
		αv ← buildValue τ α
		βv ← buildValue τ β
		let βi = valueIdentifier' "" βv
		case typeOf βv of
			TyPointer τ' → do
				let si = Store 0 TyVoid αv βv (Align 8)
				(↣) $ si:is
			_ → case typeOf αv of
				TyPointer τ' → do
					case M.lookup βi vars of
						Nothing → do 
							let si = Load 0 βi αv (Align 8)
							put γ{lastVar = Just (IR.Id βi τ')}
							(↣) $ si:is
						Just v  → undefined
				_ → do 
					let βi = valueIdentifier' "" βv
					    vars' = M.insert βi αv vars
					put γ{vars=vars', lastVar = Just αv}
					(↣) is
	AS.Cmpxchg τ α β → do
		ι ← buildCmpxchg (τGas2τ τ) α β 
		(↣) $ ι:is
	AS.Xchg τ' α β → do
		ι ← buildXchg (τGas2τ τ') α β
		(↣) $ ι:is
	AS.Xadd τ' α β → do
		ιs ← buildXAdd (τGas2τ τ') α β
		(↣) $ ιs⧺is
	AS.Sete α → (↣) is
	AS.Bswap τ' α → do
		let τ = τGas2τ τ'
		    fn = case τ of
		    	TyInt 16 → Global "llvm.bswap.i16"
		    	TyInt 32 → Global "llvm.bswap.i32"
		    	TyInt 64 → Global "llvm.bswap.i64"
		αv ← buildValue τ α
		αi ← ssaValue αv
		γ@Γ{..} ← get
		let αι = valueIdentifier' "" αi
		    ι = Call 0 αι τ fn [αv]
		put γ{lastVar = Just αi}
		(↣) $ ι:is
	AS.Canary τ' α → do
		let τ = τGas2τ τ'
		αv ← buildValue τ α
		let τβ = case typeOf αv of
			TyPointer τ1 → τ1
			_ → error "AS.Canary"
		βi ← freshLocal
		let βv = (IR.Id βi τβ)
		    li = Load 0 βi αv (Align 8)
		γ@Γ{..} ← get
		put γ{lastVar = Just βv}
		(↣) $ li:is
--	AS.Bt  τ' α β → do
--		let τ = τGas2τ τ'
--		αv ← buildValue undefined α -- actually incorrect
--		βv ← buildValue τ β
	AS.Sbb τ' (AS.Reg "0") (AS.Reg "0") → do
		let τ = τGas2τ τ'
		γ@Γ{..} ← get
		let α = case M.lookup (Local "0") vars of
				Nothing → if null imr
						  then IR.Id (Local "0") τ
						  else let x = show $ head imr
							   in case M.lookup (Local x) vars of
							      Nothing → IR.Id (Local x) τ
							      Just v  → v
				Just v  → v
		let τ1 = typeOf α
		βj ← freshLocal
		let β = IR.Id βj τ1
		γ@Γ{..} ← get
		put γ{lastVar = Just β}
		let ι = Add 0 βj τ1 α α
		(↣) $ ι:is
	_ → (↣) is

type BinOpTy = (Int → Id → Type → Value → Value → Instruction)

buildBinOp ∷ BinOpTy → Type → AS.Operand → AS.Operand → State Γ Instructions
buildBinOp op τ α β@(AS.Reg _) = do
	αv ← buildValue τ  α
	βv ← buildValue τ β
	case typeOf βv of
		TyInt n → do
			βi ← freshLocal
			γ@Γ{..} ← get
			let βvi = valueIdentifier' "" βv
			    vars' = M.insert βvi (IR.Id βi τ) vars
			put γ{vars = vars'}
			(↣) $ [op 0 βi τ αv βv]
		TyPointer τ' → do
			βii ← freshLocal
			let βi = (IR.Id βii τ)
			    li = Load 0 βii βv (Align 8)
			j ← freshLocal
			let ai = op 0 j τ αv βi
			    si = Store 0 TyVoid (IR.Id j τ) βv (Align 8)
			(↣) $ [li,ai,si]

buildXchg ∷ Type → AS.Operand → AS.Operand → State Γ Instruction
buildXchg τ α@(AS.Reg _) ρ@(AS.Reg _) = do
	γ@Γ{..} ← get
	let τptr = TyPointer τ
	αv ← buildValue τ α
	ρv ← buildValue τptr ρ
	j ← freshLocal
	let βj = IR.Id j τ
	γ@Γ{..} ← get
	put γ{lastVar = Just βj}
	(↣) $ AtomicRMW 0 j ρv αv OpXchg Monotonic
buildXchg τ _ _ = error "buildXchg"
	    

buildXAdd ∷ Type → AS.Operand → AS.Operand → State Γ Instructions
buildXAdd τ α β@(AS.Reg _) = do
	αv ← buildValue τ  α
	βv ← buildValue τ β
	case typeOf βv of
		TyInt n → error "error buildXAdd: expecting TyPointer"
		TyPointer τ' → do
			βii ← freshLocal
			let βi = (IR.Id βii τ)
			γ@Γ{..} ← get
			put γ{lastVar = Just βi}
			(↣) $ [AtomicRMW 0 βii βv αv OpAdd Monotonic]
{-
			    li = Load 0 βii βv (Align 8)
			j ← freshLocal
			let ai = Add 0 j τ αv βi
			    si = Store 0 TyVoid (IR.Id j τ) βv (Align 8)
			γ@Γ{..} ← get
			put γ{lastVar = Just βi}
			(↣) $ [li,ai,si]
-}

buildCmpxchg ∷ Type → AS.Operand → AS.Operand → State Γ Instruction
buildCmpxchg τ n ρ@(AS.Reg ptr) = do
	γ@Γ{..} ← get
	let τptr = TyPointer τ
	    α = case M.lookup "eax" mri of
	    		Nothing → error "buildCmpxchg: no eax"
	    		Just αi → AS.Reg $ show αi 
	nv ← buildValue τ n
	ρv ← buildValue τptr ρ
	αv ← buildValue τ α
	j ← freshLocal
	let βj = IR.Id j τ
	γ@Γ{..} ← get
	put γ{lastVar = Just βj}
	(↣) $ Cmpxchg 0 j ρv αv nv Monotonic
buildCmpxchg τ n _ = error "buildCmpxchg"


buildValue ∷ Type → AS.Operand → State Γ Value
buildValue τ (AS.Lit n) = (↣) $ Constant $ SmpConst $ ConstantInt n τ
buildValue τ (AS.Reg s) = do γ@Γ{..} ← get
                             let i = Local s
                             case M.lookup i vars of
                             	Nothing → (↣) $ IR.Id i τ
                             	Just v  → (↣) v
buildValue τ (AS.CReg s) = error "buildValue: does not support clobber registers"

ssaValue ∷ Value → State Γ Value
ssaValue (IR.Id i τ)  = do j ← freshLocal
                           γ@Γ{..} ← get
                           let v = IR.Id j τ
                               vars' = M.insert i v vars
                           put γ{vars=vars'}
                           (↣) v 
ssaValue (Constant c) = do i ← freshLocal
                           (↣) $ IR.Id i (typeOf c)

τGas2τ ∷ AS.TyGas → Type
τGas2τ (AS.I n) = TyInt n
τGas2τ (AS.Fp n) = TyFloatPoint $ fpτGas2τ n

fpτGas2τ ∷ Int → TyFloatPoint
fpτGas2τ 16  = TyHalf
fpτGas2τ 32  = TyFloat
fpτGas2τ 64  = TyDouble
fpτGas2τ 128 = TyFP128
fpτGas2τ 80  = Tyx86FP80
fpτGas2τ _   = error "fpτGas2τ" 