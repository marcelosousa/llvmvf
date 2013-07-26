{-#LANGUAGE UnicodeSyntax, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.Instruction
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.Instruction where

import Language.LLVMIR hiding (Switch)
import Language.LLVMIR.Util
import Concurrent.Model.Analysis.ControlFlow
import Concurrent.Model.Analysis.DataFlow
import Concurrent.Model.Analysis.Context
import Concurrent.Model.Analysis.Util
import qualified Data.Map   as M
import qualified Data.Maybe as MB

analyseTmn ∷ Terminator → Context ()
analyseTmn tmn = do
  e@Env{..} ← getEnv
  let l@Location{..} = ploc
  case tmn of
    -- Terminators
    Ret pc v -> do let l' = Location fn bb pc False
                       c  = flow pc ploc ccfg
                       el = ExitLoc l' EndFn
                       el' = updateLocs l' el efloc
                       e' = e {ccfg = c, ploc = l', efloc = el'}
                   putEnv e'
                   analyseRetValue v
    Unreachable pc -> let l' = Location fn bb pc False
                          c  = flow pc ploc ccfg
                          el = ExitLoc l' EndFn
                          el' = updateLocs l' el efloc
                          e' = e {ccfg = c, ploc = l', efloc = el'}
                      in putEnv e'
    Br  pc v t f -> do let ti = valueIdentifier' "" t
                           fi = valueIdentifier' "" f
                           l' = Location fn bb pc False
                           c  = flow pc ploc ccfg
                           elt = ExitLoc l' $ BBLoc ti
                           elf = ExitLoc l' $ BBLoc fi
                           el = updateLocs l' elt $ updateLocs l' elf efloc
                           e' = e {ccfg = c, ploc = l', efloc = el}
                       putEnv e'
                       analyseValue v
    UBr pc d -> let di = valueIdentifier' "" d
                    l' = Location fn bb pc False
                    c  = flow pc ploc ccfg
                    el = updateLocs l' (ExitLoc l' $ BBLoc di) efloc
                    e' = e {ccfg = c, ploc = l', efloc = el}
                in putEnv e'


-- Phi Instructions
analysePHI ∷ PHI → Context ()
analysePHI (PHI pc i ty vals) = 
  let (v,t) = unzip vals
  in analyseNInstr pc [ty] $ v ++ t 

analyseInstr :: Instruction -> Context ()
analyseInstr i = do 
  e@Env{..} <- getEnv
  let l@Location{..} = ploc
  case i of
    -- Call Operation
    Call pc i ty callee vs -> do let l' = Location fn bb pc False
                                     c  = flow pc ploc ccfg
                                     el = updateLocs l'  (ExitLoc l' $ FnLoc callee) efloc
                                     e' = e {ccfg = c, ploc = l', efloc = el}
                                 putEnv e'
                                 mapM_ analyseValue vs
                                 analyseType ty
    -- Standard Binary Operations
    -- Integer Operations
    Add  pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    Sub  pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    Mul  pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    UDiv pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    SDiv pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    URem pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    SRem pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    -- Bitwise Binary Operations
    Shl  pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    LShr pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    AShr pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    And  pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    Or   pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    Xor  pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2] 
    -- Float Operations
    FAdd pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2]
    FSub pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2]
    FMul pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2]
    FDiv pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2]
    FRem pc i ty op1 op2 -> analyseNInstr pc [ty] [op1,op2]
    -- Cast Operations
    Trunc    pc i v ty -> analyseNInstr pc [ty] [v]
    ZExt     pc i v ty -> analyseNInstr pc [ty] [v]
    SExt     pc i v ty -> analyseNInstr pc [ty] [v]
    FPTrunc  pc i v ty -> analyseNInstr pc [ty] [v]
    FPExt    pc i v ty -> analyseNInstr pc [ty] [v]
    FPToUI   pc i v ty -> analyseNInstr pc [ty] [v]
    FPToSI   pc i v ty -> analyseNInstr pc [ty] [v]
    UIToFP   pc i v ty -> analyseNInstr pc [ty] [v]
    SIToFP   pc i v ty -> analyseNInstr pc [ty] [v]
    PtrToInt pc i v ty -> analyseNInstr pc [ty] [v]
    IntToPtr pc i v ty -> analyseNInstr pc [ty] [v]                                    
    BitCast  pc i v ty -> analyseNInstr pc [ty] [v] 
    -- Other Operations
    ICmp pc i cond ty op1 op2 -> analyseNInstr pc [ty] [op1,op2]
    FCmp pc i cond ty op1 op2 -> analyseNInstr pc [ty] [op1,op2]
    -- Selection Operations
    Select pc i cv vt vf       -> analyseNInstr pc [] [cv,vt,vf]
    ExtractValue pc i τ v idxs -> analyseNInstr pc [] [v]
    InsertValue pc i v vi idxs -> analyseNInstr pc [] [v,vi]
    -- Memory Operations
    Alloca pc i ty       align   -> analyseNInstr pc [ty] []
    Store  pc   ty v1 v2 align   -> analyseNInstr pc [ty] [v1,v2]
    Load   pc i    v     align   -> do analyseNInstr pc []   [v]
                                       o@Env{..} <- getEnv
                                       let iv = infoValue v
                                           d@DataFlow{..} = df
                                           d' = d { loadMap = updateLoadMap fn (i,iv) loadMap }
                                           o' = o { df = d' }
                                       putEnv o' 
    GetElementPtr pc i ty v idxs -> analyseNInstr pc [ty] (v:idxs)
    -- Atomic Operations
    Cmpxchg   pc i mptr cval nval ord -> analyseNInstr pc [] [mptr,cval,nval]
    AtomicRMW pc i mptr val op ord -> analyseNInstr pc [] [mptr,val]
    -- Concurrent Operations Added
    CreateThread pc args -> do let ti = infoValue $ args !! 0 
                                   t = valueIdentifier' "" $ args !! 2 
                                   l' = Location fn bb pc False
                                   c  = flow pc ploc ccfg
                                   el = ExitLoc l' $ ThLoc t
                                   el' = updateLocs l' el efloc
                                   ths = updateThreads fn (ti,t) threads
                                   e' = e {ccfg = c, ploc = l', efloc = el', threads = ths}
                               putEnv e'
                               mapM_ analyseValue args  
    JoinThread   pc i -> do let l' = Location fn bb pc False
                                c  = flow pc ploc ccfg
                                ti = findThread fn i df threads
                                el = SyncLoc l' ti
                                el' = updateLocs l' el efloc
                                e' = e {ccfg = c, ploc = l', efloc = el'}
                            putEnv e'
    ExitThread   pc -> do let l' = Location fn bb pc False
                              c  = flow pc ploc ccfg
                              el = ExitLoc l' $ EndTh fn
                              el' = updateLocs l' el efloc
                              e' = e {ccfg = c, ploc = l', efloc = el'}
                          putEnv e'
    -- Mutexes
    MutexInit    pc i mutex -> analyseNInstr pc [] [mutex]    
    MutexLock    pc i mutex -> analyseNInstr pc [] [mutex]
    MutexUnlock  pc i mutex -> analyseNInstr pc [] [mutex]
    MutexDestroy pc i mutex -> analyseNInstr pc [] [mutex]
    -- Conditional Variables
    CondInit     pc i cond  -> analyseNInstr pc [] [cond]    
    CondWait     pc i cond mutex -> analyseNInstr pc [] [cond,mutex]    
    CondSignal   pc i cond -> analyseNInstr pc [] [cond]    
    _ -> error $ "analyseInstr: " ++ show i ++ " not supported."    
{-    WaitEvent    pc :: PC event :: Int
    NotifyEvent  pc :: PC event :: Int
    WaitTime     pc :: PC time :: Value    
-}


analyseRetValue :: RetInst -> Context ()
analyseRetValue VoidRet = return ()
analyseRetValue (ValueRet v) = analyseValue v

analyseValue :: Value -> Context ()
analyseValue v = return ()

analyseType :: Type -> Context ()
analyseType ty = return ()

analyseNInstr :: PC -> [Type] -> [Value] -> Context ()
analyseNInstr pc ty v = do
    e@Env{..} <- getEnv
    let l@Location{..} = ploc
        l' = Location fn bb pc False
        c  = flow pc ploc ccfg
        e' = e {ccfg = c, ploc = l'}
    putEnv e'
    mapM_ analyseValue v
    mapM_ analyseType ty
	
flow :: PC -> Location -> ControlFlow -> ControlFlow
flow pc l@Location{..} cfg = 
    if ise
    then cfg
    else (Intra lpc pc):cfg                          

iflow :: PC -> Location -> ControlFlow -> ControlFlow
iflow pc l@Location{..} cfg = (Inter lpc pc):cfg
    
tflow :: PC -> Location -> ControlFlow -> ControlFlow
tflow pc l@Location{..} cfg = (Switch lpc pc):cfg
