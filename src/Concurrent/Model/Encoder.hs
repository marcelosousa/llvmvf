{-#LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Encoder (encode) where

import Concurrent.Model.Analysis.ControlFlow (ControlFlow(..))
import Concurrent.Model.Encoder.Model -- hiding (GlobalState,Transition) 
import Concurrent.Model               hiding (State)

import Language.LLVMIR

import Language.SMTLib2.Base
import Language.SMTLib2.Builder

import qualified Data.IntMap as IM
import qualified Data.Map as Map

import Data.Maybe

import Debug.Trace (trace)

import Control.Monad.State
 
-- In general, the problem of verifying two-threaded programs
-- (with unbounded stacks) is undecidable.
-- Constraint the scheduler by guard stregthening

-- To guarantee correctness, the scheduler must allow context-switch
-- that are conflicting, i.e., accesses whose relative execution
-- order can produce different global system states. 
-- 1. Persistent/ample set computations.
-- 2. Lock-set and/or lock-acquisition history analysis
-- 3. Conditional dependency
--class (SCModel t) => Encode t where

--type GlobalState = (Map.Map String (PC, Map.Map Id Value), Map.Map Id Value, PC)

--type Transition = (PC, Bool, SExpression, GlobalState -> GlobalState, PC) -- (PC, a -> Bool, SExpr -> SExpr, PC)

encode :: (SCModel t) => Model t -> SMod
encode m@Model{..} = let ccfg@ControlFlow{..} = controlflow m
                         cme = fromMaybe (error "encode") $ Map.lookup "main" cte
                         tvs = Map.map (\pci -> (pci, Map.empty)) $ Map.delete "main" cte
                         s0  = (tvs, Map.empty, cme) :: GlobalState
                         (smod, sf) = runState (encModel m ccfg) s0
                     in trace (show s0 ++ show sf) $ smod

encModel :: (SCModel t) => Model t -> ControlFlow -> State GlobalState SMod
encModel m ccfg = do eg <- encGlobals m
                     em <- encMain    m ccfg
                     et <- return [] --  encProcs   m ccfg
                     return $ preamble ++ eg ++ em ++ et ++ final

preamble :: [SExpression]
preamble = [ setlogic QF_AUFBV
           , setoption "produce-models"
           ]

final :: [SExpression]
final = [ checksat , exit ]

encGlobals :: (SCModel t) => Model t -> State GlobalState [SExpression]
encGlobals m@Model{..} = do gs <- get
                            let gw = wrap_Globals (sem_Globals gvars) $ Inh_Globals { gs_Inh_Globals = gs }
                            put $ gs_Syn_Globals gw
                            return $ genc_Syn_Globals gw  

type Transitions = [Transition]

encMain :: (SCModel t) => Model t -> ControlFlow -> State GlobalState [SExpression]
encMain m@Model{..} ccfg@ControlFlow{..} =
  let ts = ts_Syn_Function $ wrap_Function (sem_Function $ unProc mainf) $ Inh_Function { flow_Inh_Function = fromJust $ Map.lookup "main" cfg, tname_Inh_Function = ""} 
  in apply ts

apply :: Transitions -> State GlobalState [SExpression]
apply ts = do s <- get
              case enabled s ts of
                []  -> return []
                [t] -> do sexpr  <- fire t
                          sexpr' <- apply $ remove t ts
                          return $ sexpr ++ sexpr'
                _   -> error "In a deterministic model only one transition can be enabled.."

enabled :: GlobalState -> Transitions -> Transitions
enabled (_, _, pc) = filter (\(pci, g, _, _) -> pc == pci && g)   

remove :: Transition -> Transitions -> Transitions
remove (pci, _, _, pce) = filter (\(pci0, _,_,pce0) -> pci /= pci0 || pce /= pce0 )

fire :: Transition -> State GlobalState [SExpression]
fire (_,g,gsf,pce) = do s <- get
                        let (s',sexpr) = gsf s
                        put s'
                        return sexpr

{-
encMain :: (SCModel t) => Model t -> ControlFlow -> State GlobalState [SExpression]
encMain m@Model{..} ccfg@ControlFlow{..} =
   do let syn_fun =  wrap_Function (sem_Function $ unProc mainf) $ Inh_Function { tys_Inh_Function = nmdtys, vars_Inh_Function = gvars }
      return $ trace (show $ locals_Syn_Function syn_fun) $ menc_Syn_Function syn_fun
-}

encProcs :: (SCModel t) => Model t -> ControlFlow -> State GlobalState [SExpression]
encProcs m@Model{..} ccfg@ControlFlow{..} =
  let syn_fun = wrap_Functions (sem_Functions $ foldr (\p' r -> Map.insert (ident p') (unProc p') r) Map.empty $ IM.elems procs) $ Inh_Functions { cflow_Inh_Functions = Map.delete "main" cfg}
  in  capply $ cts_Syn_Functions syn_fun

--type GlobalState = (Map.Map String (PC, Map.Map Id Value), Map.Map Id Value, PC)

capply :: Map.Map String Transitions -> State GlobalState [SExpression]
capply m = do ts <- cenabled m 
              sexpr <- cfire ts
              sexpr' <- capply $ mremove ts m
              return $ sexpr ++ sexpr'

cenabled :: Map.Map String Transitions -> State GlobalState Transitions
cenabled m = do (mv,_,_) <- get
                let mi = Map.keys m
                    getpc = \k -> fst $ fromJust $ Map.lookup k mv 
                    gett  = \k -> filter (\(pci,g,_,_) -> pci == getpc k && g ) $ fromJust $ Map.lookup k m
                return $ concatMap gett mi

cfire :: Transitions -> State GlobalState [SExpression]
cfire = undefined

mremove = undefined
