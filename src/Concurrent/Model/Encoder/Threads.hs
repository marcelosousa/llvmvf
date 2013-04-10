

{-#LANGUAGE RecordWildCards #-}
-- UUAGC 0.9.42.2 (src/Concurrent/Model/Encoder/Threads.ag)
module Concurrent.Model.Encoder.Threads where

{-# LINE 23 "./src/Concurrent/Model/Encoder/Threads.ag" #-}

import Prelude hiding (foldr)
import Language.SMTLib2.Base
import Language.SMTLib2.Builder

import Language.LLVMIR

import Concurrent.Model
import Concurrent.Model.Analysis.ControlFlow (ControlFlow(..),CF)
import Concurrent.Model.Analysis.DataFlow    (DataFlow(..))

import Data.Char
import Data.Maybe
import Data.List (find,nub,elemIndex,transpose)

import Numeric

import Debug.Trace (trace)

{-# LINE 28 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 11 "src/Language/LLVMIR/Grammar/Base.ag" #-}

import Prelude              hiding (sequence)
import Data.Char            (chr)
import qualified Data.Map as Data.Map
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
#if __GLASGOW_HASKELL__ >= 704
import Data.Map hiding (foldr)
#else
import Data.Map 
#endif
{-# LINE 42 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 11 "./src/Concurrent/Model/Encoder/Global.ag" #-}

import Control.Applicative ((<$>))
import Control.Monad       (mplus)
{-# LINE 48 "src/Concurrent/Model/Encoder/Threads.hs" #-}
{-# LINE 5 "./src/Concurrent/Model/Encoder/Threads.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder.Threads
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 55 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 332 "./src/Concurrent/Model/Encoder/Threads.ag" #-}

nextpc :: PC -> [(PC,PC)] -> [PC]
nextpc i = snd . unzip . Prelude.filter (\(p,r) -> p == i && r /= -1)

encOFPC :: [(SExpr, Maybe SExpr)] -> [SExpr]
encOFPC []               = []
encOFPC ((_,Nothing):xs) = [] 
encOFPC es               = [wrap sAnd $ Prelude.map (\(x,y) -> sFn "=" (fromJust y) x) es]

encPreds :: [(SExpr, Maybe SExpr)] -> Bound -> PC -> Map.Map Id (Type, [PC]) -> Maybe Id -> [SExpr]
encPreds preds            k pc stores Nothing  = Prelude.foldr (\(s1,ms0) r -> (maybe [] (\m -> [sFn "=" m s1]) ms0) ++ r) [] preds
encPreds preds            k pc stores (Just x) = case Map.lookup x stores of
                                                           Nothing -> error "encPreds 1"
                                                           Just (_,lp) -> let pvsym = IdentExpr $ SymIdent $ SimpleSym $ "p" ++ x ++ show k
                                                                          in Prelude.foldr (\(s1,ms0) r -> if s1 == pvsym 
                                                                                                           then let v = IdentExpr $ IdxIdent (bv pc) [32]
                                                                                                                in maybe [] (\m -> [sFn "=" m v]) ms0 ++ r
                                                                                                           else maybe [] (\m -> [sFn "=" m s1]) ms0 ++ r) [] preds 

encFreshGlobal :: Id -> PC -> Map.Map Id (Type, [PC]) -> SExpr
encFreshGlobal n pc stores = case Map.lookup n stores of
                                  Nothing -> error $ "encFreshGlobal 1 " ++ n ++ " " ++ show pc ++ " " ++ show stores
                                  Just (_,lp) -> case elemIndex pc lp of 
                                                  Nothing -> error $ "encFreshGlobal 2 " ++  n ++ " " ++ show pc ++ " " ++ show lp
                                                  Just i  -> IdentExpr $ SymIdent $ SimpleSym $ n ++ show i

encodeThreads :: Functions -> Bound -> PreEncoder -> Map.Map Id SExpr -> Map.Map String PC -> Map.Map String CF -> (SExpressions, SExpr)
encodeThreads fs k p l ep cfg = let ks = [0..k-1]
                                    cpcsi   = Prelude.map (\ki -> Map.mapWithKey (\s _ -> SimpleSym $ s ++ "pc" ++ show ki) ep) ks -- [Map.Map String SSymbol]
                                    cpcexpr = Prelude.map (\m ->  Map.map (\cpci -> IdentExpr $ SymIdent cpci) m) cpcsi            -- [Map.Map String SExpr  ]
                                    cpcdexp = Prelude.concatMap (\m -> Prelude.map (\cpci -> declfun cpci $ SymSort "I32") $ Map.elems m) cpcsi
                                    -- Sparkle
                                    sparki  = Prelude.map (\ki -> Map.foldrWithKey (\s _ l -> (SimpleSym $ s ++ show ki) : l) [] cfg) ks
                                    sparkdexpr = Prelude.map (\si -> declfun si $ SymSort "Bool") $ concat sparki
                                    sparkexprs = Prelude.map (\l -> sched $ Prelude.map (\si -> IdentExpr $ SymIdent si) l) sparki
                                  --  sparkexprs = Prelude.map (\l -> wrap (sFn "xor") $ Prelude.map (\si -> IdentExpr $ SymIdent si) l) sparki
                                    -- Predicates
                                    predi = Prelude.map (\ki -> Map.foldrWithKey (\s _ l -> [SimpleSym $ "p" ++ s ++ show ki] ++ l) [] $ fStore p)  ks -- [[SSymbol]]
                                    preddexpr = Prelude.map (\si -> declfun si $ SymSort "I32") $ concat predi                -- [SExpr]
                                    predexprs = Prelude.map (Prelude.map (IdentExpr . SymIdent)) predi                        -- [[SExpr]] 
                                    predexpr  = initPred (zip (Map.keys $ fStore p) $ head predexprs) l  

                                -- predi  = Prelude.map (\ki -> Map.foldrWithKey (\s (_,e) l -> Prelude.map (\i -> SimpleSym $ "p" ++ s ++ show i ++ show ki) [0..((length e)-1)] ++ l) [] $ fStore p)  ks
                                -- preddexpr = Prelude.map (\si -> declfun si $ SymSort "Bool") $ concat predi
                                -- ffalse = IdentExpr $ SymIdent $ SimpleSym "false"
                                -- predexpr = wrap sAnd $ Prelude.map (\pr -> sFn "=" (IdentExpr $ SymIdent pr) ffalse ) $ head predi
                                    -- Mutexes
                                    muti = Prelude.map (\ki -> Prelude.map (\m -> SimpleSym $ m ++ show ki) $ mutexes p) ks -- [[Symbol]]
                                    mutdexpr = Prelude.map (\si -> declfun si $ SymSort "Bool") $ concat muti
                                    mutexprs = Prelude.map (Prelude.map (IdentExpr . SymIdent)) muti                        -- [[SExpr]] 
             
                                    spark = if cpcexpr == [] then error "spark" else encSpark $ zip (Map.elems $ head cpcexpr) (Map.elems ep)
                                    ts = encTs fs sparkexprs (pcprep cpcexpr) (prep' predexprs) (prep' mutexprs) p ep cfg
                                    phi = encPhi (Prelude.concatMap Map.elems cpcexpr) $ fails p
                                in (preddexpr ++ mutdexpr ++ cpcdexp ++ sparkdexpr,  sAnd spark $ sAnd predexpr $ sAnd ts phi)

sched :: [SExpr] -> SExpr
sched ps = wrap sOr $ Prelude.map (\p -> xorT p ps) ps

xorT :: SExpr -> [SExpr] -> SExpr
xorT x xs = wrap sAnd (x:(Prelude.map (\y -> FnAppExpr (SymIdent $ SimpleSym "not") [ y ]) $ Prelude.filter (/=x) xs))

initPred :: [(Id,SExpr)] -> Map.Map Id SExpr -> SExpr
initPred l m = wrap sAnd $ Prelude.map (\(i,s) -> case Map.lookup i m of
                                                    Nothing -> let d = IdentExpr $ SymIdent $  SimpleSym $ "l" ++ i
                                                               in sFn "=" s d
                                                    Just d  -> sFn "=" s d) l

prep' :: [[SExpr]] -> [[(SExpr, Maybe SExpr)]]
prep' le = let l = transpose $ Prelude.map prep $ transpose le
           in l -- trace (show l) $ l

pcprep :: [Map.Map String SExpr] -> [Map.Map String (SExpr, Maybe SExpr)]
pcprep []       = []
pcprep [x]      = [Map.map (\e -> (e, Nothing)) x]
pcprep (x:y:xs) = (Map.mapWithKey (\k e -> (e, Map.lookup k y)) x):pcprep (y:xs)

prep :: [SExpr] -> [(SExpr, Maybe SExpr)]
prep []     = []
prep [x]    = [(x, Nothing)]
prep [x,y]  = (x, Just y):prep [y]
prep (x:y:xs) = (x, Just y):prep (y:xs)

encTs :: Functions -> [SExpr] -> [Map.Map String (SExpr, Maybe SExpr)] -> [[(SExpr, Maybe SExpr)]] -> [[(SExpr, Maybe SExpr)]] -> PreEncoder -> Map.Map Identifier PC -> Map.Map String CF -> SExpr
encTs fs sparks cpcs prds muts p ep cfg = let ts = ts_Syn_Functions $ wrap_Functions (sem_Functions fs) $ Inh_Functions { prenc_Inh_Functions = p, cfg_Inh_Functions = cfg, cte_Inh_Functions = ep, mutexes_Inh_Functions = muts, mn_Inh_Functions = undefined, mts_Inh_Functions = undefined, sortexpr_Inh_Functions = undefined, val_Inh_Functions = undefined}
                                              rexpr = Prelude.map (\(k,(cpc,prd)) -> Prelude.map (\t -> t cpc k prd) ts) $ zip [0..] $ zip cpcs prds -- [[SExpr]]
                                              iexpr = foldr (\(s,e) r -> (s `sAnd` wrap sOr e):r) [] $ zip sparks rexpr -- [SExpr]
                                          in wrap sAnd iexpr

encSpark :: [(SExpr,PC)] -> SExpr
encSpark m = wrap sAnd $ Prelude.map (\(se,pc) -> sFn "=" se $ IdentExpr $ IdxIdent (bv pc) [32]) m

{-
encSpark _   []  = error "encSpark"
encSpark cpce ep = let fexprs = Prelude.map (\f -> IdentExpr $ IdxIdent (bv f) [32]) ep
                       exprs  = Prelude.map (\fe -> sFn "=" cpce fe) fexprs
                   in wrap sOr exprs
-}
 
encPhi :: [SExpr] -> [PC] -> SExpr
encPhi []   _  = error "encPhi 1"
encPhi _    [] = error "encPhi 2"
encPhi cpcs fs = let fsexpr = Prelude.map (\f -> IdentExpr $ IdxIdent (bv f) [32]) fs
                     exprs  = Prelude.concatMap (\cpc -> Prelude.map (\fe -> sFn "=" cpc fe) fsexpr) cpcs
                 in wrap sOr exprs
{-# LINE 163 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Grammar/Base.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Base
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 171 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 153 "src/Language/LLVMIR/Grammar/Base.ag" #-}

emptyFunction :: Function
emptyFunction = FunctionDef (Global "undefined") ExternalLinkage TyVoid False [] []
{-# LINE 177 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Grammar/Instruction.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Grammar.Instruction
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 185 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Type/Type.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Type.Type
-- Copyright :  (c) 2012 Marcelo Sousa
-- Standard LLVM IR Types
-------------------------------------------------------------------------------
{-# LINE 194 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "./src/Concurrent/Model/Encoder/Types.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder.Types
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 202 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 107 "./src/Concurrent/Model/Encoder/Types.ag" #-}

errormessage = error "enc type not supported"

fstu  (a,b,c) = a
sndu (a,b,c) = b
trdu (a,b,c) = c

getIdxN :: Type -> [Int]
getIdxN (TyArray  ne ty) = (getBSize ne):(getIdxN ty)
getIdxN (TyVector ne ty) = (getBSize ne):(getIdxN ty)
getIdxN (TyPointer ty)   = getIdxN ty
getIdxN _ = []

getBSize :: Int -> Int
getBSize n =length $  showIntAtBase 2 intToDigit n ""

getIdxSize :: Type -> Int
getIdxSize (TyArray  n _) = getBSize n
getIdxSize (TyVector n _) = getBSize n
getIdxSize (TyPointer ty) = getIdxSize ty
getIdxSize _ = error "getIdxSize"

getISize :: Type -> Int
getISize (TyInt p) = p
getISize (TyPointer t) = getISize t
getISize _ = 0

-- TODO - Define a new sort for each element of the struct
encTypes :: Types -> TypeEnv -> (TypeEnv, SExpressions, SSortExpr) 
encTypes []     _   = error "empty struct"
encTypes [x]    mts = encType x Nothing mts
encTypes (x:xs) mts = let (mts', sexprs, sort) = encType x Nothing mts
                      in  encTypes' (sexprs,sort) xs mts' 
  where encTypes' (sexprs,ssort) []     mts = (mts, sexprs, ssort)
        encTypes' (sexprs,ssort) (x:xs) mts = let (mts', sexprs', ssort') = encType x Nothing mts
                                              in  encTypes' (sexprs ++ sexprs', PairSort ssort ssort') xs mts'

encType :: Type -> Maybe SSort -> TypeEnv -> (TypeEnv, SExpressions, SSortExpr)
encType ty s mts = let tw = wrap_Type (sem_Type ty) $ Inh_Type { mn_Inh_Type = s, mts_Inh_Type = mts }
                   in case Map.lookup ty mts of
                           Nothing -> (mts_Syn_Type tw, sexprs_Syn_Type tw, sort_Syn_Type tw) 
                           Just tsn  -> case s of
                                        Nothing -> (mts, [], fst tsn)
                                        Just sn -> if sn == snd tsn
                                                   then (mts, [], SymSort sn)
                                                   else (mts, [ defsort sn (snd tsn) ], SymSort sn)

{-# LINE 252 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "./src/Concurrent/Model/Encoder/Global.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder.Global
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 260 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 52 "./src/Concurrent/Model/Encoder/Global.ag" #-}

getIdName :: Identifier -> String
getIdName (Global n) = n
getIdName (Local  n) = n

wrap :: (SExpr -> SExpr -> SExpr) -> [SExpr] -> SExpr
wrap f []     = error "wrap SExprs"
wrap f [x]    = x
wrap f (x:xs) = f x $ wrap f xs

sAnd :: SExpr -> SExpr -> SExpr
sAnd a b = sFn "and" a b

sOr :: SExpr -> SExpr -> SExpr
sOr a b = sFn "or" a b

sFn :: String -> SExpr -> SExpr -> SExpr
sFn f s1 s2 = FnAppExpr (SymIdent $ SimpleSym f) [s1, s2]

-- | Encode Global Variables
encGlobalVars :: Globals -> GlobalState -> (GlobalState, SExpressions)
encGlobalVars gvars gs = let gw = wrap_Globals (sem_Globals gvars) $ Inh_Globals { gs_Inh_Globals = gs, mn_Inh_Globals = undefined, sortexpr_Inh_Globals = undefined, tn_Inh_Globals = undefined, val_Inh_Globals = undefined}
                             me  = case sexpr_Syn_Globals gw of
                                        []  -> []
                                        [e] -> [assert e]
                                        _        -> error "encGlobalVars" 
                         in (gs_Syn_Globals gw, sexprs_Syn_Globals gw ++ me)

{-# LINE 291 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "./src/Concurrent/Model/Encoder/Value.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder.Value
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 299 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 300 "./src/Concurrent/Model/Encoder/Value.ag" #-}

init' :: [a] -> [a]
init' [] = []
init' [x] = [x]
init' [x,y] = [x]
init' (x:y:ys) = x:(init' (y:ys))

verrormessage = error "value instance not supported"

bv :: Int -> SSymbol
bv n = SimpleSym $ "bv" ++ show n

changeN :: SExpr -> Int -> SExpr
changeN (IdentExpr (IdxIdent s _)) n = IdentExpr $ IdxIdent s [n]
changeN _ _ = error "changeN"

getGValueId :: Value -> Maybe Id
getGValueId (Id (Global i) _) = Just i
getGValueId (Constant (GlobalValue (GlobalVariable (Global i) _))) = Just i
getGValueId _ = Nothing

getValueId :: Value -> Maybe Id
getValueId (Id (Global i) _) = Just i
getValueId (Id (Local  i) _) = Just i
getValueId (Constant (GlobalValue (GlobalVariable (Global i) _))) = Just i
getValueId _ = Nothing

getParameterId :: Parameter -> String -> Id
getParameterId (Parameter i _) s = s ++ (getIdName i)

ivalueId :: Valuation -> Id -> Maybe Id
ivalueId vals i = case Map.lookup i vals of
                       Nothing        -> Nothing -- error $ "Global var " ++ show i ++ " not found in the env"
                       Just (Right v) -> Just i
                       Just (Left  j) -> case ivalueId vals j of
                                              Nothing -> Just j
                                              Just h  -> Just h

encValue :: Value -> TypeEnv -> Map.Map Id (Type, [PC]) -> String -> (TypeEnv, SExpressions, [SExpr]) 
encValue v mts val tn = let vw = wrap_Value (sem_Value v) $ Inh_Value {mts_Inh_Value = mts, tn_Inh_Value = tn, val_Inh_Value = val, mn_Inh_Value = undefined, sortexpr_Inh_Value = undefined}
                        in (mts_Syn_Value vw, sexprs_Syn_Value vw, sexpr_Syn_Value vw)

encParameter :: Parameter -> TypeEnv -> String -> (TypeEnv, SExpressions, SExpr)
encParameter p mts tn = let pw = wrap_Parameter (sem_Parameter p) $ Inh_Parameter {mts_Inh_Parameter = mts, tn_Inh_Parameter = tn, sortexpr_Inh_Parameter=undefined}
                        in (mts_Syn_Parameter pw, sexprs_Syn_Parameter pw, sexpr_Syn_Parameter pw)


getValueType :: Value -> Type 
getValueType v = vtype_Syn_Value $ wrap_Value (sem_Value v) $ Inh_Value {mts_Inh_Value = undefined, tn_Inh_Value = undefined, val_Inh_Value = undefined, mn_Inh_Value = undefined, sortexpr_Inh_Value = undefined}

getFnValueName :: Value -> Id
getFnValueName (Constant (GlobalValue (FunctionValue (Global n) _))) = n
getFnValueName _ = error "getFnValueName failed"
{-# LINE 355 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder.Identifier
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 363 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 38 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}


freshId :: Id -> Id
freshId x = x ++ "0"

{-# LINE 371 "src/Concurrent/Model/Encoder/Threads.hs" #-}
-- Alias -------------------------------------------------------
-- cata
sem_Alias :: Alias ->
             T_Alias
sem_Alias (Alias _name) =
    (sem_Alias_Alias (sem_Id _name))
-- semantic domain
type T_Alias = ( Alias)
data Inh_Alias = Inh_Alias {}
data Syn_Alias = Syn_Alias {self_Syn_Alias :: Alias}
wrap_Alias :: T_Alias ->
              Inh_Alias ->
              Syn_Alias
wrap_Alias sem (Inh_Alias) =
    (let ( _lhsOself) = sem
     in  (Syn_Alias _lhsOself))
sem_Alias_Alias :: T_Id ->
                   T_Alias
sem_Alias_Alias name_ =
    (let _lhsOself :: Alias
         _nameIself :: Id
         _self =
             Alias _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
     in  ( _lhsOself))
-- Aliases -----------------------------------------------------
-- cata
sem_Aliases :: Aliases ->
               T_Aliases
sem_Aliases list =
    (Prelude.foldr sem_Aliases_Cons sem_Aliases_Nil (Prelude.map sem_Alias list))
-- semantic domain
type T_Aliases = ( Aliases)
data Inh_Aliases = Inh_Aliases {}
data Syn_Aliases = Syn_Aliases {self_Syn_Aliases :: Aliases}
wrap_Aliases :: T_Aliases ->
                Inh_Aliases ->
                Syn_Aliases
wrap_Aliases sem (Inh_Aliases) =
    (let ( _lhsOself) = sem
     in  (Syn_Aliases _lhsOself))
sem_Aliases_Cons :: T_Alias ->
                    T_Aliases ->
                    T_Aliases
sem_Aliases_Cons hd_ tl_ =
    (let _lhsOself :: Aliases
         _hdIself :: Alias
         _tlIself :: Aliases
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Aliases_Nil :: T_Aliases
sem_Aliases_Nil =
    (let _lhsOself :: Aliases
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Align -------------------------------------------------------
-- cata
sem_Align :: Align ->
             T_Align
sem_Align (Align _n) =
    (sem_Align_Align _n)
-- semantic domain
type T_Align = ( Align)
data Inh_Align = Inh_Align {}
data Syn_Align = Syn_Align {self_Syn_Align :: Align}
wrap_Align :: T_Align ->
              Inh_Align ->
              Syn_Align
wrap_Align sem (Inh_Align) =
    (let ( _lhsOself) = sem
     in  (Syn_Align _lhsOself))
sem_Align_Align :: Int ->
                   T_Align
sem_Align_Align n_ =
    (let _lhsOself :: Align
         _self =
             Align n_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Argument ----------------------------------------------------
-- cata
sem_Argument :: Argument ->
                T_Argument
sem_Argument (Argument _arg) =
    (sem_Argument_Argument (sem_Value _arg))
-- semantic domain
type T_Argument = (Maybe SSort) ->
                  TypeEnv ->
                  (Maybe SSortExpr) ->
                  String ->
                  (Map.Map Id (Type, [PC])) ->
                  ( Argument)
data Inh_Argument = Inh_Argument {mn_Inh_Argument :: (Maybe SSort),mts_Inh_Argument :: TypeEnv,sortexpr_Inh_Argument :: (Maybe SSortExpr),tn_Inh_Argument :: String,val_Inh_Argument :: (Map.Map Id (Type, [PC]))}
data Syn_Argument = Syn_Argument {self_Syn_Argument :: Argument}
wrap_Argument :: T_Argument ->
                 Inh_Argument ->
                 Syn_Argument
wrap_Argument sem (Inh_Argument _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_Argument _lhsOself))
sem_Argument_Argument :: T_Value ->
                         T_Argument
sem_Argument_Argument arg_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: Argument
              _argOmn :: (Maybe SSort)
              _argOmts :: TypeEnv
              _argOsortexpr :: (Maybe SSortExpr)
              _argOtn :: String
              _argOval :: (Map.Map Id (Type, [PC]))
              _argIident :: (Maybe String)
              _argIisGlobal :: Bool
              _argImts :: TypeEnv
              _argIpsexpr :: (Int -> [SExpr])
              _argIself :: Value
              _argIsexpr :: ([SExpr])
              _argIsexprs :: SExpressions
              _argIsort :: SSortExpr
              _argIvtype :: Type
              _self =
                  Argument _argIself
              _lhsOself =
                  _self
              _argOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 516 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 521 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 526 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 531 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 536 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _argIident,_argIisGlobal,_argImts,_argIpsexpr,_argIself,_argIsexpr,_argIsexprs,_argIsort,_argIvtype) =
                  arg_ _argOmn _argOmts _argOsortexpr _argOtn _argOval
          in  ( _lhsOself)))
-- Arguments ---------------------------------------------------
-- cata
sem_Arguments :: Arguments ->
                 T_Arguments
sem_Arguments list =
    (Prelude.foldr sem_Arguments_Cons sem_Arguments_Nil (Prelude.map sem_Argument list))
-- semantic domain
type T_Arguments = (Maybe SSort) ->
                   TypeEnv ->
                   (Maybe SSortExpr) ->
                   String ->
                   (Map.Map Id (Type, [PC])) ->
                   ( Arguments)
data Inh_Arguments = Inh_Arguments {mn_Inh_Arguments :: (Maybe SSort),mts_Inh_Arguments :: TypeEnv,sortexpr_Inh_Arguments :: (Maybe SSortExpr),tn_Inh_Arguments :: String,val_Inh_Arguments :: (Map.Map Id (Type, [PC]))}
data Syn_Arguments = Syn_Arguments {self_Syn_Arguments :: Arguments}
wrap_Arguments :: T_Arguments ->
                  Inh_Arguments ->
                  Syn_Arguments
wrap_Arguments sem (Inh_Arguments _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_Arguments _lhsOself))
sem_Arguments_Cons :: T_Argument ->
                      T_Arguments ->
                      T_Arguments
sem_Arguments_Cons hd_ tl_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: Arguments
              _hdOmn :: (Maybe SSort)
              _hdOmts :: TypeEnv
              _hdOsortexpr :: (Maybe SSortExpr)
              _hdOtn :: String
              _hdOval :: (Map.Map Id (Type, [PC]))
              _tlOmn :: (Maybe SSort)
              _tlOmts :: TypeEnv
              _tlOsortexpr :: (Maybe SSortExpr)
              _tlOtn :: String
              _tlOval :: (Map.Map Id (Type, [PC]))
              _hdIself :: Argument
              _tlIself :: Arguments
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 591 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmts =
                  ({-# LINE 72 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 596 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 601 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 73 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 606 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOval =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 611 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 616 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 72 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 621 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 626 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 73 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 631 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 636 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIself) =
                  hd_ _hdOmn _hdOmts _hdOsortexpr _hdOtn _hdOval
              ( _tlIself) =
                  tl_ _tlOmn _tlOmts _tlOsortexpr _tlOtn _tlOval
          in  ( _lhsOself)))
sem_Arguments_Nil :: T_Arguments
sem_Arguments_Nil =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: Arguments
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- AtomicOrdering ----------------------------------------------
-- cata
sem_AtomicOrdering :: AtomicOrdering ->
                      T_AtomicOrdering
sem_AtomicOrdering (NotAtomic) =
    (sem_AtomicOrdering_NotAtomic)
sem_AtomicOrdering (Unordered) =
    (sem_AtomicOrdering_Unordered)
sem_AtomicOrdering (Monotonic) =
    (sem_AtomicOrdering_Monotonic)
sem_AtomicOrdering (Acquire) =
    (sem_AtomicOrdering_Acquire)
sem_AtomicOrdering (Release) =
    (sem_AtomicOrdering_Release)
sem_AtomicOrdering (AcquireRelease) =
    (sem_AtomicOrdering_AcquireRelease)
sem_AtomicOrdering (SequentiallyConsistent) =
    (sem_AtomicOrdering_SequentiallyConsistent)
-- semantic domain
type T_AtomicOrdering = ( AtomicOrdering)
data Inh_AtomicOrdering = Inh_AtomicOrdering {}
data Syn_AtomicOrdering = Syn_AtomicOrdering {self_Syn_AtomicOrdering :: AtomicOrdering}
wrap_AtomicOrdering :: T_AtomicOrdering ->
                       Inh_AtomicOrdering ->
                       Syn_AtomicOrdering
wrap_AtomicOrdering sem (Inh_AtomicOrdering) =
    (let ( _lhsOself) = sem
     in  (Syn_AtomicOrdering _lhsOself))
sem_AtomicOrdering_NotAtomic :: T_AtomicOrdering
sem_AtomicOrdering_NotAtomic =
    (let _lhsOself :: AtomicOrdering
         _self =
             NotAtomic
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_AtomicOrdering_Unordered :: T_AtomicOrdering
sem_AtomicOrdering_Unordered =
    (let _lhsOself :: AtomicOrdering
         _self =
             Unordered
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_AtomicOrdering_Monotonic :: T_AtomicOrdering
sem_AtomicOrdering_Monotonic =
    (let _lhsOself :: AtomicOrdering
         _self =
             Monotonic
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_AtomicOrdering_Acquire :: T_AtomicOrdering
sem_AtomicOrdering_Acquire =
    (let _lhsOself :: AtomicOrdering
         _self =
             Acquire
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_AtomicOrdering_Release :: T_AtomicOrdering
sem_AtomicOrdering_Release =
    (let _lhsOself :: AtomicOrdering
         _self =
             Release
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_AtomicOrdering_AcquireRelease :: T_AtomicOrdering
sem_AtomicOrdering_AcquireRelease =
    (let _lhsOself :: AtomicOrdering
         _self =
             AcquireRelease
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_AtomicOrdering_SequentiallyConsistent :: T_AtomicOrdering
sem_AtomicOrdering_SequentiallyConsistent =
    (let _lhsOself :: AtomicOrdering
         _self =
             SequentiallyConsistent
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Attribute ---------------------------------------------------
-- cata
sem_Attribute :: Attribute ->
                 T_Attribute
sem_Attribute (ZExtAttribute) =
    (sem_Attribute_ZExtAttribute)
sem_Attribute (SExtAttribute) =
    (sem_Attribute_SExtAttribute)
sem_Attribute (NoReturnAttribute) =
    (sem_Attribute_NoReturnAttribute)
sem_Attribute (InRegAttribute) =
    (sem_Attribute_InRegAttribute)
sem_Attribute (StructRetAttribute) =
    (sem_Attribute_StructRetAttribute)
sem_Attribute (NoUnwindAttribute) =
    (sem_Attribute_NoUnwindAttribute)
sem_Attribute (NoAliasAttribute) =
    (sem_Attribute_NoAliasAttribute)
sem_Attribute (ByValAttribute) =
    (sem_Attribute_ByValAttribute)
sem_Attribute (NestAttribute) =
    (sem_Attribute_NestAttribute)
sem_Attribute (ReadNoneAttribute) =
    (sem_Attribute_ReadNoneAttribute)
sem_Attribute (ReadOnlyAttribute) =
    (sem_Attribute_ReadOnlyAttribute)
sem_Attribute (NoInlineAttribute) =
    (sem_Attribute_NoInlineAttribute)
sem_Attribute (AlwaysInlineAttribute) =
    (sem_Attribute_AlwaysInlineAttribute)
sem_Attribute (OptimizeForSizeAttribute) =
    (sem_Attribute_OptimizeForSizeAttribute)
sem_Attribute (StackProtectAttribute) =
    (sem_Attribute_StackProtectAttribute)
sem_Attribute (StackProtectReqAttribute) =
    (sem_Attribute_StackProtectReqAttribute)
sem_Attribute (NoCaptureAttribute) =
    (sem_Attribute_NoCaptureAttribute)
sem_Attribute (NoRedZoneAttribute) =
    (sem_Attribute_NoRedZoneAttribute)
sem_Attribute (NoImplicitFloatAttribute) =
    (sem_Attribute_NoImplicitFloatAttribute)
sem_Attribute (NakedAttribute) =
    (sem_Attribute_NakedAttribute)
-- semantic domain
type T_Attribute = ( Attribute)
data Inh_Attribute = Inh_Attribute {}
data Syn_Attribute = Syn_Attribute {self_Syn_Attribute :: Attribute}
wrap_Attribute :: T_Attribute ->
                  Inh_Attribute ->
                  Syn_Attribute
wrap_Attribute sem (Inh_Attribute) =
    (let ( _lhsOself) = sem
     in  (Syn_Attribute _lhsOself))
sem_Attribute_ZExtAttribute :: T_Attribute
sem_Attribute_ZExtAttribute =
    (let _lhsOself :: Attribute
         _self =
             ZExtAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_SExtAttribute :: T_Attribute
sem_Attribute_SExtAttribute =
    (let _lhsOself :: Attribute
         _self =
             SExtAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_NoReturnAttribute :: T_Attribute
sem_Attribute_NoReturnAttribute =
    (let _lhsOself :: Attribute
         _self =
             NoReturnAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_InRegAttribute :: T_Attribute
sem_Attribute_InRegAttribute =
    (let _lhsOself :: Attribute
         _self =
             InRegAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_StructRetAttribute :: T_Attribute
sem_Attribute_StructRetAttribute =
    (let _lhsOself :: Attribute
         _self =
             StructRetAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_NoUnwindAttribute :: T_Attribute
sem_Attribute_NoUnwindAttribute =
    (let _lhsOself :: Attribute
         _self =
             NoUnwindAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_NoAliasAttribute :: T_Attribute
sem_Attribute_NoAliasAttribute =
    (let _lhsOself :: Attribute
         _self =
             NoAliasAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_ByValAttribute :: T_Attribute
sem_Attribute_ByValAttribute =
    (let _lhsOself :: Attribute
         _self =
             ByValAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_NestAttribute :: T_Attribute
sem_Attribute_NestAttribute =
    (let _lhsOself :: Attribute
         _self =
             NestAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_ReadNoneAttribute :: T_Attribute
sem_Attribute_ReadNoneAttribute =
    (let _lhsOself :: Attribute
         _self =
             ReadNoneAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_ReadOnlyAttribute :: T_Attribute
sem_Attribute_ReadOnlyAttribute =
    (let _lhsOself :: Attribute
         _self =
             ReadOnlyAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_NoInlineAttribute :: T_Attribute
sem_Attribute_NoInlineAttribute =
    (let _lhsOself :: Attribute
         _self =
             NoInlineAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_AlwaysInlineAttribute :: T_Attribute
sem_Attribute_AlwaysInlineAttribute =
    (let _lhsOself :: Attribute
         _self =
             AlwaysInlineAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_OptimizeForSizeAttribute :: T_Attribute
sem_Attribute_OptimizeForSizeAttribute =
    (let _lhsOself :: Attribute
         _self =
             OptimizeForSizeAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_StackProtectAttribute :: T_Attribute
sem_Attribute_StackProtectAttribute =
    (let _lhsOself :: Attribute
         _self =
             StackProtectAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_StackProtectReqAttribute :: T_Attribute
sem_Attribute_StackProtectReqAttribute =
    (let _lhsOself :: Attribute
         _self =
             StackProtectReqAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_NoCaptureAttribute :: T_Attribute
sem_Attribute_NoCaptureAttribute =
    (let _lhsOself :: Attribute
         _self =
             NoCaptureAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_NoRedZoneAttribute :: T_Attribute
sem_Attribute_NoRedZoneAttribute =
    (let _lhsOself :: Attribute
         _self =
             NoRedZoneAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_NoImplicitFloatAttribute :: T_Attribute
sem_Attribute_NoImplicitFloatAttribute =
    (let _lhsOself :: Attribute
         _self =
             NoImplicitFloatAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_NakedAttribute :: T_Attribute
sem_Attribute_NakedAttribute =
    (let _lhsOself :: Attribute
         _self =
             NakedAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Attributes --------------------------------------------------
-- cata
sem_Attributes :: Attributes ->
                  T_Attributes
sem_Attributes list =
    (Prelude.foldr sem_Attributes_Cons sem_Attributes_Nil (Prelude.map sem_Attribute list))
-- semantic domain
type T_Attributes = ( Attributes)
data Inh_Attributes = Inh_Attributes {}
data Syn_Attributes = Syn_Attributes {self_Syn_Attributes :: Attributes}
wrap_Attributes :: T_Attributes ->
                   Inh_Attributes ->
                   Syn_Attributes
wrap_Attributes sem (Inh_Attributes) =
    (let ( _lhsOself) = sem
     in  (Syn_Attributes _lhsOself))
sem_Attributes_Cons :: T_Attribute ->
                       T_Attributes ->
                       T_Attributes
sem_Attributes_Cons hd_ tl_ =
    (let _lhsOself :: Attributes
         _hdIself :: Attribute
         _tlIself :: Attributes
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Attributes_Nil :: T_Attributes
sem_Attributes_Nil =
    (let _lhsOself :: Attributes
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- BasicBlock --------------------------------------------------
-- cata
sem_BasicBlock :: BasicBlock ->
                  T_BasicBlock
sem_BasicBlock (BasicBlock _label _instrs) =
    (sem_BasicBlock_BasicBlock (sem_Identifier _label) (sem_Instructions _instrs))
-- semantic domain
type T_BasicBlock = CF ->
                    (Map.Map Identifier PC) ->
                    (Maybe SSort) ->
                    TypeEnv ->
                    ([[(SExpr, Maybe SExpr)]]) ->
                    (Map.Map Identifier [PC]) ->
                    PreEncoder ->
                    (Maybe SSortExpr) ->
                    (Int -> SExpr) ->
                    Id ->
                    (Map.Map Id (Type, [PC])) ->
                    ( BasicBlock,([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr]))
data Inh_BasicBlock = Inh_BasicBlock {cfg_Inh_BasicBlock :: CF,cte_Inh_BasicBlock :: (Map.Map Identifier PC),mn_Inh_BasicBlock :: (Maybe SSort),mts_Inh_BasicBlock :: TypeEnv,mutexes_Inh_BasicBlock :: ([[(SExpr, Maybe SExpr)]]),pcs_Inh_BasicBlock :: (Map.Map Identifier [PC]),prenc_Inh_BasicBlock :: PreEncoder,sortexpr_Inh_BasicBlock :: (Maybe SSortExpr),spark_Inh_BasicBlock :: (Int -> SExpr),tn_Inh_BasicBlock :: Id,val_Inh_BasicBlock :: (Map.Map Id (Type, [PC]))}
data Syn_BasicBlock = Syn_BasicBlock {self_Syn_BasicBlock :: BasicBlock,ts_Syn_BasicBlock :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])}
wrap_BasicBlock :: T_BasicBlock ->
                   Inh_BasicBlock ->
                   Syn_BasicBlock
wrap_BasicBlock sem (Inh_BasicBlock _lhsIcfg _lhsIcte _lhsImn _lhsImts _lhsImutexes _lhsIpcs _lhsIprenc _lhsIsortexpr _lhsIspark _lhsItn _lhsIval) =
    (let ( _lhsOself,_lhsOts) = sem _lhsIcfg _lhsIcte _lhsImn _lhsImts _lhsImutexes _lhsIpcs _lhsIprenc _lhsIsortexpr _lhsIspark _lhsItn _lhsIval
     in  (Syn_BasicBlock _lhsOself _lhsOts))
sem_BasicBlock_BasicBlock :: T_Identifier ->
                             T_Instructions ->
                             T_BasicBlock
sem_BasicBlock_BasicBlock label_ instrs_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: BasicBlock
              _labelOsortexpr :: (Maybe SSortExpr)
              _labelOtn :: String
              _instrsOcfg :: CF
              _instrsOcte :: (Map.Map Identifier PC)
              _instrsOmn :: (Maybe SSort)
              _instrsOmts :: TypeEnv
              _instrsOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _instrsOpcs :: (Map.Map Identifier [PC])
              _instrsOprenc :: PreEncoder
              _instrsOsortexpr :: (Maybe SSortExpr)
              _instrsOspark :: (Int -> SExpr)
              _instrsOtn :: Id
              _instrsOval :: (Map.Map Id (Type, [PC]))
              _labelIdeclexpr :: ([SExpression])
              _labelIident :: String
              _labelIself :: Identifier
              _labelIsexpr :: SExpr
              _labelIssymbol :: SSymbol
              _instrsIself :: Instructions
              _instrsIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _instrsIts
                   {-# LINE 1061 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  BasicBlock _labelIself _instrsIself
              _lhsOself =
                  _self
              _labelOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 1070 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _labelOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 1075 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOcfg =
                  ({-# LINE 68 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 1080 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOcte =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 1085 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 1090 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1095 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOmutexes =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 1100 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOpcs =
                  ({-# LINE 70 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIpcs
                   {-# LINE 1105 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOprenc =
                  ({-# LINE 72 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 1110 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 1115 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOspark =
                  ({-# LINE 73 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIspark
                   {-# LINE 1120 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOtn =
                  ({-# LINE 71 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 1125 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1130 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _labelIdeclexpr,_labelIident,_labelIself,_labelIsexpr,_labelIssymbol) =
                  label_ _labelOsortexpr _labelOtn
              ( _instrsIself,_instrsIts) =
                  instrs_ _instrsOcfg _instrsOcte _instrsOmn _instrsOmts _instrsOmutexes _instrsOpcs _instrsOprenc _instrsOsortexpr _instrsOspark _instrsOtn _instrsOval
          in  ( _lhsOself,_lhsOts)))
-- BasicBlocks -------------------------------------------------
-- cata
sem_BasicBlocks :: BasicBlocks ->
                   T_BasicBlocks
sem_BasicBlocks list =
    (Prelude.foldr sem_BasicBlocks_Cons sem_BasicBlocks_Nil (Prelude.map sem_BasicBlock list))
-- semantic domain
type T_BasicBlocks = CF ->
                     (Map.Map Identifier PC) ->
                     (Maybe SSort) ->
                     TypeEnv ->
                     ([[(SExpr, Maybe SExpr)]]) ->
                     (Map.Map Identifier [PC]) ->
                     PreEncoder ->
                     (Maybe SSortExpr) ->
                     (Int -> SExpr) ->
                     Id ->
                     (Map.Map Id (Type, [PC])) ->
                     ( BasicBlocks,([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr]))
data Inh_BasicBlocks = Inh_BasicBlocks {cfg_Inh_BasicBlocks :: CF,cte_Inh_BasicBlocks :: (Map.Map Identifier PC),mn_Inh_BasicBlocks :: (Maybe SSort),mts_Inh_BasicBlocks :: TypeEnv,mutexes_Inh_BasicBlocks :: ([[(SExpr, Maybe SExpr)]]),pcs_Inh_BasicBlocks :: (Map.Map Identifier [PC]),prenc_Inh_BasicBlocks :: PreEncoder,sortexpr_Inh_BasicBlocks :: (Maybe SSortExpr),spark_Inh_BasicBlocks :: (Int -> SExpr),tn_Inh_BasicBlocks :: Id,val_Inh_BasicBlocks :: (Map.Map Id (Type, [PC]))}
data Syn_BasicBlocks = Syn_BasicBlocks {self_Syn_BasicBlocks :: BasicBlocks,ts_Syn_BasicBlocks :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])}
wrap_BasicBlocks :: T_BasicBlocks ->
                    Inh_BasicBlocks ->
                    Syn_BasicBlocks
wrap_BasicBlocks sem (Inh_BasicBlocks _lhsIcfg _lhsIcte _lhsImn _lhsImts _lhsImutexes _lhsIpcs _lhsIprenc _lhsIsortexpr _lhsIspark _lhsItn _lhsIval) =
    (let ( _lhsOself,_lhsOts) = sem _lhsIcfg _lhsIcte _lhsImn _lhsImts _lhsImutexes _lhsIpcs _lhsIprenc _lhsIsortexpr _lhsIspark _lhsItn _lhsIval
     in  (Syn_BasicBlocks _lhsOself _lhsOts))
sem_BasicBlocks_Cons :: T_BasicBlock ->
                        T_BasicBlocks ->
                        T_BasicBlocks
sem_BasicBlocks_Cons hd_ tl_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: BasicBlocks
              _hdOcfg :: CF
              _hdOcte :: (Map.Map Identifier PC)
              _hdOmn :: (Maybe SSort)
              _hdOmts :: TypeEnv
              _hdOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _hdOpcs :: (Map.Map Identifier [PC])
              _hdOprenc :: PreEncoder
              _hdOsortexpr :: (Maybe SSortExpr)
              _hdOspark :: (Int -> SExpr)
              _hdOtn :: Id
              _hdOval :: (Map.Map Id (Type, [PC]))
              _tlOcfg :: CF
              _tlOcte :: (Map.Map Identifier PC)
              _tlOmn :: (Maybe SSort)
              _tlOmts :: TypeEnv
              _tlOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _tlOpcs :: (Map.Map Identifier [PC])
              _tlOprenc :: PreEncoder
              _tlOsortexpr :: (Maybe SSortExpr)
              _tlOspark :: (Int -> SExpr)
              _tlOtn :: Id
              _tlOval :: (Map.Map Id (Type, [PC]))
              _hdIself :: BasicBlock
              _hdIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _tlIself :: BasicBlocks
              _tlIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _hdIts ++ _tlIts
                   {-# LINE 1210 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOcfg =
                  ({-# LINE 68 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 1219 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOcte =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 1224 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 1229 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1234 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmutexes =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 1239 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOpcs =
                  ({-# LINE 70 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIpcs
                   {-# LINE 1244 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOprenc =
                  ({-# LINE 72 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 1249 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 1254 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOspark =
                  ({-# LINE 73 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIspark
                   {-# LINE 1259 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 71 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 1264 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1269 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOcfg =
                  ({-# LINE 68 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 1274 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOcte =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 1279 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 1284 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1289 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmutexes =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 1294 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOpcs =
                  ({-# LINE 70 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIpcs
                   {-# LINE 1299 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOprenc =
                  ({-# LINE 72 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 1304 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 1309 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOspark =
                  ({-# LINE 73 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIspark
                   {-# LINE 1314 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 71 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 1319 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1324 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIself,_hdIts) =
                  hd_ _hdOcfg _hdOcte _hdOmn _hdOmts _hdOmutexes _hdOpcs _hdOprenc _hdOsortexpr _hdOspark _hdOtn _hdOval
              ( _tlIself,_tlIts) =
                  tl_ _tlOcfg _tlOcte _tlOmn _tlOmts _tlOmutexes _tlOpcs _tlOprenc _tlOsortexpr _tlOspark _tlOtn _tlOval
          in  ( _lhsOself,_lhsOts)))
sem_BasicBlocks_Nil :: T_BasicBlocks
sem_BasicBlocks_Nil =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: BasicBlocks
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 1349 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOself,_lhsOts)))
-- BinOp -------------------------------------------------------
-- cata
sem_BinOp :: BinOp ->
             T_BinOp
sem_BinOp (OpXchg) =
    (sem_BinOp_OpXchg)
sem_BinOp (OpAdd) =
    (sem_BinOp_OpAdd)
sem_BinOp (OpSub) =
    (sem_BinOp_OpSub)
sem_BinOp (OpAnd) =
    (sem_BinOp_OpAnd)
sem_BinOp (OpNand) =
    (sem_BinOp_OpNand)
sem_BinOp (OpOr) =
    (sem_BinOp_OpOr)
sem_BinOp (OpXor) =
    (sem_BinOp_OpXor)
sem_BinOp (OpMax) =
    (sem_BinOp_OpMax)
sem_BinOp (OpMin) =
    (sem_BinOp_OpMin)
sem_BinOp (OpUMax) =
    (sem_BinOp_OpUMax)
sem_BinOp (OpUMin) =
    (sem_BinOp_OpUMin)
-- semantic domain
type T_BinOp = ( BinOp)
data Inh_BinOp = Inh_BinOp {}
data Syn_BinOp = Syn_BinOp {self_Syn_BinOp :: BinOp}
wrap_BinOp :: T_BinOp ->
              Inh_BinOp ->
              Syn_BinOp
wrap_BinOp sem (Inh_BinOp) =
    (let ( _lhsOself) = sem
     in  (Syn_BinOp _lhsOself))
sem_BinOp_OpXchg :: T_BinOp
sem_BinOp_OpXchg =
    (let _lhsOself :: BinOp
         _self =
             OpXchg
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_BinOp_OpAdd :: T_BinOp
sem_BinOp_OpAdd =
    (let _lhsOself :: BinOp
         _self =
             OpAdd
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_BinOp_OpSub :: T_BinOp
sem_BinOp_OpSub =
    (let _lhsOself :: BinOp
         _self =
             OpSub
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_BinOp_OpAnd :: T_BinOp
sem_BinOp_OpAnd =
    (let _lhsOself :: BinOp
         _self =
             OpAnd
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_BinOp_OpNand :: T_BinOp
sem_BinOp_OpNand =
    (let _lhsOself :: BinOp
         _self =
             OpNand
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_BinOp_OpOr :: T_BinOp
sem_BinOp_OpOr =
    (let _lhsOself :: BinOp
         _self =
             OpOr
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_BinOp_OpXor :: T_BinOp
sem_BinOp_OpXor =
    (let _lhsOself :: BinOp
         _self =
             OpXor
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_BinOp_OpMax :: T_BinOp
sem_BinOp_OpMax =
    (let _lhsOself :: BinOp
         _self =
             OpMax
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_BinOp_OpMin :: T_BinOp
sem_BinOp_OpMin =
    (let _lhsOself :: BinOp
         _self =
             OpMin
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_BinOp_OpUMax :: T_BinOp
sem_BinOp_OpUMax =
    (let _lhsOself :: BinOp
         _self =
             OpUMax
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_BinOp_OpUMin :: T_BinOp
sem_BinOp_OpUMin =
    (let _lhsOself :: BinOp
         _self =
             OpUMin
         _lhsOself =
             _self
     in  ( _lhsOself))
-- CConv -------------------------------------------------------
-- cata
sem_CConv :: CConv ->
             T_CConv
sem_CConv (Ccc) =
    (sem_CConv_Ccc)
sem_CConv (Fastcc) =
    (sem_CConv_Fastcc)
sem_CConv (Coldcc) =
    (sem_CConv_Coldcc)
sem_CConv (Cc10) =
    (sem_CConv_Cc10)
sem_CConv (Cc _n) =
    (sem_CConv_Cc _n)
-- semantic domain
type T_CConv = ( CConv)
data Inh_CConv = Inh_CConv {}
data Syn_CConv = Syn_CConv {self_Syn_CConv :: CConv}
wrap_CConv :: T_CConv ->
              Inh_CConv ->
              Syn_CConv
wrap_CConv sem (Inh_CConv) =
    (let ( _lhsOself) = sem
     in  (Syn_CConv _lhsOself))
sem_CConv_Ccc :: T_CConv
sem_CConv_Ccc =
    (let _lhsOself :: CConv
         _self =
             Ccc
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_CConv_Fastcc :: T_CConv
sem_CConv_Fastcc =
    (let _lhsOself :: CConv
         _self =
             Fastcc
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_CConv_Coldcc :: T_CConv
sem_CConv_Coldcc =
    (let _lhsOself :: CConv
         _self =
             Coldcc
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_CConv_Cc10 :: T_CConv
sem_CConv_Cc10 =
    (let _lhsOself :: CConv
         _self =
             Cc10
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_CConv_Cc :: Int ->
                T_CConv
sem_CConv_Cc n_ =
    (let _lhsOself :: CConv
         _self =
             Cc n_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- CompareConstantExpr -----------------------------------------
-- cata
sem_CompareConstantExpr :: CompareConstantExpr ->
                           T_CompareConstantExpr
sem_CompareConstantExpr (ICmpExpr _cond _ty _op1 _op2) =
    (sem_CompareConstantExpr_ICmpExpr (sem_IntPredicate _cond) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_CompareConstantExpr (FCmpExpr _cond _ty _op1 _op2) =
    (sem_CompareConstantExpr_FCmpExpr (sem_RealPredicate _cond) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
-- semantic domain
type T_CompareConstantExpr = (Maybe SSort) ->
                             TypeEnv ->
                             (Maybe SSortExpr) ->
                             String ->
                             (Map.Map Id (Type, [PC])) ->
                             ( TypeEnv,CompareConstantExpr,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_CompareConstantExpr = Inh_CompareConstantExpr {mn_Inh_CompareConstantExpr :: (Maybe SSort),mts_Inh_CompareConstantExpr :: TypeEnv,sortexpr_Inh_CompareConstantExpr :: (Maybe SSortExpr),tn_Inh_CompareConstantExpr :: String,val_Inh_CompareConstantExpr :: (Map.Map Id (Type, [PC]))}
data Syn_CompareConstantExpr = Syn_CompareConstantExpr {mts_Syn_CompareConstantExpr :: TypeEnv,self_Syn_CompareConstantExpr :: CompareConstantExpr,sexpr_Syn_CompareConstantExpr :: ([SExpr]),sexprs_Syn_CompareConstantExpr :: SExpressions,sort_Syn_CompareConstantExpr :: SSortExpr,vtype_Syn_CompareConstantExpr :: Type}
wrap_CompareConstantExpr :: T_CompareConstantExpr ->
                            Inh_CompareConstantExpr ->
                            Syn_CompareConstantExpr
wrap_CompareConstantExpr sem (Inh_CompareConstantExpr _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_CompareConstantExpr _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_CompareConstantExpr_ICmpExpr :: T_IntPredicate ->
                                    T_Type ->
                                    T_Value ->
                                    T_Value ->
                                    T_CompareConstantExpr
sem_CompareConstantExpr_ICmpExpr cond_ ty_ op1_ op2_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: CompareConstantExpr
              _lhsOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _condIpred :: String
              _condIself :: IntPredicate
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _tyOmn =
                  ({-# LINE 275 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 1625 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 276 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1630 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 277 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1635 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 278 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1640 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 279 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 1645 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 280 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 1650 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ICmpExpr _condIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op2Imts
                   {-# LINE 1659 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 1664 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1669 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 1674 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1679 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1684 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 1689 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 1694 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 1699 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1704 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1709 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _condIpred,_condIself) =
                  cond_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_CompareConstantExpr_FCmpExpr :: T_RealPredicate ->
                                    T_Type ->
                                    T_Value ->
                                    T_Value ->
                                    T_CompareConstantExpr
sem_CompareConstantExpr_FCmpExpr cond_ ty_ op1_ op2_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: CompareConstantExpr
              _lhsOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _condIself :: RealPredicate
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _tyOmn =
                  ({-# LINE 275 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 1776 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 276 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1781 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 277 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1786 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 278 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1791 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 279 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 1796 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 280 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 1801 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FCmpExpr _condIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op2Imts
                   {-# LINE 1810 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 1815 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1820 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 1825 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1830 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1835 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 1840 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 1845 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 1850 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1855 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1860 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _condIself) =
                  cond_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- ComplexConstant ---------------------------------------------
-- cata
sem_ComplexConstant :: ComplexConstant ->
                       T_ComplexConstant
sem_ComplexConstant (ConstantAggregateZero _ty) =
    (sem_ComplexConstant_ConstantAggregateZero (sem_Type _ty))
sem_ComplexConstant (ConstantDataSequential _cds) =
    (sem_ComplexConstant_ConstantDataSequential (sem_ConstantDataSequential _cds))
sem_ComplexConstant (ConstantStruct _ty _vals) =
    (sem_ComplexConstant_ConstantStruct (sem_Type _ty) (sem_Values _vals))
sem_ComplexConstant (ConstantArray _ty _vals) =
    (sem_ComplexConstant_ConstantArray (sem_Type _ty) (sem_Values _vals))
sem_ComplexConstant (ConstantVector _ty _vals) =
    (sem_ComplexConstant_ConstantVector (sem_Type _ty) (sem_Values _vals))
-- semantic domain
type T_ComplexConstant = (Maybe SSort) ->
                         TypeEnv ->
                         (Maybe SSortExpr) ->
                         String ->
                         (Map.Map Id (Type, [PC])) ->
                         ( TypeEnv,ComplexConstant,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_ComplexConstant = Inh_ComplexConstant {mn_Inh_ComplexConstant :: (Maybe SSort),mts_Inh_ComplexConstant :: TypeEnv,sortexpr_Inh_ComplexConstant :: (Maybe SSortExpr),tn_Inh_ComplexConstant :: String,val_Inh_ComplexConstant :: (Map.Map Id (Type, [PC]))}
data Syn_ComplexConstant = Syn_ComplexConstant {mts_Syn_ComplexConstant :: TypeEnv,self_Syn_ComplexConstant :: ComplexConstant,sexpr_Syn_ComplexConstant :: ([SExpr]),sexprs_Syn_ComplexConstant :: SExpressions,sort_Syn_ComplexConstant :: SSortExpr,vtype_Syn_ComplexConstant :: Type}
wrap_ComplexConstant :: T_ComplexConstant ->
                        Inh_ComplexConstant ->
                        Syn_ComplexConstant
wrap_ComplexConstant sem (Inh_ComplexConstant _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_ComplexConstant _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_ComplexConstant_ConstantAggregateZero :: T_Type ->
                                             T_ComplexConstant
sem_ComplexConstant_ConstantAggregateZero ty_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOvtype :: Type
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOsort :: SSortExpr
              _lhsOself :: ComplexConstant
              _lhsOmts :: TypeEnv
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _tyOmn =
                  ({-# LINE 188 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 1924 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 189 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1929 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 190 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 1934 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 191 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 1939 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 192 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 1944 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 193 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 1949 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantAggregateZero _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1958 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ComplexConstant_ConstantDataSequential :: T_ConstantDataSequential ->
                                              T_ComplexConstant
sem_ComplexConstant_ConstantDataSequential cds_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ComplexConstant
              _lhsOmts :: TypeEnv
              _cdsOmts :: TypeEnv
              _cdsOtn :: String
              _cdsOval :: (Map.Map Id (Type, [PC]))
              _cdsImts :: TypeEnv
              _cdsIself :: ConstantDataSequential
              _cdsIsexpr :: ([SExpr])
              _cdsIsexprs :: SExpressions
              _cdsIsort :: SSortExpr
              _cdsIvtype :: Type
              _lhsOsexpr =
                  ({-# LINE 183 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cdsIsexpr
                   {-# LINE 1989 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 184 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cdsIsexprs
                   {-# LINE 1994 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 185 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cdsIvtype
                   {-# LINE 1999 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 186 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cdsIsort
                   {-# LINE 2004 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantDataSequential _cdsIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cdsImts
                   {-# LINE 2013 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cdsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2018 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cdsOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2023 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cdsOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2028 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _cdsImts,_cdsIself,_cdsIsexpr,_cdsIsexprs,_cdsIsort,_cdsIvtype) =
                  cds_ _cdsOmts _cdsOtn _cdsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ComplexConstant_ConstantStruct :: T_Type ->
                                      T_Values ->
                                      T_ComplexConstant
sem_ComplexConstant_ConstantStruct ty_ vals_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOvtype :: Type
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOsort :: SSortExpr
              _lhsOself :: ComplexConstant
              _lhsOmts :: TypeEnv
              _valsOmn :: (Maybe SSort)
              _valsOmts :: TypeEnv
              _valsOsortexpr :: (Maybe SSortExpr)
              _valsOtn :: String
              _valsOval :: (Map.Map Id (Type, [PC]))
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _valsImts :: TypeEnv
              _valsIself :: Values
              _valsIsexpr :: ([SExpr])
              _valsIsexprs :: SExpressions
              _valsIvtype :: ([Type])
              _tyOmn =
                  ({-# LINE 188 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2068 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 189 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2073 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 190 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 2078 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 191 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2083 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 192 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2088 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 193 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 2093 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantStruct _tyIself _valsIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _valsImts
                   {-# LINE 2102 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 2107 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOmts =
                  ({-# LINE 27 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 2112 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 2117 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOtn =
                  ({-# LINE 29 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2122 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOval =
                  ({-# LINE 28 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2127 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _valsImts,_valsIself,_valsIsexpr,_valsIsexprs,_valsIvtype) =
                  vals_ _valsOmn _valsOmts _valsOsortexpr _valsOtn _valsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ComplexConstant_ConstantArray :: T_Type ->
                                     T_Values ->
                                     T_ComplexConstant
sem_ComplexConstant_ConstantArray ty_ vals_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOvtype :: Type
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOsort :: SSortExpr
              _lhsOself :: ComplexConstant
              _lhsOmts :: TypeEnv
              _valsOmn :: (Maybe SSort)
              _valsOmts :: TypeEnv
              _valsOsortexpr :: (Maybe SSortExpr)
              _valsOtn :: String
              _valsOval :: (Map.Map Id (Type, [PC]))
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _valsImts :: TypeEnv
              _valsIself :: Values
              _valsIsexpr :: ([SExpr])
              _valsIsexprs :: SExpressions
              _valsIvtype :: ([Type])
              _tyOmn =
                  ({-# LINE 188 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2169 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 189 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2174 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 190 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 2179 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 191 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2184 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 192 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2189 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 193 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 2194 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantArray _tyIself _valsIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _valsImts
                   {-# LINE 2203 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 2208 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOmts =
                  ({-# LINE 27 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 2213 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 2218 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOtn =
                  ({-# LINE 29 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2223 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOval =
                  ({-# LINE 28 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2228 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _valsImts,_valsIself,_valsIsexpr,_valsIsexprs,_valsIvtype) =
                  vals_ _valsOmn _valsOmts _valsOsortexpr _valsOtn _valsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ComplexConstant_ConstantVector :: T_Type ->
                                      T_Values ->
                                      T_ComplexConstant
sem_ComplexConstant_ConstantVector ty_ vals_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ComplexConstant
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _valsOmn :: (Maybe SSort)
              _valsOmts :: TypeEnv
              _valsOsortexpr :: (Maybe SSortExpr)
              _valsOtn :: String
              _valsOval :: (Map.Map Id (Type, [PC]))
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _valsImts :: TypeEnv
              _valsIself :: Values
              _valsIsexpr :: ([SExpr])
              _valsIsexprs :: SExpressions
              _valsIvtype :: ([Type])
              _lhsOsexpr =
                  ({-# LINE 195 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2270 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 196 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2275 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 197 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2280 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantVector _tyIself _valsIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _valsImts
                   {-# LINE 2289 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 2294 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 2299 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 2304 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 2309 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOmts =
                  ({-# LINE 27 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 2314 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 2319 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOtn =
                  ({-# LINE 29 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2324 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOval =
                  ({-# LINE 28 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2329 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _valsImts,_valsIself,_valsIsexpr,_valsIsexprs,_valsIvtype) =
                  vals_ _valsOmn _valsOmts _valsOsortexpr _valsOtn _valsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- Constant ----------------------------------------------------
-- cata
sem_Constant :: Constant ->
                T_Constant
sem_Constant (UndefValue) =
    (sem_Constant_UndefValue)
sem_Constant (PoisonValue) =
    (sem_Constant_PoisonValue)
sem_Constant (BlockAddr) =
    (sem_Constant_BlockAddr)
sem_Constant (SmpConst _sc) =
    (sem_Constant_SmpConst (sem_SimpleConstant _sc))
sem_Constant (CmpConst _cc) =
    (sem_Constant_CmpConst (sem_ComplexConstant _cc))
sem_Constant (GlobalValue _gv) =
    (sem_Constant_GlobalValue (sem_GlobalValue _gv))
sem_Constant (ConstantExpr _expr) =
    (sem_Constant_ConstantExpr (sem_ConstantExpr _expr))
-- semantic domain
type T_Constant = (Maybe SSort) ->
                  TypeEnv ->
                  (Maybe SSortExpr) ->
                  String ->
                  (Map.Map Id (Type, [PC])) ->
                  ( (Maybe String),Bool,TypeEnv,(Int -> [SExpr]),Constant,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_Constant = Inh_Constant {mn_Inh_Constant :: (Maybe SSort),mts_Inh_Constant :: TypeEnv,sortexpr_Inh_Constant :: (Maybe SSortExpr),tn_Inh_Constant :: String,val_Inh_Constant :: (Map.Map Id (Type, [PC]))}
data Syn_Constant = Syn_Constant {ident_Syn_Constant :: (Maybe String),isGlobal_Syn_Constant :: Bool,mts_Syn_Constant :: TypeEnv,psexpr_Syn_Constant :: (Int -> [SExpr]),self_Syn_Constant :: Constant,sexpr_Syn_Constant :: ([SExpr]),sexprs_Syn_Constant :: SExpressions,sort_Syn_Constant :: SSortExpr,vtype_Syn_Constant :: Type}
wrap_Constant :: T_Constant ->
                 Inh_Constant ->
                 Syn_Constant
wrap_Constant sem (Inh_Constant _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_Constant _lhsOident _lhsOisGlobal _lhsOmts _lhsOpsexpr _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_Constant_UndefValue :: T_Constant
sem_Constant_UndefValue =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: Constant
              _lhsOmts :: TypeEnv
              _lhsOisGlobal =
                  ({-# LINE 136 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 2388 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 137 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2393 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 138 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> []
                   {-# LINE 2398 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 142 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2403 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 143 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2408 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 144 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2413 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 145 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2418 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  UndefValue
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2427 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Constant_PoisonValue :: T_Constant
sem_Constant_PoisonValue =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: Constant
              _lhsOmts :: TypeEnv
              _lhsOisGlobal =
                  ({-# LINE 136 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 2449 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 137 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2454 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 138 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> []
                   {-# LINE 2459 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 142 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2464 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 143 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2469 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 144 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2474 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 145 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2479 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  PoisonValue
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2488 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Constant_BlockAddr :: T_Constant
sem_Constant_BlockAddr =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: Constant
              _lhsOmts :: TypeEnv
              _lhsOisGlobal =
                  ({-# LINE 136 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 2510 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 137 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2515 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 138 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> []
                   {-# LINE 2520 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 142 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2525 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 143 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2530 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 144 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2535 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 145 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2540 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  BlockAddr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2549 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Constant_SmpConst :: T_SimpleConstant ->
                         T_Constant
sem_Constant_SmpConst sc_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOself :: Constant
              _lhsOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOsort :: SSortExpr
              _lhsOvtype :: Type
              _scOmts :: TypeEnv
              _scOtn :: String
              _scOval :: (Map.Map Id (Type, [PC]))
              _scImts :: TypeEnv
              _scIself :: SimpleConstant
              _scIsexpr :: ([SExpr])
              _scIsexprs :: SExpressions
              _scIsort :: SSortExpr
              _scIvtype :: Type
              _lhsOisGlobal =
                  ({-# LINE 136 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 2581 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 137 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2586 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 138 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> []
                   {-# LINE 2591 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  SmpConst _scIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _scImts
                   {-# LINE 2600 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 46 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _scIsexpr
                   {-# LINE 2605 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 45 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _scIsexprs
                   {-# LINE 2610 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _scIsort
                   {-# LINE 2615 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 49 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _scIvtype
                   {-# LINE 2620 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _scOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2625 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _scOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2630 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _scOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2635 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _scImts,_scIself,_scIsexpr,_scIsexprs,_scIsort,_scIvtype) =
                  sc_ _scOmts _scOtn _scOval
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Constant_CmpConst :: T_ComplexConstant ->
                         T_Constant
sem_Constant_CmpConst cc_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOself :: Constant
              _lhsOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOsort :: SSortExpr
              _lhsOvtype :: Type
              _ccOmn :: (Maybe SSort)
              _ccOmts :: TypeEnv
              _ccOsortexpr :: (Maybe SSortExpr)
              _ccOtn :: String
              _ccOval :: (Map.Map Id (Type, [PC]))
              _ccImts :: TypeEnv
              _ccIself :: ComplexConstant
              _ccIsexpr :: ([SExpr])
              _ccIsexprs :: SExpressions
              _ccIsort :: SSortExpr
              _ccIvtype :: Type
              _lhsOisGlobal =
                  ({-# LINE 136 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 2671 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 137 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2676 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 138 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> []
                   {-# LINE 2681 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  CmpConst _ccIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _ccImts
                   {-# LINE 2690 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 46 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _ccIsexpr
                   {-# LINE 2695 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 45 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _ccIsexprs
                   {-# LINE 2700 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _ccIsort
                   {-# LINE 2705 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 49 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _ccIvtype
                   {-# LINE 2710 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ccOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 2715 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ccOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2720 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ccOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 2725 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ccOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2730 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ccOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2735 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _ccImts,_ccIself,_ccIsexpr,_ccIsexprs,_ccIsort,_ccIvtype) =
                  cc_ _ccOmn _ccOmts _ccOsortexpr _ccOtn _ccOval
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Constant_GlobalValue :: T_GlobalValue ->
                            T_Constant
sem_Constant_GlobalValue gv_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: Constant
              _lhsOmts :: TypeEnv
              _gvOmts :: TypeEnv
              _gvOsortexpr :: (Maybe SSortExpr)
              _gvOtn :: String
              _gvOval :: (Map.Map Id (Type, [PC]))
              _gvIident :: (Maybe String)
              _gvIisGlobal :: Bool
              _gvImts :: TypeEnv
              _gvIpsexpr :: (Int -> [SExpr])
              _gvIself :: GlobalValue
              _gvIsexpr :: ([SExpr])
              _gvIsexprs :: SExpressions
              _gvIsort :: SSortExpr
              _gvIvtype :: Type
              _lhsOisGlobal =
                  ({-# LINE 132 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIisGlobal
                   {-# LINE 2773 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 133 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIident
                   {-# LINE 2778 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 134 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIpsexpr
                   {-# LINE 2783 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 154 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIsexpr
                   {-# LINE 2788 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 155 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIsexprs
                   {-# LINE 2793 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 156 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIvtype
                   {-# LINE 2798 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 157 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIsort
                   {-# LINE 2803 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  GlobalValue _gvIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvImts
                   {-# LINE 2812 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _gvOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2817 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _gvOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 2822 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _gvOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2827 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _gvOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2832 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _gvIident,_gvIisGlobal,_gvImts,_gvIpsexpr,_gvIself,_gvIsexpr,_gvIsexprs,_gvIsort,_gvIvtype) =
                  gv_ _gvOmts _gvOsortexpr _gvOtn _gvOval
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Constant_ConstantExpr :: T_ConstantExpr ->
                             T_Constant
sem_Constant_ConstantExpr expr_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: Constant
              _lhsOmts :: TypeEnv
              _exprOmn :: (Maybe SSort)
              _exprOmts :: TypeEnv
              _exprOsortexpr :: (Maybe SSortExpr)
              _exprOtn :: String
              _exprOval :: (Map.Map Id (Type, [PC]))
              _exprImts :: TypeEnv
              _exprIself :: ConstantExpr
              _exprIsexpr :: ([SExpr])
              _exprIsexprs :: SExpressions
              _exprIsort :: SSortExpr
              _exprIvtype :: Type
              _lhsOisGlobal =
                  ({-# LINE 136 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 2868 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 137 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2873 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 138 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> []
                   {-# LINE 2878 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 148 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _exprIsexpr
                   {-# LINE 2883 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 149 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _exprIsexprs
                   {-# LINE 2888 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 150 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _exprIvtype
                   {-# LINE 2893 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 151 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _exprIsort
                   {-# LINE 2898 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantExpr _exprIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _exprImts
                   {-# LINE 2907 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _exprOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 2912 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _exprOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2917 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _exprOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 2922 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _exprOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2927 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _exprOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2932 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _exprImts,_exprIself,_exprIsexpr,_exprIsexprs,_exprIsort,_exprIvtype) =
                  expr_ _exprOmn _exprOmts _exprOsortexpr _exprOtn _exprOval
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- ConstantDataSequential --------------------------------------
-- cata
sem_ConstantDataSequential :: ConstantDataSequential ->
                              T_ConstantDataSequential
sem_ConstantDataSequential (ConstantDataArray _ty _val) =
    (sem_ConstantDataSequential_ConstantDataArray (sem_Type _ty) _val)
sem_ConstantDataSequential (ConstantDataVector _ty _val) =
    (sem_ConstantDataSequential_ConstantDataVector (sem_Type _ty) _val)
-- semantic domain
type T_ConstantDataSequential = TypeEnv ->
                                String ->
                                (Map.Map Id (Type, [PC])) ->
                                ( TypeEnv,ConstantDataSequential,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_ConstantDataSequential = Inh_ConstantDataSequential {mts_Inh_ConstantDataSequential :: TypeEnv,tn_Inh_ConstantDataSequential :: String,val_Inh_ConstantDataSequential :: (Map.Map Id (Type, [PC]))}
data Syn_ConstantDataSequential = Syn_ConstantDataSequential {mts_Syn_ConstantDataSequential :: TypeEnv,self_Syn_ConstantDataSequential :: ConstantDataSequential,sexpr_Syn_ConstantDataSequential :: ([SExpr]),sexprs_Syn_ConstantDataSequential :: SExpressions,sort_Syn_ConstantDataSequential :: SSortExpr,vtype_Syn_ConstantDataSequential :: Type}
wrap_ConstantDataSequential :: T_ConstantDataSequential ->
                               Inh_ConstantDataSequential ->
                               Syn_ConstantDataSequential
wrap_ConstantDataSequential sem (Inh_ConstantDataSequential _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_ConstantDataSequential _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_ConstantDataSequential_ConstantDataArray :: T_Type ->
                                                String ->
                                                T_ConstantDataSequential
sem_ConstantDataSequential_ConstantDataArray ty_ val_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantDataSequential
              _lhsOmts :: TypeEnv
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _tyOmn =
                  ({-# LINE 231 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2981 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 232 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2986 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 233 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2991 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 234 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2996 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 235 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 3001 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 236 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 3006 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantDataArray _tyIself val_
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 3015 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantDataSequential_ConstantDataVector :: T_Type ->
                                                 String ->
                                                 T_ConstantDataSequential
sem_ConstantDataSequential_ConstantDataVector ty_ val_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantDataSequential
              _lhsOmts :: TypeEnv
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _tyOmn =
                  ({-# LINE 231 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 3043 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 232 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3048 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 233 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3053 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 234 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3058 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 235 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 3063 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 236 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 3068 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantDataVector _tyIself val_
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 3077 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- ConstantExpr ------------------------------------------------
-- cata
sem_ConstantExpr :: ConstantExpr ->
                    T_ConstantExpr
sem_ConstantExpr (BinaryConstantExpr) =
    (sem_ConstantExpr_BinaryConstantExpr)
sem_ConstantExpr (CompareConstantExpr _cmpExpr) =
    (sem_ConstantExpr_CompareConstantExpr (sem_CompareConstantExpr _cmpExpr))
sem_ConstantExpr (ExtractElementConstantExpr) =
    (sem_ConstantExpr_ExtractElementConstantExpr)
sem_ConstantExpr (ExtractValueConstantExpr) =
    (sem_ConstantExpr_ExtractValueConstantExpr)
sem_ConstantExpr (GetElementPtrConstantExpr _struct _idxs) =
    (sem_ConstantExpr_GetElementPtrConstantExpr (sem_Value _struct) (sem_Values _idxs))
sem_ConstantExpr (InsertElementConstantExpr) =
    (sem_ConstantExpr_InsertElementConstantExpr)
sem_ConstantExpr (InsertValueConstantExpr) =
    (sem_ConstantExpr_InsertValueConstantExpr)
sem_ConstantExpr (SelectConstantExpr) =
    (sem_ConstantExpr_SelectConstantExpr)
sem_ConstantExpr (ShuffleVectorConstantExpr) =
    (sem_ConstantExpr_ShuffleVectorConstantExpr)
sem_ConstantExpr (UnaryConstantExpr _name _op _val _ty) =
    (sem_ConstantExpr_UnaryConstantExpr _name _op (sem_Value _val) (sem_Type _ty))
-- semantic domain
type T_ConstantExpr = (Maybe SSort) ->
                      TypeEnv ->
                      (Maybe SSortExpr) ->
                      String ->
                      (Map.Map Id (Type, [PC])) ->
                      ( TypeEnv,ConstantExpr,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_ConstantExpr = Inh_ConstantExpr {mn_Inh_ConstantExpr :: (Maybe SSort),mts_Inh_ConstantExpr :: TypeEnv,sortexpr_Inh_ConstantExpr :: (Maybe SSortExpr),tn_Inh_ConstantExpr :: String,val_Inh_ConstantExpr :: (Map.Map Id (Type, [PC]))}
data Syn_ConstantExpr = Syn_ConstantExpr {mts_Syn_ConstantExpr :: TypeEnv,self_Syn_ConstantExpr :: ConstantExpr,sexpr_Syn_ConstantExpr :: ([SExpr]),sexprs_Syn_ConstantExpr :: SExpressions,sort_Syn_ConstantExpr :: SSortExpr,vtype_Syn_ConstantExpr :: Type}
wrap_ConstantExpr :: T_ConstantExpr ->
                     Inh_ConstantExpr ->
                     Syn_ConstantExpr
wrap_ConstantExpr sem (Inh_ConstantExpr _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_ConstantExpr _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_ConstantExpr_BinaryConstantExpr :: T_ConstantExpr
sem_ConstantExpr_BinaryConstantExpr =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsexpr =
                  ({-# LINE 254 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3137 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 255 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3142 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 256 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3147 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 257 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3152 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  BinaryConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3161 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_CompareConstantExpr :: T_CompareConstantExpr ->
                                        T_ConstantExpr
sem_ConstantExpr_CompareConstantExpr cmpExpr_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _cmpExprOmn :: (Maybe SSort)
              _cmpExprOmts :: TypeEnv
              _cmpExprOsortexpr :: (Maybe SSortExpr)
              _cmpExprOtn :: String
              _cmpExprOval :: (Map.Map Id (Type, [PC]))
              _cmpExprImts :: TypeEnv
              _cmpExprIself :: CompareConstantExpr
              _cmpExprIsexpr :: ([SExpr])
              _cmpExprIsexprs :: SExpressions
              _cmpExprIsort :: SSortExpr
              _cmpExprIvtype :: Type
              _lhsOsexpr =
                  ({-# LINE 268 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cmpExprIsexpr
                   {-# LINE 3192 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 269 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cmpExprIsexprs
                   {-# LINE 3197 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 270 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cmpExprIvtype
                   {-# LINE 3202 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 271 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cmpExprIsort
                   {-# LINE 3207 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  CompareConstantExpr _cmpExprIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cmpExprImts
                   {-# LINE 3216 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cmpExprOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 3221 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cmpExprOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3226 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cmpExprOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 3231 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cmpExprOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 3236 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cmpExprOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 3241 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _cmpExprImts,_cmpExprIself,_cmpExprIsexpr,_cmpExprIsexprs,_cmpExprIsort,_cmpExprIvtype) =
                  cmpExpr_ _cmpExprOmn _cmpExprOmts _cmpExprOsortexpr _cmpExprOtn _cmpExprOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_ExtractElementConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ExtractElementConstantExpr =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsexpr =
                  ({-# LINE 254 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3262 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 255 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3267 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 256 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3272 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 257 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3277 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ExtractElementConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3286 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_ExtractValueConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ExtractValueConstantExpr =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsexpr =
                  ({-# LINE 254 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3305 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 255 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3310 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 256 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3315 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 257 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3320 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ExtractValueConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3329 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_GetElementPtrConstantExpr :: T_Value ->
                                              T_Values ->
                                              T_ConstantExpr
sem_ConstantExpr_GetElementPtrConstantExpr struct_ idxs_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _structOmts :: TypeEnv
              _idxsOmts :: TypeEnv
              _lhsOmts :: TypeEnv
              _structOtn :: String
              _idxsOtn :: String
              _lhsOvtype :: Type
              _lhsOsexprs :: SExpressions
              _lhsOsexpr :: ([SExpr])
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantExpr
              _structOmn :: (Maybe SSort)
              _structOsortexpr :: (Maybe SSortExpr)
              _structOval :: (Map.Map Id (Type, [PC]))
              _idxsOmn :: (Maybe SSort)
              _idxsOsortexpr :: (Maybe SSortExpr)
              _idxsOval :: (Map.Map Id (Type, [PC]))
              _structIident :: (Maybe String)
              _structIisGlobal :: Bool
              _structImts :: TypeEnv
              _structIpsexpr :: (Int -> [SExpr])
              _structIself :: Value
              _structIsexpr :: ([SExpr])
              _structIsexprs :: SExpressions
              _structIsort :: SSortExpr
              _structIvtype :: Type
              _idxsImts :: TypeEnv
              _idxsIself :: Values
              _idxsIsexpr :: ([SExpr])
              _idxsIsexprs :: SExpressions
              _idxsIvtype :: ([Type])
              _structOmts =
                  ({-# LINE 241 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3374 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOmts =
                  ({-# LINE 242 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _structImts
                   {-# LINE 3379 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 243 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _idxsImts
                   {-# LINE 3384 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOtn =
                  ({-# LINE 244 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 3389 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOtn =
                  ({-# LINE 245 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 3394 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 246 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _structIvtype
                   {-# LINE 3399 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxn =
                  ({-# LINE 247 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   let x = getIdxN _structIvtype
                   in x
                   {-# LINE 3405 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 249 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _structIsexprs ++ _idxsIsexprs
                   {-# LINE 3410 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 250 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   [ foldr (\(n, s1) s2 -> sFn "select" s2 $ changeN s1 n) (head _structIsexpr) $ zip _idxn     $ init' _idxsIsexpr ]
                   {-# LINE 3415 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 251 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3420 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  GetElementPtrConstantExpr _structIself _idxsIself
              _lhsOself =
                  _self
              _structOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 3429 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 3434 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 3439 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 3444 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 3449 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOval =
                  ({-# LINE 28 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 3454 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _structIident,_structIisGlobal,_structImts,_structIpsexpr,_structIself,_structIsexpr,_structIsexprs,_structIsort,_structIvtype) =
                  struct_ _structOmn _structOmts _structOsortexpr _structOtn _structOval
              ( _idxsImts,_idxsIself,_idxsIsexpr,_idxsIsexprs,_idxsIvtype) =
                  idxs_ _idxsOmn _idxsOmts _idxsOsortexpr _idxsOtn _idxsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_InsertElementConstantExpr :: T_ConstantExpr
sem_ConstantExpr_InsertElementConstantExpr =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsexpr =
                  ({-# LINE 254 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3477 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 255 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3482 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 256 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3487 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 257 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3492 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  InsertElementConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3501 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_InsertValueConstantExpr :: T_ConstantExpr
sem_ConstantExpr_InsertValueConstantExpr =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsexpr =
                  ({-# LINE 254 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3520 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 255 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3525 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 256 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3530 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 257 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3535 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  InsertValueConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3544 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_SelectConstantExpr :: T_ConstantExpr
sem_ConstantExpr_SelectConstantExpr =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsexpr =
                  ({-# LINE 254 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3563 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 255 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3568 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 256 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3573 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 257 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3578 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  SelectConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3587 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_ShuffleVectorConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ShuffleVectorConstantExpr =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsexpr =
                  ({-# LINE 254 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3606 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 255 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3611 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 256 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3616 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 257 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3621 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ShuffleVectorConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3630 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_UnaryConstantExpr :: String ->
                                      Int ->
                                      T_Value ->
                                      T_Type ->
                                      T_ConstantExpr
sem_ConstantExpr_UnaryConstantExpr name_ op_ val_ ty_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _valOmn :: (Maybe SSort)
              _valOmts :: TypeEnv
              _valOsortexpr :: (Maybe SSortExpr)
              _valOtn :: String
              _valOval :: (Map.Map Id (Type, [PC]))
              _valIident :: (Maybe String)
              _valIisGlobal :: Bool
              _valImts :: TypeEnv
              _valIpsexpr :: (Int -> [SExpr])
              _valIself :: Value
              _valIsexpr :: ([SExpr])
              _valIsexprs :: SExpressions
              _valIsort :: SSortExpr
              _valIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _tyOmn =
                  ({-# LINE 260 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 3674 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 261 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3679 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 262 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3684 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 263 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3689 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 264 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 3694 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 265 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 3699 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  UnaryConstantExpr name_ op_ _valIself _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 3708 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 3713 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3718 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 3723 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 3728 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 3733 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _valIident,_valIisGlobal,_valImts,_valIpsexpr,_valIself,_valIsexpr,_valIsexprs,_valIsort,_valIvtype) =
                  val_ _valOmn _valOmts _valOsortexpr _valOtn _valOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- ConstantFP --------------------------------------------------
-- cata
sem_ConstantFP :: ConstantFP ->
                  T_ConstantFP
sem_ConstantFP (ConstantFPFloat _fpv _ty) =
    (sem_ConstantFP_ConstantFPFloat _fpv (sem_Type _ty))
sem_ConstantFP (ConstantFPDouble _dbv _ty) =
    (sem_ConstantFP_ConstantFPDouble _dbv (sem_Type _ty))
-- semantic domain
type T_ConstantFP = TypeEnv ->
                    String ->
                    (Map.Map Id (Type, [PC])) ->
                    ( TypeEnv,ConstantFP,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_ConstantFP = Inh_ConstantFP {mts_Inh_ConstantFP :: TypeEnv,tn_Inh_ConstantFP :: String,val_Inh_ConstantFP :: (Map.Map Id (Type, [PC]))}
data Syn_ConstantFP = Syn_ConstantFP {mts_Syn_ConstantFP :: TypeEnv,self_Syn_ConstantFP :: ConstantFP,sexpr_Syn_ConstantFP :: ([SExpr]),sexprs_Syn_ConstantFP :: SExpressions,sort_Syn_ConstantFP :: SSortExpr,vtype_Syn_ConstantFP :: Type}
wrap_ConstantFP :: T_ConstantFP ->
                   Inh_ConstantFP ->
                   Syn_ConstantFP
wrap_ConstantFP sem (Inh_ConstantFP _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_ConstantFP _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_ConstantFP_ConstantFPFloat :: Float ->
                                  T_Type ->
                                  T_ConstantFP
sem_ConstantFP_ConstantFPFloat fpv_ ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantFP
              _lhsOmts :: TypeEnv
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _tyOmn =
                  ({-# LINE 202 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 3784 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 203 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3789 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 204 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3794 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 205 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3799 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 206 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 3804 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 207 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 3809 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantFPFloat fpv_ _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 3818 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantFP_ConstantFPDouble :: Double ->
                                   T_Type ->
                                   T_ConstantFP
sem_ConstantFP_ConstantFPDouble dbv_ ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantFP
              _lhsOmts :: TypeEnv
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _tyOmn =
                  ({-# LINE 202 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 3846 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 203 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3851 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 204 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3856 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 205 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3861 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 206 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 3866 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 207 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 3871 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantFPDouble dbv_ _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 3880 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- DLayout -----------------------------------------------------
-- cata
sem_DLayout :: DLayout ->
               T_DLayout
sem_DLayout list =
    (Prelude.foldr sem_DLayout_Cons sem_DLayout_Nil list)
-- semantic domain
type T_DLayout = ( DLayout)
data Inh_DLayout = Inh_DLayout {}
data Syn_DLayout = Syn_DLayout {self_Syn_DLayout :: DLayout}
wrap_DLayout :: T_DLayout ->
                Inh_DLayout ->
                Syn_DLayout
wrap_DLayout sem (Inh_DLayout) =
    (let ( _lhsOself) = sem
     in  (Syn_DLayout _lhsOself))
sem_DLayout_Cons :: String ->
                    T_DLayout ->
                    T_DLayout
sem_DLayout_Cons hd_ tl_ =
    (let _lhsOself :: DLayout
         _tlIself :: DLayout
         _self =
             (:) hd_ _tlIself
         _lhsOself =
             _self
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_DLayout_Nil :: T_DLayout
sem_DLayout_Nil =
    (let _lhsOself :: DLayout
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- DataLayout --------------------------------------------------
-- cata
sem_DataLayout :: DataLayout ->
                  T_DataLayout
sem_DataLayout (DataLayout _s) =
    (sem_DataLayout_DataLayout (sem_DLayout _s))
-- semantic domain
type T_DataLayout = ( DataLayout)
data Inh_DataLayout = Inh_DataLayout {}
data Syn_DataLayout = Syn_DataLayout {self_Syn_DataLayout :: DataLayout}
wrap_DataLayout :: T_DataLayout ->
                   Inh_DataLayout ->
                   Syn_DataLayout
wrap_DataLayout sem (Inh_DataLayout) =
    (let ( _lhsOself) = sem
     in  (Syn_DataLayout _lhsOself))
sem_DataLayout_DataLayout :: T_DLayout ->
                             T_DataLayout
sem_DataLayout_DataLayout s_ =
    (let _lhsOself :: DataLayout
         _sIself :: DLayout
         _self =
             DataLayout _sIself
         _lhsOself =
             _self
         ( _sIself) =
             s_
     in  ( _lhsOself))
-- DefinitionTy ------------------------------------------------
-- cata
sem_DefinitionTy :: DefinitionTy ->
                    T_DefinitionTy
sem_DefinitionTy (ThreadLocal) =
    (sem_DefinitionTy_ThreadLocal)
sem_DefinitionTy (ConstantD) =
    (sem_DefinitionTy_ConstantD)
-- semantic domain
type T_DefinitionTy = ( DefinitionTy)
data Inh_DefinitionTy = Inh_DefinitionTy {}
data Syn_DefinitionTy = Syn_DefinitionTy {self_Syn_DefinitionTy :: DefinitionTy}
wrap_DefinitionTy :: T_DefinitionTy ->
                     Inh_DefinitionTy ->
                     Syn_DefinitionTy
wrap_DefinitionTy sem (Inh_DefinitionTy) =
    (let ( _lhsOself) = sem
     in  (Syn_DefinitionTy _lhsOself))
sem_DefinitionTy_ThreadLocal :: T_DefinitionTy
sem_DefinitionTy_ThreadLocal =
    (let _lhsOself :: DefinitionTy
         _self =
             ThreadLocal
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_DefinitionTy_ConstantD :: T_DefinitionTy
sem_DefinitionTy_ConstantD =
    (let _lhsOself :: DefinitionTy
         _self =
             ConstantD
         _lhsOself =
             _self
     in  ( _lhsOself))
-- FunAttr -----------------------------------------------------
-- cata
sem_FunAttr :: FunAttr ->
               T_FunAttr
sem_FunAttr (AddressSafety) =
    (sem_FunAttr_AddressSafety)
sem_FunAttr (Alignstack _n) =
    (sem_FunAttr_Alignstack _n)
sem_FunAttr (Alwaysinline) =
    (sem_FunAttr_Alwaysinline)
sem_FunAttr (Nonlazybind) =
    (sem_FunAttr_Nonlazybind)
sem_FunAttr (Inlinehint) =
    (sem_FunAttr_Inlinehint)
sem_FunAttr (Naked) =
    (sem_FunAttr_Naked)
sem_FunAttr (Noimplicitfloat) =
    (sem_FunAttr_Noimplicitfloat)
sem_FunAttr (Noinline) =
    (sem_FunAttr_Noinline)
sem_FunAttr (Noredzone) =
    (sem_FunAttr_Noredzone)
sem_FunAttr (Noreturn) =
    (sem_FunAttr_Noreturn)
sem_FunAttr (Nounwind) =
    (sem_FunAttr_Nounwind)
sem_FunAttr (Optsize) =
    (sem_FunAttr_Optsize)
sem_FunAttr (Readnone) =
    (sem_FunAttr_Readnone)
sem_FunAttr (Readonly) =
    (sem_FunAttr_Readonly)
sem_FunAttr (ReturnsTwice) =
    (sem_FunAttr_ReturnsTwice)
sem_FunAttr (Ssp) =
    (sem_FunAttr_Ssp)
sem_FunAttr (Sspreq) =
    (sem_FunAttr_Sspreq)
sem_FunAttr (Uwtable) =
    (sem_FunAttr_Uwtable)
-- semantic domain
type T_FunAttr = ( FunAttr)
data Inh_FunAttr = Inh_FunAttr {}
data Syn_FunAttr = Syn_FunAttr {self_Syn_FunAttr :: FunAttr}
wrap_FunAttr :: T_FunAttr ->
                Inh_FunAttr ->
                Syn_FunAttr
wrap_FunAttr sem (Inh_FunAttr) =
    (let ( _lhsOself) = sem
     in  (Syn_FunAttr _lhsOself))
sem_FunAttr_AddressSafety :: T_FunAttr
sem_FunAttr_AddressSafety =
    (let _lhsOself :: FunAttr
         _self =
             AddressSafety
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Alignstack :: Int ->
                          T_FunAttr
sem_FunAttr_Alignstack n_ =
    (let _lhsOself :: FunAttr
         _self =
             Alignstack n_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Alwaysinline :: T_FunAttr
sem_FunAttr_Alwaysinline =
    (let _lhsOself :: FunAttr
         _self =
             Alwaysinline
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Nonlazybind :: T_FunAttr
sem_FunAttr_Nonlazybind =
    (let _lhsOself :: FunAttr
         _self =
             Nonlazybind
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Inlinehint :: T_FunAttr
sem_FunAttr_Inlinehint =
    (let _lhsOself :: FunAttr
         _self =
             Inlinehint
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Naked :: T_FunAttr
sem_FunAttr_Naked =
    (let _lhsOself :: FunAttr
         _self =
             Naked
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Noimplicitfloat :: T_FunAttr
sem_FunAttr_Noimplicitfloat =
    (let _lhsOself :: FunAttr
         _self =
             Noimplicitfloat
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Noinline :: T_FunAttr
sem_FunAttr_Noinline =
    (let _lhsOself :: FunAttr
         _self =
             Noinline
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Noredzone :: T_FunAttr
sem_FunAttr_Noredzone =
    (let _lhsOself :: FunAttr
         _self =
             Noredzone
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Noreturn :: T_FunAttr
sem_FunAttr_Noreturn =
    (let _lhsOself :: FunAttr
         _self =
             Noreturn
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Nounwind :: T_FunAttr
sem_FunAttr_Nounwind =
    (let _lhsOself :: FunAttr
         _self =
             Nounwind
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Optsize :: T_FunAttr
sem_FunAttr_Optsize =
    (let _lhsOself :: FunAttr
         _self =
             Optsize
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Readnone :: T_FunAttr
sem_FunAttr_Readnone =
    (let _lhsOself :: FunAttr
         _self =
             Readnone
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Readonly :: T_FunAttr
sem_FunAttr_Readonly =
    (let _lhsOself :: FunAttr
         _self =
             Readonly
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_ReturnsTwice :: T_FunAttr
sem_FunAttr_ReturnsTwice =
    (let _lhsOself :: FunAttr
         _self =
             ReturnsTwice
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Ssp :: T_FunAttr
sem_FunAttr_Ssp =
    (let _lhsOself :: FunAttr
         _self =
             Ssp
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Sspreq :: T_FunAttr
sem_FunAttr_Sspreq =
    (let _lhsOself :: FunAttr
         _self =
             Sspreq
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Uwtable :: T_FunAttr
sem_FunAttr_Uwtable =
    (let _lhsOself :: FunAttr
         _self =
             Uwtable
         _lhsOself =
             _self
     in  ( _lhsOself))
-- FuncAttrs ---------------------------------------------------
-- cata
sem_FuncAttrs :: FuncAttrs ->
                 T_FuncAttrs
sem_FuncAttrs list =
    (Prelude.foldr sem_FuncAttrs_Cons sem_FuncAttrs_Nil (Prelude.map sem_FunAttr list))
-- semantic domain
type T_FuncAttrs = ( FuncAttrs)
data Inh_FuncAttrs = Inh_FuncAttrs {}
data Syn_FuncAttrs = Syn_FuncAttrs {self_Syn_FuncAttrs :: FuncAttrs}
wrap_FuncAttrs :: T_FuncAttrs ->
                  Inh_FuncAttrs ->
                  Syn_FuncAttrs
wrap_FuncAttrs sem (Inh_FuncAttrs) =
    (let ( _lhsOself) = sem
     in  (Syn_FuncAttrs _lhsOself))
sem_FuncAttrs_Cons :: T_FunAttr ->
                      T_FuncAttrs ->
                      T_FuncAttrs
sem_FuncAttrs_Cons hd_ tl_ =
    (let _lhsOself :: FuncAttrs
         _hdIself :: FunAttr
         _tlIself :: FuncAttrs
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_FuncAttrs_Nil :: T_FuncAttrs
sem_FuncAttrs_Nil =
    (let _lhsOself :: FuncAttrs
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Function ----------------------------------------------------
-- cata
sem_Function :: Function ->
                T_Function
sem_Function (FunctionDef _name _linkage _retty _isVar _params _body) =
    (sem_Function_FunctionDef (sem_Identifier _name) (sem_Linkage _linkage) (sem_Type _retty) _isVar (sem_Parameters _params) (sem_BasicBlocks _body))
sem_Function (FunctionDecl _name _linkage _retty _isVar _params) =
    (sem_Function_FunctionDecl (sem_Identifier _name) (sem_Linkage _linkage) (sem_Type _retty) _isVar (sem_Parameters _params))
-- semantic domain
type T_Function = CF ->
                  (Map.Map Identifier PC) ->
                  (Maybe SSort) ->
                  TypeEnv ->
                  ([[(SExpr, Maybe SExpr)]]) ->
                  (Map.Map Identifier [PC]) ->
                  PreEncoder ->
                  (Maybe SSortExpr) ->
                  (Int -> SExpr) ->
                  Id ->
                  (Map.Map Id (Type, [PC])) ->
                  ( Function,([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr]))
data Inh_Function = Inh_Function {cfg_Inh_Function :: CF,cte_Inh_Function :: (Map.Map Identifier PC),mn_Inh_Function :: (Maybe SSort),mts_Inh_Function :: TypeEnv,mutexes_Inh_Function :: ([[(SExpr, Maybe SExpr)]]),pcs_Inh_Function :: (Map.Map Identifier [PC]),prenc_Inh_Function :: PreEncoder,sortexpr_Inh_Function :: (Maybe SSortExpr),spark_Inh_Function :: (Int -> SExpr),tn_Inh_Function :: Id,val_Inh_Function :: (Map.Map Id (Type, [PC]))}
data Syn_Function = Syn_Function {self_Syn_Function :: Function,ts_Syn_Function :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])}
wrap_Function :: T_Function ->
                 Inh_Function ->
                 Syn_Function
wrap_Function sem (Inh_Function _lhsIcfg _lhsIcte _lhsImn _lhsImts _lhsImutexes _lhsIpcs _lhsIprenc _lhsIsortexpr _lhsIspark _lhsItn _lhsIval) =
    (let ( _lhsOself,_lhsOts) = sem _lhsIcfg _lhsIcte _lhsImn _lhsImts _lhsImutexes _lhsIpcs _lhsIprenc _lhsIsortexpr _lhsIspark _lhsItn _lhsIval
     in  (Syn_Function _lhsOself _lhsOts))
sem_Function_FunctionDef :: T_Identifier ->
                            T_Linkage ->
                            T_Type ->
                            Bool ->
                            T_Parameters ->
                            T_BasicBlocks ->
                            T_Function
sem_Function_FunctionDef name_ linkage_ retty_ isVar_ params_ body_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Function
              _nameOsortexpr :: (Maybe SSortExpr)
              _nameOtn :: String
              _rettyOmn :: (Maybe SSort)
              _rettyOmts :: TypeEnv
              _paramsOmts :: TypeEnv
              _paramsOsortexpr :: (Maybe SSortExpr)
              _paramsOtn :: String
              _paramsOval :: (Map.Map Id (Type, [PC]))
              _bodyOcfg :: CF
              _bodyOcte :: (Map.Map Identifier PC)
              _bodyOmn :: (Maybe SSort)
              _bodyOmts :: TypeEnv
              _bodyOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _bodyOpcs :: (Map.Map Identifier [PC])
              _bodyOprenc :: PreEncoder
              _bodyOsortexpr :: (Maybe SSortExpr)
              _bodyOspark :: (Int -> SExpr)
              _bodyOtn :: Id
              _bodyOval :: (Map.Map Id (Type, [PC]))
              _nameIdeclexpr :: ([SExpression])
              _nameIident :: String
              _nameIself :: Identifier
              _nameIsexpr :: SExpr
              _nameIssymbol :: SSymbol
              _linkageIself :: Linkage
              _rettyImts :: TypeEnv
              _rettyIself :: Type
              _rettyIsexprs :: SExpressions
              _rettyIsort :: SSortExpr
              _rettyIsortn :: SSort
              _paramsIself :: Parameters
              _bodyIself :: BasicBlocks
              _bodyIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _bodyIts
                   {-# LINE 4305 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FunctionDef _nameIself _linkageIself _rettyIself isVar_ _paramsIself _bodyIself
              _lhsOself =
                  _self
              _nameOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 4314 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nameOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 4319 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rettyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 4324 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rettyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 4329 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _paramsOmts =
                  ({-# LINE 72 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _rettyImts
                   {-# LINE 4334 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _paramsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 4339 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _paramsOtn =
                  ({-# LINE 73 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4344 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _paramsOval =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 4349 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOcfg =
                  ({-# LINE 68 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 4354 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOcte =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 4359 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 4364 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _rettyImts
                   {-# LINE 4369 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOmutexes =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 4374 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOpcs =
                  ({-# LINE 70 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIpcs
                   {-# LINE 4379 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOprenc =
                  ({-# LINE 72 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 4384 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 4389 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOspark =
                  ({-# LINE 73 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIspark
                   {-# LINE 4394 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOtn =
                  ({-# LINE 71 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 4399 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 4404 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _nameIdeclexpr,_nameIident,_nameIself,_nameIsexpr,_nameIssymbol) =
                  name_ _nameOsortexpr _nameOtn
              ( _linkageIself) =
                  linkage_
              ( _rettyImts,_rettyIself,_rettyIsexprs,_rettyIsort,_rettyIsortn) =
                  retty_ _rettyOmn _rettyOmts
              ( _paramsIself) =
                  params_ _paramsOmts _paramsOsortexpr _paramsOtn _paramsOval
              ( _bodyIself,_bodyIts) =
                  body_ _bodyOcfg _bodyOcte _bodyOmn _bodyOmts _bodyOmutexes _bodyOpcs _bodyOprenc _bodyOsortexpr _bodyOspark _bodyOtn _bodyOval
          in  ( _lhsOself,_lhsOts)))
sem_Function_FunctionDecl :: T_Identifier ->
                             T_Linkage ->
                             T_Type ->
                             Bool ->
                             T_Parameters ->
                             T_Function
sem_Function_FunctionDecl name_ linkage_ retty_ isVar_ params_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Function
              _nameOsortexpr :: (Maybe SSortExpr)
              _nameOtn :: String
              _rettyOmn :: (Maybe SSort)
              _rettyOmts :: TypeEnv
              _paramsOmts :: TypeEnv
              _paramsOsortexpr :: (Maybe SSortExpr)
              _paramsOtn :: String
              _paramsOval :: (Map.Map Id (Type, [PC]))
              _nameIdeclexpr :: ([SExpression])
              _nameIident :: String
              _nameIself :: Identifier
              _nameIsexpr :: SExpr
              _nameIssymbol :: SSymbol
              _linkageIself :: Linkage
              _rettyImts :: TypeEnv
              _rettyIself :: Type
              _rettyIsexprs :: SExpressions
              _rettyIsort :: SSortExpr
              _rettyIsortn :: SSort
              _paramsIself :: Parameters
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 4460 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FunctionDecl _nameIself _linkageIself _rettyIself isVar_ _paramsIself
              _lhsOself =
                  _self
              _nameOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 4469 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nameOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 4474 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rettyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 4479 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rettyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 4484 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _paramsOmts =
                  ({-# LINE 72 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _rettyImts
                   {-# LINE 4489 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _paramsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 4494 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _paramsOtn =
                  ({-# LINE 73 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4499 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _paramsOval =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 4504 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _nameIdeclexpr,_nameIident,_nameIself,_nameIsexpr,_nameIssymbol) =
                  name_ _nameOsortexpr _nameOtn
              ( _linkageIself) =
                  linkage_
              ( _rettyImts,_rettyIself,_rettyIsexprs,_rettyIsort,_rettyIsortn) =
                  retty_ _rettyOmn _rettyOmts
              ( _paramsIself) =
                  params_ _paramsOmts _paramsOsortexpr _paramsOtn _paramsOval
          in  ( _lhsOself,_lhsOts)))
-- Functions ---------------------------------------------------
-- cata
sem_Functions :: Functions ->
                 T_Functions
sem_Functions m =
    (Data.Map.foldrWithKey sem_Functions_Entry sem_Functions_Nil (Data.Map.map sem_Function m))
-- semantic domain
type T_Functions = (Map.Map Identifier CF) ->
                   (Map.Map Identifier PC) ->
                   (Maybe SSort) ->
                   TypeEnv ->
                   ([[(SExpr, Maybe SExpr)]]) ->
                   PreEncoder ->
                   (Maybe SSortExpr) ->
                   (Map.Map Id (Type, [PC])) ->
                   ( Functions,([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr]))
data Inh_Functions = Inh_Functions {cfg_Inh_Functions :: (Map.Map Identifier CF),cte_Inh_Functions :: (Map.Map Identifier PC),mn_Inh_Functions :: (Maybe SSort),mts_Inh_Functions :: TypeEnv,mutexes_Inh_Functions :: ([[(SExpr, Maybe SExpr)]]),prenc_Inh_Functions :: PreEncoder,sortexpr_Inh_Functions :: (Maybe SSortExpr),val_Inh_Functions :: (Map.Map Id (Type, [PC]))}
data Syn_Functions = Syn_Functions {self_Syn_Functions :: Functions,ts_Syn_Functions :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])}
wrap_Functions :: T_Functions ->
                  Inh_Functions ->
                  Syn_Functions
wrap_Functions sem (Inh_Functions _lhsIcfg _lhsIcte _lhsImn _lhsImts _lhsImutexes _lhsIprenc _lhsIsortexpr _lhsIval) =
    (let ( _lhsOself,_lhsOts) = sem _lhsIcfg _lhsIcte _lhsImn _lhsImts _lhsImutexes _lhsIprenc _lhsIsortexpr _lhsIval
     in  (Syn_Functions _lhsOself _lhsOts))
sem_Functions_Entry :: String ->
                       T_Function ->
                       T_Functions ->
                       T_Functions
sem_Functions_Entry key_ val_ tl_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIprenc
       _lhsIsortexpr
       _lhsIval ->
         (let _valOprenc :: PreEncoder
              _valOcfg :: CF
              _valOtn :: Id
              _valOspark :: (Int -> SExpr)
              _valOcte :: (Map.Map Identifier PC)
              _valOpcs :: (Map.Map Identifier [PC])
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Functions
              _valOmn :: (Maybe SSort)
              _valOmts :: TypeEnv
              _valOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _valOsortexpr :: (Maybe SSortExpr)
              _valOval :: (Map.Map Id (Type, [PC]))
              _tlOcfg :: (Map.Map Identifier CF)
              _tlOcte :: (Map.Map Identifier PC)
              _tlOmn :: (Maybe SSort)
              _tlOmts :: TypeEnv
              _tlOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _tlOprenc :: PreEncoder
              _tlOsortexpr :: (Maybe SSortExpr)
              _tlOval :: (Map.Map Id (Type, [PC]))
              _valIself :: Function
              _valIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _tlIself :: Functions
              _tlIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _valOprenc =
                  ({-# LINE 59 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 4580 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOcfg =
                  ({-# LINE 60 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fromMaybe (error $ "no cfg for " ++ show key_) $ Map.lookup (Global key_) _lhsIcfg
                   {-# LINE 4585 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOtn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   key_
                   {-# LINE 4590 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOspark =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \i -> IdentExpr $ SymIdent $ SimpleSym $ key_ ++ show i
                   {-# LINE 4595 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOcte =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Map.delete (Global key_) _lhsIcte
                   {-# LINE 4600 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOpcs =
                  ({-# LINE 64 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Map.map (\cf -> nub $ Prelude.map snd cf) $ Map.delete (Global key_) _lhsIcfg
                   {-# LINE 4605 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 55 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _valIts ++ _tlIts
                   {-# LINE 4610 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Data.Map.insert key_ _valIself _tlIself
              _lhsOself =
                  _self
              _valOmn =
                  ({-# LINE 57 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 4619 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOmts =
                  ({-# LINE 58 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 4624 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOmutexes =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 4629 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 4634 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOval =
                  ({-# LINE 66 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 4639 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOcfg =
                  ({-# LINE 52 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 4644 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOcte =
                  ({-# LINE 53 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 4649 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmn =
                  ({-# LINE 57 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 4654 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 58 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 4659 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmutexes =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 4664 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOprenc =
                  ({-# LINE 51 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 4669 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 4674 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 66 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 4679 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _valIself,_valIts) =
                  val_ _valOcfg _valOcte _valOmn _valOmts _valOmutexes _valOpcs _valOprenc _valOsortexpr _valOspark _valOtn _valOval
              ( _tlIself,_tlIts) =
                  tl_ _tlOcfg _tlOcte _tlOmn _tlOmts _tlOmutexes _tlOprenc _tlOsortexpr _tlOval
          in  ( _lhsOself,_lhsOts)))
sem_Functions_Nil :: T_Functions
sem_Functions_Nil =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIprenc
       _lhsIsortexpr
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Functions
              _lhsOts =
                  ({-# LINE 55 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 4701 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Data.Map.empty
              _lhsOself =
                  _self
          in  ( _lhsOself,_lhsOts)))
-- GCName ------------------------------------------------------
-- cata
sem_GCName :: GCName ->
              T_GCName
sem_GCName (GCName _name) =
    (sem_GCName_GCName _name)
-- semantic domain
type T_GCName = ( GCName)
data Inh_GCName = Inh_GCName {}
data Syn_GCName = Syn_GCName {self_Syn_GCName :: GCName}
wrap_GCName :: T_GCName ->
               Inh_GCName ->
               Syn_GCName
wrap_GCName sem (Inh_GCName) =
    (let ( _lhsOself) = sem
     in  (Syn_GCName _lhsOself))
sem_GCName_GCName :: String ->
                     T_GCName
sem_GCName_GCName name_ =
    (let _lhsOself :: GCName
         _self =
             GCName name_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Global ------------------------------------------------------
-- cata
sem_Global :: Global ->
              T_Global
sem_Global (GlobalVar _name _linkage _isConst _isUaddr _ty _ival _align) =
    (sem_Global_GlobalVar (sem_Identifier _name) (sem_Linkage _linkage) _isConst _isUaddr (sem_Type _ty) (sem_MConstant _ival) (sem_Align _align))
-- semantic domain
type T_Global = GlobalState ->
                (Maybe SSort) ->
                (Maybe SSortExpr) ->
                String ->
                (Map.Map Id (Type, [PC])) ->
                ( GlobalState,Global,([SExpr]),SExpressions)
data Inh_Global = Inh_Global {gs_Inh_Global :: GlobalState,mn_Inh_Global :: (Maybe SSort),sortexpr_Inh_Global :: (Maybe SSortExpr),tn_Inh_Global :: String,val_Inh_Global :: (Map.Map Id (Type, [PC]))}
data Syn_Global = Syn_Global {gs_Syn_Global :: GlobalState,self_Syn_Global :: Global,sexpr_Syn_Global :: ([SExpr]),sexprs_Syn_Global :: SExpressions}
wrap_Global :: T_Global ->
               Inh_Global ->
               Syn_Global
wrap_Global sem (Inh_Global _lhsIgs _lhsImn _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOgs,_lhsOself,_lhsOsexpr,_lhsOsexprs) = sem _lhsIgs _lhsImn _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_Global _lhsOgs _lhsOself _lhsOsexpr _lhsOsexprs))
sem_Global_GlobalVar :: T_Identifier ->
                        T_Linkage ->
                        Bool ->
                        Bool ->
                        T_Type ->
                        T_MConstant ->
                        T_Align ->
                        T_Global
sem_Global_GlobalVar name_ linkage_ isConst_ isUaddr_ ty_ ival_ align_ =
    (\ _lhsIgs
       _lhsImn
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _tyOmts :: TypeEnv
              _tyOmn :: (Maybe SSort)
              _ivalOmts :: TypeEnv
              _ivalOtn :: String
              _lhsOsexprs :: SExpressions
              _lhsOsexpr :: ([SExpr])
              _lhsOgs :: GlobalState
              _lhsOself :: Global
              _nameOsortexpr :: (Maybe SSortExpr)
              _nameOtn :: String
              _ivalOmn :: (Maybe SSort)
              _ivalOsortexpr :: (Maybe SSortExpr)
              _ivalOval :: (Map.Map Id (Type, [PC]))
              _nameIdeclexpr :: ([SExpression])
              _nameIident :: String
              _nameIself :: Identifier
              _nameIsexpr :: SExpr
              _nameIssymbol :: SSymbol
              _linkageIself :: Linkage
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _ivalImts :: TypeEnv
              _ivalIself :: MConstant
              _ivalIsexpr :: ([SExpr])
              _ivalIsexprs :: SExpressions
              _alignIself :: Align
              _tyOmts =
                  ({-# LINE 39 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   defsorts _lhsIgs
                   {-# LINE 4800 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 40 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   Nothing
                   {-# LINE 4805 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOmts =
                  ({-# LINE 41 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   _tyImts
                   {-# LINE 4810 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOtn =
                  ({-# LINE 42 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   ""
                   {-# LINE 4815 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 43 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   _tyIsexprs ++ _ivalIsexprs ++ [ declfun _sym     _tyIsort , declfun _psym     (SymSort "I32") ]
                   {-# LINE 4820 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   let v = IdentExpr $ IdxIdent (bv 0) [32]
                   in Prelude.map (\ve -> sFn "=" (IdentExpr $ SymIdent $ _sym    ) ve `sAnd` sFn "=" (IdentExpr $ SymIdent $ _psym    ) v) _ivalIsexpr
                   {-# LINE 4826 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sym =
                  ({-# LINE 46 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   SimpleSym _rawname
                   {-# LINE 4831 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _psym =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   SimpleSym $ "l" ++ _rawname
                   {-# LINE 4836 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOgs =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   let ogs@GlobalState{..} = _lhsIgs
                       gvals' = maybe gvals (\v -> Map.insert _rawname     (Right v) gvals) $ Constant <$> _ivalIself
                   in ogs { defsorts = _ivalImts, gvals = gvals' }
                   {-# LINE 4843 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rawname =
                  ({-# LINE 51 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   getIdName _nameIself
                   {-# LINE 4848 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  GlobalVar _nameIself _linkageIself isConst_ isUaddr_ _tyIself _ivalIself _alignIself
              _lhsOself =
                  _self
              _nameOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 4857 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nameOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 4862 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 4867 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 4872 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOval =
                  ({-# LINE 16 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 4877 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _nameIdeclexpr,_nameIident,_nameIself,_nameIsexpr,_nameIssymbol) =
                  name_ _nameOsortexpr _nameOtn
              ( _linkageIself) =
                  linkage_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _ivalImts,_ivalIself,_ivalIsexpr,_ivalIsexprs) =
                  ival_ _ivalOmn _ivalOmts _ivalOsortexpr _ivalOtn _ivalOval
              ( _alignIself) =
                  align_
          in  ( _lhsOgs,_lhsOself,_lhsOsexpr,_lhsOsexprs)))
-- GlobalValue -------------------------------------------------
-- cata
sem_GlobalValue :: GlobalValue ->
                   T_GlobalValue
sem_GlobalValue (FunctionValue _n _ty) =
    (sem_GlobalValue_FunctionValue (sem_Identifier _n) (sem_Type _ty))
sem_GlobalValue (GlobalAlias _n _ty) =
    (sem_GlobalValue_GlobalAlias (sem_Identifier _n) (sem_Type _ty))
sem_GlobalValue (GlobalVariable _n _ty) =
    (sem_GlobalValue_GlobalVariable (sem_Identifier _n) (sem_Type _ty))
-- semantic domain
type T_GlobalValue = TypeEnv ->
                     (Maybe SSortExpr) ->
                     String ->
                     (Map.Map Id (Type, [PC])) ->
                     ( (Maybe String),Bool,TypeEnv,(Int -> [SExpr]),GlobalValue,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_GlobalValue = Inh_GlobalValue {mts_Inh_GlobalValue :: TypeEnv,sortexpr_Inh_GlobalValue :: (Maybe SSortExpr),tn_Inh_GlobalValue :: String,val_Inh_GlobalValue :: (Map.Map Id (Type, [PC]))}
data Syn_GlobalValue = Syn_GlobalValue {ident_Syn_GlobalValue :: (Maybe String),isGlobal_Syn_GlobalValue :: Bool,mts_Syn_GlobalValue :: TypeEnv,psexpr_Syn_GlobalValue :: (Int -> [SExpr]),self_Syn_GlobalValue :: GlobalValue,sexpr_Syn_GlobalValue :: ([SExpr]),sexprs_Syn_GlobalValue :: SExpressions,sort_Syn_GlobalValue :: SSortExpr,vtype_Syn_GlobalValue :: Type}
wrap_GlobalValue :: T_GlobalValue ->
                    Inh_GlobalValue ->
                    Syn_GlobalValue
wrap_GlobalValue sem (Inh_GlobalValue _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_GlobalValue _lhsOident _lhsOisGlobal _lhsOmts _lhsOpsexpr _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_GlobalValue_FunctionValue :: T_Identifier ->
                                 T_Type ->
                                 T_GlobalValue
sem_GlobalValue_FunctionValue n_ ty_ =
    (\ _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _nOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOvtype :: Type
              _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOsort :: SSortExpr
              _lhsOself :: GlobalValue
              _lhsOmts :: TypeEnv
              _nOsortexpr :: (Maybe SSortExpr)
              _nIdeclexpr :: ([SExpression])
              _nIident :: String
              _nIself :: Identifier
              _nIsexpr :: SExpr
              _nIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _nOtn =
                  ({-# LINE 211 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4948 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 212 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 4953 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 213 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 4958 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sexpr' =
                  ({-# LINE 214 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   case Map.lookup _nIident _lhsIval of
                     Nothing -> []
                     Just (_,l) -> [ IdentExpr $ SymIdent $ SimpleSym $ _nIident ++ show n | n <- [0..(length l)-1]]
                   {-# LINE 4965 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 217 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   [ IdentExpr $ SymIdent _nIssymbol ] ++ _sexpr'
                   {-# LINE 4970 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 218 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 4975 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 219 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   \i -> let pi = IdentExpr $ SymIdent $ SimpleSym $ "p" ++ _nIident ++ show i
                             z  = IdentExpr $ SymIdent $ SimpleSym $ "l" ++ _nIident
                         in case Map.lookup _nIident _lhsIval of
                              Nothing    -> [ sFn "=" (IdentExpr $ SymIdent $ SimpleSym $ "l" ++ _nIident) z ]
                              Just (_,l) -> [ sFn "=" pi z ] ++ Prelude.map (\v -> sFn "=" pi $ IdentExpr $ IdxIdent (bv v) [32]) l
                   {-# LINE 4984 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 224 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 4989 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOisGlobal =
                  ({-# LINE 225 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   True
                   {-# LINE 4994 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 226 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Just _nIident
                   {-# LINE 4999 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 227 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 5004 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FunctionValue _nIself _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 5013 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 5018 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _nIdeclexpr,_nIident,_nIself,_nIsexpr,_nIssymbol) =
                  n_ _nOsortexpr _nOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_GlobalValue_GlobalAlias :: T_Identifier ->
                               T_Type ->
                               T_GlobalValue
sem_GlobalValue_GlobalAlias n_ ty_ =
    (\ _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _nOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOvtype :: Type
              _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOsort :: SSortExpr
              _lhsOself :: GlobalValue
              _lhsOmts :: TypeEnv
              _nOsortexpr :: (Maybe SSortExpr)
              _nIdeclexpr :: ([SExpression])
              _nIident :: String
              _nIself :: Identifier
              _nIsexpr :: SExpr
              _nIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _nOtn =
                  ({-# LINE 211 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5059 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 212 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 5064 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 213 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 5069 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sexpr' =
                  ({-# LINE 214 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   case Map.lookup _nIident _lhsIval of
                     Nothing -> []
                     Just (_,l) -> [ IdentExpr $ SymIdent $ SimpleSym $ _nIident ++ show n | n <- [0..(length l)-1]]
                   {-# LINE 5076 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 217 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   [ IdentExpr $ SymIdent _nIssymbol ] ++ _sexpr'
                   {-# LINE 5081 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 218 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 5086 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 219 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   \i -> let pi = IdentExpr $ SymIdent $ SimpleSym $ "p" ++ _nIident ++ show i
                             z  = IdentExpr $ SymIdent $ SimpleSym $ "l" ++ _nIident
                         in case Map.lookup _nIident _lhsIval of
                              Nothing    -> [ sFn "=" (IdentExpr $ SymIdent $ SimpleSym $ "l" ++ _nIident) z ]
                              Just (_,l) -> [ sFn "=" pi z ] ++ Prelude.map (\v -> sFn "=" pi $ IdentExpr $ IdxIdent (bv v) [32]) l
                   {-# LINE 5095 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 224 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 5100 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOisGlobal =
                  ({-# LINE 225 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   True
                   {-# LINE 5105 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 226 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Just _nIident
                   {-# LINE 5110 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 227 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 5115 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  GlobalAlias _nIself _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 5124 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 5129 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _nIdeclexpr,_nIident,_nIself,_nIsexpr,_nIssymbol) =
                  n_ _nOsortexpr _nOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_GlobalValue_GlobalVariable :: T_Identifier ->
                                  T_Type ->
                                  T_GlobalValue
sem_GlobalValue_GlobalVariable n_ ty_ =
    (\ _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _nOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOvtype :: Type
              _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOsort :: SSortExpr
              _lhsOself :: GlobalValue
              _lhsOmts :: TypeEnv
              _nOsortexpr :: (Maybe SSortExpr)
              _nIdeclexpr :: ([SExpression])
              _nIident :: String
              _nIself :: Identifier
              _nIsexpr :: SExpr
              _nIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _nOtn =
                  ({-# LINE 211 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5170 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 212 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 5175 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 213 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 5180 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sexpr' =
                  ({-# LINE 214 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   case Map.lookup _nIident _lhsIval of
                     Nothing -> []
                     Just (_,l) -> [ IdentExpr $ SymIdent $ SimpleSym $ _nIident ++ show n | n <- [0..(length l)-1]]
                   {-# LINE 5187 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 217 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   [ IdentExpr $ SymIdent _nIssymbol ] ++ _sexpr'
                   {-# LINE 5192 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 218 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 5197 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 219 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   \i -> let pi = IdentExpr $ SymIdent $ SimpleSym $ "p" ++ _nIident ++ show i
                             z  = IdentExpr $ SymIdent $ SimpleSym $ "l" ++ _nIident
                         in case Map.lookup _nIident _lhsIval of
                              Nothing    -> [ sFn "=" (IdentExpr $ SymIdent $ SimpleSym $ "l" ++ _nIident) z ]
                              Just (_,l) -> [ sFn "=" pi z ] ++ Prelude.map (\v -> sFn "=" pi $ IdentExpr $ IdxIdent (bv v) [32]) l
                   {-# LINE 5206 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 224 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 5211 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOisGlobal =
                  ({-# LINE 225 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   True
                   {-# LINE 5216 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 226 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Just _nIident
                   {-# LINE 5221 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 227 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 5226 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  GlobalVariable _nIself _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 5235 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 5240 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _nIdeclexpr,_nIident,_nIself,_nIsexpr,_nIssymbol) =
                  n_ _nOsortexpr _nOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- Globals -----------------------------------------------------
-- cata
sem_Globals :: Globals ->
               T_Globals
sem_Globals list =
    (Prelude.foldr sem_Globals_Cons sem_Globals_Nil (Prelude.map sem_Global list))
-- semantic domain
type T_Globals = GlobalState ->
                 (Maybe SSort) ->
                 (Maybe SSortExpr) ->
                 String ->
                 (Map.Map Id (Type, [PC])) ->
                 ( GlobalState,Globals,([SExpr]),SExpressions)
data Inh_Globals = Inh_Globals {gs_Inh_Globals :: GlobalState,mn_Inh_Globals :: (Maybe SSort),sortexpr_Inh_Globals :: (Maybe SSortExpr),tn_Inh_Globals :: String,val_Inh_Globals :: (Map.Map Id (Type, [PC]))}
data Syn_Globals = Syn_Globals {gs_Syn_Globals :: GlobalState,self_Syn_Globals :: Globals,sexpr_Syn_Globals :: ([SExpr]),sexprs_Syn_Globals :: SExpressions}
wrap_Globals :: T_Globals ->
                Inh_Globals ->
                Syn_Globals
wrap_Globals sem (Inh_Globals _lhsIgs _lhsImn _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOgs,_lhsOself,_lhsOsexpr,_lhsOsexprs) = sem _lhsIgs _lhsImn _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_Globals _lhsOgs _lhsOself _lhsOsexpr _lhsOsexprs))
sem_Globals_Cons :: T_Global ->
                    T_Globals ->
                    T_Globals
sem_Globals_Cons hd_ tl_ =
    (\ _lhsIgs
       _lhsImn
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOself :: Globals
              _lhsOgs :: GlobalState
              _hdOgs :: GlobalState
              _hdOmn :: (Maybe SSort)
              _hdOsortexpr :: (Maybe SSortExpr)
              _hdOtn :: String
              _hdOval :: (Map.Map Id (Type, [PC]))
              _tlOgs :: GlobalState
              _tlOmn :: (Maybe SSort)
              _tlOsortexpr :: (Maybe SSortExpr)
              _tlOtn :: String
              _tlOval :: (Map.Map Id (Type, [PC]))
              _hdIgs :: GlobalState
              _hdIself :: Global
              _hdIsexpr :: ([SExpr])
              _hdIsexprs :: SExpressions
              _tlIgs :: GlobalState
              _tlIself :: Globals
              _tlIsexpr :: ([SExpr])
              _tlIsexprs :: SExpressions
              _lhsOsexpr =
                  ({-# LINE 26 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   let l = _hdIsexpr ++ _tlIsexpr
                   in  if l == []
                       then []
                       else [ wrap sAnd l ]
                   {-# LINE 5305 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 30 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   nub $  _hdIsexprs ++ _tlIsexprs
                   {-# LINE 5310 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOgs =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   _tlIgs
                   {-# LINE 5319 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOgs =
                  ({-# LINE 35 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   _lhsIgs
                   {-# LINE 5324 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 5329 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 5334 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5339 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOval =
                  ({-# LINE 66 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 5344 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOgs =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   _hdIgs
                   {-# LINE 5349 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 5354 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 5359 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5364 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 66 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 5369 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIgs,_hdIself,_hdIsexpr,_hdIsexprs) =
                  hd_ _hdOgs _hdOmn _hdOsortexpr _hdOtn _hdOval
              ( _tlIgs,_tlIself,_tlIsexpr,_tlIsexprs) =
                  tl_ _tlOgs _tlOmn _tlOsortexpr _tlOtn _tlOval
          in  ( _lhsOgs,_lhsOself,_lhsOsexpr,_lhsOsexprs)))
sem_Globals_Nil :: T_Globals
sem_Globals_Nil =
    (\ _lhsIgs
       _lhsImn
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOself :: Globals
              _lhsOgs :: GlobalState
              _lhsOsexpr =
                  ({-# LINE 24 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   []
                   {-# LINE 5390 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   []
                   {-# LINE 5395 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOgs =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Global.ag" #-}
                   _lhsIgs
                   {-# LINE 5404 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOgs,_lhsOself,_lhsOsexpr,_lhsOsexprs)))
-- Id ----------------------------------------------------------
-- cata
sem_Id :: Id ->
          T_Id
sem_Id ( x1) =
    (sem_Id_Tuple x1)
-- semantic domain
type T_Id = ( Id)
data Inh_Id = Inh_Id {}
data Syn_Id = Syn_Id {self_Syn_Id :: Id}
wrap_Id :: T_Id ->
           Inh_Id ->
           Syn_Id
wrap_Id sem (Inh_Id) =
    (let ( _lhsOself) = sem
     in  (Syn_Id _lhsOself))
sem_Id_Tuple :: String ->
                T_Id
sem_Id_Tuple x1_ =
    (let _lhsOself :: Id
         _self =
             (x1_)
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Identifier --------------------------------------------------
-- cata
sem_Identifier :: Identifier ->
                  T_Identifier
sem_Identifier (Global _name) =
    (sem_Identifier_Global (sem_Id _name))
sem_Identifier (Local _name) =
    (sem_Identifier_Local (sem_Id _name))
-- semantic domain
type T_Identifier = (Maybe SSortExpr) ->
                    String ->
                    ( ([SExpression]),String,Identifier,SExpr,SSymbol)
data Inh_Identifier = Inh_Identifier {sortexpr_Inh_Identifier :: (Maybe SSortExpr),tn_Inh_Identifier :: String}
data Syn_Identifier = Syn_Identifier {declexpr_Syn_Identifier :: ([SExpression]),ident_Syn_Identifier :: String,self_Syn_Identifier :: Identifier,sexpr_Syn_Identifier :: SExpr,ssymbol_Syn_Identifier :: SSymbol}
wrap_Identifier :: T_Identifier ->
                   Inh_Identifier ->
                   Syn_Identifier
wrap_Identifier sem (Inh_Identifier _lhsIsortexpr _lhsItn) =
    (let ( _lhsOdeclexpr,_lhsOident,_lhsOself,_lhsOsexpr,_lhsOssymbol) = sem _lhsIsortexpr _lhsItn
     in  (Syn_Identifier _lhsOdeclexpr _lhsOident _lhsOself _lhsOsexpr _lhsOssymbol))
sem_Identifier_Global :: T_Id ->
                         T_Identifier
sem_Identifier_Global name_ =
    (\ _lhsIsortexpr
       _lhsItn ->
         (let _lhsOssymbol :: SSymbol
              _lhsOident :: String
              _lhsOsexpr :: SExpr
              _lhsOdeclexpr :: ([SExpression])
              _lhsOself :: Identifier
              _nameIself :: Id
              _lhsOssymbol =
                  ({-# LINE 24 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _sym
                   {-# LINE 5466 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 25 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _nameIself
                   {-# LINE 5471 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 26 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   IdentExpr $ SymIdent _sym
                   {-# LINE 5476 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOdeclexpr =
                  ({-# LINE 27 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   maybe [] (\se -> [declfun _sym     se]) _lhsIsortexpr
                   {-# LINE 5481 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sym =
                  ({-# LINE 28 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   SimpleSym _nameIself
                   {-# LINE 5486 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Global _nameIself
              _lhsOself =
                  _self
              ( _nameIself) =
                  name_
          in  ( _lhsOdeclexpr,_lhsOident,_lhsOself,_lhsOsexpr,_lhsOssymbol)))
sem_Identifier_Local :: T_Id ->
                        T_Identifier
sem_Identifier_Local name_ =
    (\ _lhsIsortexpr
       _lhsItn ->
         (let _lhsOssymbol :: SSymbol
              _lhsOident :: String
              _lhsOsexpr :: SExpr
              _lhsOdeclexpr :: ([SExpression])
              _lhsOself :: Identifier
              _nameIself :: Id
              _lhsOssymbol =
                  ({-# LINE 30 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _sym
                   {-# LINE 5509 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 31 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn ++ _nameIself
                   {-# LINE 5514 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 32 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   IdentExpr $ SymIdent _sym
                   {-# LINE 5519 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOdeclexpr =
                  ({-# LINE 33 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   if _nameIself == ""
                   then []
                   else maybe [] (\se -> [declfun _sym     se]) _lhsIsortexpr
                   {-# LINE 5526 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sym =
                  ({-# LINE 36 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   SimpleSym $ _lhsItn ++ _nameIself
                   {-# LINE 5531 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Local _nameIself
              _lhsOself =
                  _self
              ( _nameIself) =
                  name_
          in  ( _lhsOdeclexpr,_lhsOident,_lhsOself,_lhsOsexpr,_lhsOssymbol)))
-- Identifiers -------------------------------------------------
-- cata
sem_Identifiers :: Identifiers ->
                   T_Identifiers
sem_Identifiers list =
    (Prelude.foldr sem_Identifiers_Cons sem_Identifiers_Nil (Prelude.map sem_Identifier list))
-- semantic domain
type T_Identifiers = (Maybe SSortExpr) ->
                     String ->
                     ( Identifiers)
data Inh_Identifiers = Inh_Identifiers {sortexpr_Inh_Identifiers :: (Maybe SSortExpr),tn_Inh_Identifiers :: String}
data Syn_Identifiers = Syn_Identifiers {self_Syn_Identifiers :: Identifiers}
wrap_Identifiers :: T_Identifiers ->
                    Inh_Identifiers ->
                    Syn_Identifiers
wrap_Identifiers sem (Inh_Identifiers _lhsIsortexpr _lhsItn) =
    (let ( _lhsOself) = sem _lhsIsortexpr _lhsItn
     in  (Syn_Identifiers _lhsOself))
sem_Identifiers_Cons :: T_Identifier ->
                        T_Identifiers ->
                        T_Identifiers
sem_Identifiers_Cons hd_ tl_ =
    (\ _lhsIsortexpr
       _lhsItn ->
         (let _lhsOself :: Identifiers
              _hdOsortexpr :: (Maybe SSortExpr)
              _hdOtn :: String
              _tlOsortexpr :: (Maybe SSortExpr)
              _tlOtn :: String
              _hdIdeclexpr :: ([SExpression])
              _hdIident :: String
              _hdIself :: Identifier
              _hdIsexpr :: SExpr
              _hdIssymbol :: SSymbol
              _tlIself :: Identifiers
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 5582 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5587 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 5592 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5597 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIdeclexpr,_hdIident,_hdIself,_hdIsexpr,_hdIssymbol) =
                  hd_ _hdOsortexpr _hdOtn
              ( _tlIself) =
                  tl_ _tlOsortexpr _tlOtn
          in  ( _lhsOself)))
sem_Identifiers_Nil :: T_Identifiers
sem_Identifiers_Nil =
    (\ _lhsIsortexpr
       _lhsItn ->
         (let _lhsOself :: Identifiers
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- Instruction -------------------------------------------------
-- cata
sem_Instruction :: Instruction ->
                   T_Instruction
sem_Instruction (Ret _pc _r) =
    (sem_Instruction_Ret (sem_PC _pc) (sem_RetInst _r))
sem_Instruction (Br _pc _v _t _f) =
    (sem_Instruction_Br (sem_PC _pc) (sem_Value _v) (sem_Value _t) (sem_Value _f))
sem_Instruction (UBr _pc _d) =
    (sem_Instruction_UBr (sem_PC _pc) (sem_Value _d))
sem_Instruction (Switch _pc _ty _v _elems) =
    (sem_Instruction_Switch (sem_PC _pc) (sem_Type _ty) (sem_Value _v) (sem_ValIdL _elems))
sem_Instruction (Unreachable _pc) =
    (sem_Instruction_Unreachable (sem_PC _pc))
sem_Instruction (Add _pc _id _ty _op1 _op2) =
    (sem_Instruction_Add (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FAdd _pc _id _ty _op1 _op2) =
    (sem_Instruction_FAdd (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Sub _pc _id _ty _op1 _op2) =
    (sem_Instruction_Sub (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FSub _pc _id _ty _op1 _op2) =
    (sem_Instruction_FSub (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Mul _pc _id _ty _op1 _op2) =
    (sem_Instruction_Mul (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FMul _pc _id _ty _op1 _op2) =
    (sem_Instruction_FMul (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (UDiv _pc _id _ty _op1 _op2) =
    (sem_Instruction_UDiv (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (SDiv _pc _id _ty _op1 _op2) =
    (sem_Instruction_SDiv (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FDiv _pc _id _ty _op1 _op2) =
    (sem_Instruction_FDiv (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (URem _pc _id _ty _op1 _op2) =
    (sem_Instruction_URem (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (SRem _pc _id _ty _op1 _op2) =
    (sem_Instruction_SRem (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FRem _pc _id _ty _op1 _op2) =
    (sem_Instruction_FRem (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Shl _pc _id _ty _op1 _op2) =
    (sem_Instruction_Shl (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (LShr _pc _id _ty _op1 _op2) =
    (sem_Instruction_LShr (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (AShr _pc _id _ty _op1 _op2) =
    (sem_Instruction_AShr (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (And _pc _id _ty _op1 _op2) =
    (sem_Instruction_And (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Or _pc _id _ty _op1 _op2) =
    (sem_Instruction_Or (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Xor _pc _id _ty _op1 _op2) =
    (sem_Instruction_Xor (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Alloca _pc _id _ty _align) =
    (sem_Instruction_Alloca (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Align _align))
sem_Instruction (Store _pc _ty _v1 _v2 _align) =
    (sem_Instruction_Store (sem_PC _pc) (sem_Type _ty) (sem_Value _v1) (sem_Value _v2) (sem_Align _align))
sem_Instruction (Load _pc _id _v _align) =
    (sem_Instruction_Load (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Align _align))
sem_Instruction (GetElementPtr _pc _id _ty _struct _idxs) =
    (sem_Instruction_GetElementPtr (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _struct) (sem_Values _idxs))
sem_Instruction (Trunc _pc _id _v _ty) =
    (sem_Instruction_Trunc (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (ZExt _pc _id _v _ty) =
    (sem_Instruction_ZExt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (SExt _pc _id _v _ty) =
    (sem_Instruction_SExt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (FPToUI _pc _id _v _ty) =
    (sem_Instruction_FPToUI (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (FPToSI _pc _id _v _ty) =
    (sem_Instruction_FPToSI (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (UIToFP _pc _id _v _ty) =
    (sem_Instruction_UIToFP (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (SIToFP _pc _id _v _ty) =
    (sem_Instruction_SIToFP (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (FPTrunc _pc _id _v _ty) =
    (sem_Instruction_FPTrunc (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (FPExt _pc _id _v _ty) =
    (sem_Instruction_FPExt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (PtrToInt _pc _id _v _ty) =
    (sem_Instruction_PtrToInt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (IntToPtr _pc _id _v _ty) =
    (sem_Instruction_IntToPtr (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (BitCast _pc _id _v _ty) =
    (sem_Instruction_BitCast (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (ICmp _pc _id _cond _ty _op1 _op2) =
    (sem_Instruction_ICmp (sem_PC _pc) (sem_Identifier _id) (sem_IntPredicate _cond) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FCmp _pc _id _cond _ty _op1 _op2) =
    (sem_Instruction_FCmp (sem_PC _pc) (sem_Identifier _id) (sem_RealPredicate _cond) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (PHI _pc _id _ty _vals) =
    (sem_Instruction_PHI (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_PValues _vals))
sem_Instruction (Call _pc _mres _ty _callee _args) =
    (sem_Instruction_Call (sem_PC _pc) (sem_Identifier _mres) (sem_Type _ty) (sem_Identifier _callee) (sem_Values _args))
sem_Instruction (Select _pc _id _cond _valt _valf) =
    (sem_Instruction_Select (sem_PC _pc) (sem_Identifier _id) (sem_Value _cond) (sem_Value _valt) (sem_Value _valf))
sem_Instruction (ExtractValue _pc _id _aggr _idxs) =
    (sem_Instruction_ExtractValue (sem_PC _pc) (sem_Identifier _id) (sem_Value _aggr) (sem_Ints _idxs))
sem_Instruction (InsertValue _pc _id _aggr _ival _idxs) =
    (sem_Instruction_InsertValue (sem_PC _pc) (sem_Identifier _id) (sem_Value _aggr) (sem_Value _ival) (sem_Ints _idxs))
sem_Instruction (Cmpxchg _pc _id _mptr _cval _nval _ord) =
    (sem_Instruction_Cmpxchg (sem_PC _pc) (sem_Identifier _id) (sem_Value _mptr) (sem_Value _cval) (sem_Value _nval) (sem_AtomicOrdering _ord))
sem_Instruction (AtomicRMW _pc _id _args _op _ord) =
    (sem_Instruction_AtomicRMW (sem_PC _pc) (sem_Identifier _id) (sem_Values _args) (sem_BinOp _op) (sem_AtomicOrdering _ord))
sem_Instruction (CreateThread _pc _args) =
    (sem_Instruction_CreateThread (sem_PC _pc) (sem_Values _args))
sem_Instruction (MutexInit _pc _rv _mutex) =
    (sem_Instruction_MutexInit (sem_PC _pc) (sem_Identifier _rv) (sem_Value _mutex))
sem_Instruction (MutexLock _pc _rv _mutex) =
    (sem_Instruction_MutexLock (sem_PC _pc) (sem_Identifier _rv) (sem_Value _mutex))
sem_Instruction (MutexUnlock _pc _rv _mutex) =
    (sem_Instruction_MutexUnlock (sem_PC _pc) (sem_Identifier _rv) (sem_Value _mutex))
sem_Instruction (WaitEvent _pc _event) =
    (sem_Instruction_WaitEvent (sem_PC _pc) _event)
sem_Instruction (NotifyEvent _pc _event) =
    (sem_Instruction_NotifyEvent (sem_PC _pc) _event)
sem_Instruction (WaitTime _pc _time) =
    (sem_Instruction_WaitTime (sem_PC _pc) (sem_Value _time))
-- semantic domain
type T_Instruction = CF ->
                     (Map.Map Identifier PC) ->
                     (Maybe SSort) ->
                     TypeEnv ->
                     ([[(SExpr, Maybe SExpr)]]) ->
                     (Map.Map Identifier [PC]) ->
                     PreEncoder ->
                     (Maybe SSortExpr) ->
                     (Int -> SExpr) ->
                     Id ->
                     (Map.Map Id (Type, [PC])) ->
                     ( Instruction,([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr]))
data Inh_Instruction = Inh_Instruction {cfg_Inh_Instruction :: CF,cte_Inh_Instruction :: (Map.Map Identifier PC),mn_Inh_Instruction :: (Maybe SSort),mts_Inh_Instruction :: TypeEnv,mutexes_Inh_Instruction :: ([[(SExpr, Maybe SExpr)]]),pcs_Inh_Instruction :: (Map.Map Identifier [PC]),prenc_Inh_Instruction :: PreEncoder,sortexpr_Inh_Instruction :: (Maybe SSortExpr),spark_Inh_Instruction :: (Int -> SExpr),tn_Inh_Instruction :: Id,val_Inh_Instruction :: (Map.Map Id (Type, [PC]))}
data Syn_Instruction = Syn_Instruction {self_Syn_Instruction :: Instruction,ts_Syn_Instruction :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])}
wrap_Instruction :: T_Instruction ->
                    Inh_Instruction ->
                    Syn_Instruction
wrap_Instruction sem (Inh_Instruction _lhsIcfg _lhsIcte _lhsImn _lhsImts _lhsImutexes _lhsIpcs _lhsIprenc _lhsIsortexpr _lhsIspark _lhsItn _lhsIval) =
    (let ( _lhsOself,_lhsOts) = sem _lhsIcfg _lhsIcte _lhsImn _lhsImts _lhsImutexes _lhsIpcs _lhsIprenc _lhsIsortexpr _lhsIspark _lhsItn _lhsIval
     in  (Syn_Instruction _lhsOself _lhsOts))
sem_Instruction_Ret :: T_PC ->
                       T_RetInst ->
                       T_Instruction
sem_Instruction_Ret pc_ r_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _rOmn :: (Maybe SSort)
              _rOmts :: TypeEnv
              _rOsortexpr :: (Maybe SSortExpr)
              _rOtn :: String
              _rOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _rIself :: RetInst
              _ts =
                  ({-# LINE 250 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, _) k le -> let fpce = sFn "=" pce _pcev
                                         preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                         iexp = wrap sAnd $ fpce:preds
                                     in _lhsIspark k `sAnd` iexp `sAnd` (IdentExpr $ SymIdent $ SimpleSym "false")
                   {-# LINE 5779 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 5787 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 5792 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 5797 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 5802 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 5809 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 5814 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Ret _pcIself _rIself
              _lhsOself =
                  _self
              _rOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 5823 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rOmts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 5828 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 5833 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5838 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rOval =
                  ({-# LINE 78 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 5843 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _rIself) =
                  r_ _rOmn _rOmts _rOsortexpr _rOtn _rOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Br :: T_PC ->
                      T_Value ->
                      T_Value ->
                      T_Value ->
                      T_Instruction
sem_Instruction_Br pc_ v_ t_ f_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _vOmts :: TypeEnv
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _vOmn :: (Maybe SSort)
              _vOsortexpr :: (Maybe SSortExpr)
              _tOmn :: (Maybe SSort)
              _tOmts :: TypeEnv
              _tOsortexpr :: (Maybe SSortExpr)
              _tOtn :: String
              _tOval :: (Map.Map Id (Type, [PC]))
              _fOmn :: (Maybe SSort)
              _fOmts :: TypeEnv
              _fOsortexpr :: (Maybe SSortExpr)
              _fOtn :: String
              _fOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tIident :: (Maybe String)
              _tIisGlobal :: Bool
              _tImts :: TypeEnv
              _tIpsexpr :: (Int -> [SExpr])
              _tIself :: Value
              _tIsexpr :: ([SExpr])
              _tIsexprs :: SExpressions
              _tIsort :: SSortExpr
              _tIvtype :: Type
              _fIident :: (Maybe String)
              _fIisGlobal :: Bool
              _fImts :: TypeEnv
              _fIpsexpr :: (Int -> [SExpr])
              _fIself :: Value
              _fIsexpr :: ([SExpr])
              _fIsexprs :: SExpressions
              _fIsort :: SSortExpr
              _fIvtype :: Type
              _vOmts =
                  ({-# LINE 180 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 5915 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 181 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 5920 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 182 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 5925 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 183 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp   = wrap sAnd $ fpce:preds
                                        in case npce of
                                           Nothing -> _lhsIspark k `sAnd` iexp
                                           Just e  -> let fnpce = wrap sOr $ [ (ve `sAnd` sFn "=" e (_npcev     !! 0) ) `sOr` (FnAppExpr (SymIdent $ SimpleSym "not") [ve] `sAnd` sFn "=" e (_npcev     !! 1)) | ve <- _vIsexpr ]
                                                      in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 5936 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 5944 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 5949 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 5954 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 5959 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 5966 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 5971 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Br _pcIself _vIself _tIself _fIself
              _lhsOself =
                  _self
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 5980 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 5985 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 5990 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _vImts
                   {-# LINE 5995 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6000 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6005 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 6010 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 6015 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tImts
                   {-# LINE 6020 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6025 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6030 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 6035 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tIident,_tIisGlobal,_tImts,_tIpsexpr,_tIself,_tIsexpr,_tIsexprs,_tIsort,_tIvtype) =
                  t_ _tOmn _tOmts _tOsortexpr _tOtn _tOval
              ( _fIident,_fIisGlobal,_fImts,_fIpsexpr,_fIself,_fIsexpr,_fIsexprs,_fIsort,_fIvtype) =
                  f_ _fOmn _fOmts _fOsortexpr _fOtn _fOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_UBr :: T_PC ->
                       T_Value ->
                       T_Instruction
sem_Instruction_UBr pc_ d_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _dOmn :: (Maybe SSort)
              _dOmts :: TypeEnv
              _dOsortexpr :: (Maybe SSortExpr)
              _dOtn :: String
              _dOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _dIident :: (Maybe String)
              _dIisGlobal :: Bool
              _dImts :: TypeEnv
              _dIpsexpr :: (Int -> [SExpr])
              _dIself :: Value
              _dIsexpr :: ([SExpr])
              _dIsexprs :: SExpressions
              _dIsort :: SSortExpr
              _dIvtype :: Type
              _ts =
                  ({-# LINE 172 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp   = wrap sAnd $ fpce:preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "Ubr instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 6087 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 6095 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 6100 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 6105 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 6110 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 6117 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 6122 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  UBr _pcIself _dIself
              _lhsOself =
                  _self
              _dOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 6131 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _dOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 6136 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _dOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6141 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _dOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6146 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _dOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 6151 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _dIident,_dIisGlobal,_dImts,_dIpsexpr,_dIself,_dIsexpr,_dIsexprs,_dIsort,_dIvtype) =
                  d_ _dOmn _dOmts _dOsortexpr _dOtn _dOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Switch :: T_PC ->
                          T_Type ->
                          T_Value ->
                          T_ValIdL ->
                          T_Instruction
sem_Instruction_Switch pc_ ty_ v_ elems_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _vOmn :: (Maybe SSort)
              _vOmts :: TypeEnv
              _vOsortexpr :: (Maybe SSortExpr)
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _elemsOmn :: (Maybe SSort)
              _elemsOmts :: TypeEnv
              _elemsOsortexpr :: (Maybe SSortExpr)
              _elemsOtn :: String
              _elemsOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _elemsIself :: ValIdL
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 6208 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Switch _pcIself _tyIself _vIself _elemsIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 6217 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 6222 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 6227 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6232 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6237 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6242 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 6247 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _elemsOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 6252 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _elemsOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _vImts
                   {-# LINE 6257 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _elemsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6262 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _elemsOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6267 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _elemsOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 6272 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _elemsIself) =
                  elems_ _elemsOmn _elemsOmts _elemsOsortexpr _elemsOtn _elemsOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Unreachable :: T_PC ->
                               T_Instruction
sem_Instruction_Unreachable pc_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _pcIself :: PC
              _ts =
                  ({-# LINE 250 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, _) k le -> let fpce = sFn "=" pce _pcev
                                         preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                         iexp = wrap sAnd $ fpce:preds
                                     in _lhsIspark k `sAnd` iexp `sAnd` (IdentExpr $ SymIdent $ SimpleSym "false")
                   {-# LINE 6306 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 6314 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 6319 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 6324 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 6329 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 6336 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 6341 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Unreachable _pcIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Add :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Add pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
              _op2Otn :: String
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Osortexpr :: (Maybe SSortExpr)
              _op2Omn :: (Maybe SSort)
              _op2Osortexpr :: (Maybe SSortExpr)
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _idOtn =
                  ({-# LINE 143 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 6416 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 144 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 6421 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 145 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 6426 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 146 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 6431 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 147 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 6436 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 148 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 6441 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 149 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 6446 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 150 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 6451 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 151 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ Prelude.map (\se -> sFn "=" _idIsexpr se) _vse
                   {-# LINE 6456 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 152 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "bin instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 6467 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vse =
                  ({-# LINE 160 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ sFn "bvadd" e1 e2 | e1 <- _op1Isexpr, e2 <- _op2Isexpr ]
                   {-# LINE 6472 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 6480 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 6485 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 6490 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 6495 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 6502 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 6507 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Add _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 6516 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 6521 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 6526 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6531 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 6536 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6541 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_FAdd :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FAdd pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 6620 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FAdd _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6629 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6634 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 6639 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 6644 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 6649 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6654 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6659 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6664 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 6669 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 6674 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 6679 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6684 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6689 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 6694 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Sub :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Sub pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
              _op2Otn :: String
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Osortexpr :: (Maybe SSortExpr)
              _op2Omn :: (Maybe SSort)
              _op2Osortexpr :: (Maybe SSortExpr)
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _idOtn =
                  ({-# LINE 143 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 6773 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 144 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 6778 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 145 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 6783 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 146 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 6788 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 147 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 6793 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 148 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 6798 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 149 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 6803 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 150 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 6808 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 151 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ Prelude.map (\se -> sFn "=" _idIsexpr se) _vse
                   {-# LINE 6813 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 152 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "bin instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 6824 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vse =
                  ({-# LINE 162 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ sFn "bvsub" e1 e2 | e1 <- _op1Isexpr, e2 <- _op2Isexpr ]
                   {-# LINE 6829 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 6837 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 6842 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 6847 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 6852 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 6859 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 6864 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Sub _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 6873 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 6878 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 6883 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6888 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 6893 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6898 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_FSub :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FSub pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 6977 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FSub _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 6986 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6991 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 6996 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 7001 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7006 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 7011 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7016 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7021 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 7026 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7031 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 7036 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7041 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7046 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 7051 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Mul :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Mul pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
              _op2Otn :: String
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Osortexpr :: (Maybe SSortExpr)
              _op2Omn :: (Maybe SSort)
              _op2Osortexpr :: (Maybe SSortExpr)
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _idOtn =
                  ({-# LINE 143 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 7130 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 144 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 7135 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 145 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 7140 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 146 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 7145 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 147 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 7150 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 148 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 7155 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 149 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 7160 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 150 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 7165 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 151 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ Prelude.map (\se -> sFn "=" _idIsexpr se) _vse
                   {-# LINE 7170 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 152 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "bin instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 7181 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vse =
                  ({-# LINE 164 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ sFn "bvmul" e1 e2 | e1 <- _op1Isexpr, e2 <- _op2Isexpr ]
                   {-# LINE 7186 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 7194 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 7199 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 7204 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 7209 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 7216 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 7221 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Mul _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 7230 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 7235 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7240 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7245 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7250 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7255 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_FMul :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FMul pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 7334 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FMul _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7343 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7348 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 7353 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 7358 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7363 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 7368 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7373 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7378 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 7383 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7388 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 7393 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7398 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7403 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 7408 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_UDiv :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_UDiv pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 7487 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  UDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7496 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7501 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 7506 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 7511 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7516 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 7521 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7526 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7531 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 7536 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7541 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 7546 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7551 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7556 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 7561 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_SDiv :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_SDiv pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 7640 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  SDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7649 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7654 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 7659 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 7664 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7669 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 7674 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7679 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7684 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 7689 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7694 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 7699 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7704 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7709 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 7714 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_FDiv :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FDiv pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 7793 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7802 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7807 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 7812 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 7817 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7822 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 7827 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7832 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7837 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 7842 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7847 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 7852 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7857 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7862 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 7867 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_URem :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_URem pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 7946 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  URem _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7955 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7960 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 7965 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 7970 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 7975 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 7980 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 7985 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7990 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 7995 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 8000 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 8005 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8010 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8015 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 8020 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_SRem :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_SRem pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 8099 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  SRem _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8108 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8113 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 8118 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 8123 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 8128 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 8133 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8138 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8143 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 8148 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 8153 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 8158 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8163 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8168 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 8173 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_FRem :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FRem pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 8252 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FRem _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8261 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8266 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 8271 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 8276 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 8281 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 8286 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8291 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8296 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 8301 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 8306 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 8311 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8316 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8321 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 8326 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Shl :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Shl pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
              _op2Otn :: String
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Osortexpr :: (Maybe SSortExpr)
              _op2Omn :: (Maybe SSort)
              _op2Osortexpr :: (Maybe SSortExpr)
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _idOtn =
                  ({-# LINE 143 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8405 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 144 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 8410 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 145 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 8415 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 146 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 8420 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 147 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8425 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 148 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 8430 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 149 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 8435 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 150 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8440 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 151 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ Prelude.map (\se -> sFn "=" _idIsexpr se) _vse
                   {-# LINE 8445 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 152 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "bin instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 8456 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vse =
                  ({-# LINE 168 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ sFn "bvshl" e1 e2 | e1 <- _op1Isexpr, e2 <- _op2Isexpr ]
                   {-# LINE 8461 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 8469 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 8474 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 8479 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 8484 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 8491 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 8496 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Shl _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 8505 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 8510 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 8515 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8520 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 8525 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8530 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_LShr :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_LShr pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 8609 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  LShr _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8618 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8623 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 8628 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 8633 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 8638 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 8643 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8648 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8653 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 8658 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 8663 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 8668 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8673 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8678 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 8683 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_AShr :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_AShr pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 8762 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  AShr _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8771 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8776 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 8781 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 8786 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 8791 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 8796 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8801 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8806 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 8811 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 8816 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 8821 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 8826 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8831 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 8836 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_And :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_And pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
              _op2Otn :: String
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Osortexpr :: (Maybe SSortExpr)
              _op2Omn :: (Maybe SSort)
              _op2Osortexpr :: (Maybe SSortExpr)
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _idOtn =
                  ({-# LINE 143 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8915 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 144 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 8920 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 145 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 8925 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 146 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 8930 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 147 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8935 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 148 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 8940 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 149 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 8945 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 150 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8950 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 151 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ Prelude.map (\se -> sFn "=" _idIsexpr se) _vse
                   {-# LINE 8955 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 152 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "bin instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 8966 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vse =
                  ({-# LINE 170 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ sFn "and" e1 e2 | e1 <- _op1Isexpr, e2 <- _op2Isexpr ]
                   {-# LINE 8971 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 8979 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 8984 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 8989 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 8994 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 9001 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 9006 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  And _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 9015 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 9020 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 9025 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 9030 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 9035 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 9040 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Or :: T_PC ->
                      T_Identifier ->
                      T_Type ->
                      T_Value ->
                      T_Value ->
                      T_Instruction
sem_Instruction_Or pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
              _op2Otn :: String
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Osortexpr :: (Maybe SSortExpr)
              _op2Omn :: (Maybe SSort)
              _op2Osortexpr :: (Maybe SSortExpr)
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _idOtn =
                  ({-# LINE 143 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 9119 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 144 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 9124 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 145 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 9129 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 146 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 9134 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 147 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 9139 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 148 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 9144 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 149 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 9149 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 150 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 9154 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 151 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ Prelude.map (\se -> sFn "=" _idIsexpr se) _vse
                   {-# LINE 9159 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 152 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "bin instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 9170 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vse =
                  ({-# LINE 166 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ sFn "or" e1 e2 | e1 <- _op1Isexpr, e2 <- _op2Isexpr ]
                   {-# LINE 9175 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 9183 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 9188 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 9193 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 9198 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 9205 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 9210 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Or _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 9219 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 9224 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 9229 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 9234 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 9239 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 9244 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Xor :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Xor pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 9323 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Xor _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 9332 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 9337 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 9342 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 9347 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 9352 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 9357 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 9362 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9367 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 9372 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 9377 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 9382 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 9387 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9392 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 9397 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Alloca :: T_PC ->
                          T_Identifier ->
                          T_Type ->
                          T_Align ->
                          T_Instruction
sem_Instruction_Alloca pc_ id_ ty_ align_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _alignIself :: Align
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 9448 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Alloca _pcIself _idIself _tyIself _alignIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 9457 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 9462 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 9467 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 9472 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _alignIself) =
                  align_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Store :: T_PC ->
                         T_Type ->
                         T_Value ->
                         T_Value ->
                         T_Align ->
                         T_Instruction
sem_Instruction_Store pc_ ty_ v1_ v2_ align_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _v1Omts :: TypeEnv
              _v1Oval :: (Map.Map Id (Type, [PC]))
              _v2Omts :: TypeEnv
              _v2Oval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _v1Omn :: (Maybe SSort)
              _v1Osortexpr :: (Maybe SSortExpr)
              _v1Otn :: String
              _v2Omn :: (Maybe SSort)
              _v2Osortexpr :: (Maybe SSortExpr)
              _v2Otn :: String
              _pcIself :: PC
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _v1Iident :: (Maybe String)
              _v1IisGlobal :: Bool
              _v1Imts :: TypeEnv
              _v1Ipsexpr :: (Int -> [SExpr])
              _v1Iself :: Value
              _v1Isexpr :: ([SExpr])
              _v1Isexprs :: SExpressions
              _v1Isort :: SSortExpr
              _v1Ivtype :: Type
              _v2Iident :: (Maybe String)
              _v2IisGlobal :: Bool
              _v2Imts :: TypeEnv
              _v2Ipsexpr :: (Int -> [SExpr])
              _v2Iself :: Value
              _v2Isexpr :: ([SExpr])
              _v2Isexprs :: SExpressions
              _v2Isort :: SSortExpr
              _v2Ivtype :: Type
              _alignIself :: Align
              _val =
                  ({-# LINE 103 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 9543 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v1Omts =
                  ({-# LINE 104 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 9548 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v1Oval =
                  ({-# LINE 105 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _val
                   {-# LINE 9553 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v2Omts =
                  ({-# LINE 106 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 9558 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v2Oval =
                  ({-# LINE 107 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _val
                   {-# LINE 9563 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v1e =
                  ({-# LINE 108 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   if _v1Isexpr == [] then error "Store Instruction" else head _v1Isexpr
                   {-# LINE 9568 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 109 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            enc  = if _v2IisGlobal
                                                   then let v2i = fromJust _v2Iident
                                                            preds = encPreds le k _pcIself _val     $ Just v2i
                                                            vexpr = encFreshGlobal v2i _pcIself _val
                                                        in wrap sAnd $ (sFn "=" vexpr _v1e    ):preds
                                                   else let preds = encPreds le k _pcIself _val     Nothing
                                                        in case (_v1Isexpr, _v2Isexpr) of
                                                              ([e1],[e2]) -> wrap sAnd $ (sFn "=" e2 e1):preds
                                                              _           -> error "Store simple encoding"
                                            iexp = fpce `sAnd` enc
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "Store instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 9587 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 9595 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 9600 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 9605 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 9610 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 9617 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 9622 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Store _pcIself _tyIself _v1Iself _v2Iself _alignIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 9631 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 9636 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 9641 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 9646 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9651 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 9656 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 9661 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9666 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _v1Iident,_v1IisGlobal,_v1Imts,_v1Ipsexpr,_v1Iself,_v1Isexpr,_v1Isexprs,_v1Isort,_v1Ivtype) =
                  v1_ _v1Omn _v1Omts _v1Osortexpr _v1Otn _v1Oval
              ( _v2Iident,_v2IisGlobal,_v2Imts,_v2Ipsexpr,_v2Iself,_v2Isexpr,_v2Isexprs,_v2Isort,_v2Ivtype) =
                  v2_ _v2Omn _v2Omts _v2Osortexpr _v2Otn _v2Oval
              ( _alignIself) =
                  align_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Load :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Align ->
                        T_Instruction
sem_Instruction_Load pc_ id_ v_ align_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _vOmts :: TypeEnv
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _vOmn :: (Maybe SSort)
              _vOsortexpr :: (Maybe SSortExpr)
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _alignIself :: Align
              _vOmts =
                  ({-# LINE 87 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 9724 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 88 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 9729 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 89 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 9734 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 90 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 9739 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 91 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 9744 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 92 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            ass  = if _vIisGlobal
                                                   then wrap sOr $ Prelude.map (\(pve,ve) -> pve `sAnd` sFn "=" _idIsexpr ve) $ zip (_vIpsexpr k) _vIsexpr
                                                   else wrap sOr $ Prelude.map (\ve -> sFn "=" _idIsexpr ve) _vIsexpr
                                            iexp = wrap sAnd $ fpce:ass:preds
                                        in case npce of
                                             Nothing -> _lhsIspark k `sAnd` iexp
                                             Just e  -> let fnpce = if _npcev     == [] then error "Load Instruction" else sFn "=" e $ head _npcev
                                                        in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 9758 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 9766 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 9771 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 9776 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 9781 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 9788 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 9793 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Load _pcIself _idIself _vIself _alignIself
              _lhsOself =
                  _self
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 9802 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 9807 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _alignIself) =
                  align_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_GetElementPtr :: T_PC ->
                                 T_Identifier ->
                                 T_Type ->
                                 T_Value ->
                                 T_Values ->
                                 T_Instruction
sem_Instruction_GetElementPtr pc_ id_ ty_ struct_ idxs_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _structOmts :: TypeEnv
              _structOtn :: String
              _structOval :: (Map.Map Id (Type, [PC]))
              _idxsOmts :: TypeEnv
              _idxsOtn :: String
              _idxsOval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _structOmn :: (Maybe SSort)
              _structOsortexpr :: (Maybe SSortExpr)
              _idxsOmn :: (Maybe SSort)
              _idxsOsortexpr :: (Maybe SSortExpr)
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _structIident :: (Maybe String)
              _structIisGlobal :: Bool
              _structImts :: TypeEnv
              _structIpsexpr :: (Int -> [SExpr])
              _structIself :: Value
              _structIsexpr :: ([SExpr])
              _structIsexprs :: SExpressions
              _structIsort :: SSortExpr
              _structIvtype :: Type
              _idxsImts :: TypeEnv
              _idxsIself :: Values
              _idxsIsexpr :: ([SExpr])
              _idxsIsexprs :: SExpressions
              _idxsIvtype :: ([Type])
              _idOtn =
                  ({-# LINE 223 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 9880 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 224 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 9885 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOmts =
                  ({-# LINE 225 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 9890 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOtn =
                  ({-# LINE 226 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 9895 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOval =
                  ({-# LINE 227 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 9900 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _size =
                  ({-# LINE 228 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   getIdxSize _structIvtype
                   {-# LINE 9905 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOmts =
                  ({-# LINE 229 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 9910 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOtn =
                  ({-# LINE 230 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 9915 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOval =
                  ({-# LINE 231 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 9920 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _encidx =
                  ({-# LINE 232 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \i -> ExtractExpr [ IdentExpr $ IdxIdent (SimpleSym "extract") [_size     - 1, 0] , i ]
                   {-# LINE 9925 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 233 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ [ sFn "=" _idIsexpr (sFn "select" a (_encidx     i)) | a <- _structIsexpr, i <- tail _idxsIsexpr  ]
                   {-# LINE 9930 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 234 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "GetElementPtr instruction" else  sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 9941 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 9949 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 9954 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 9959 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 9964 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 9971 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 9976 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  GetElementPtr _pcIself _idIself _tyIself _structIself _idxsIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 9985 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 9990 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 9995 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10000 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 10005 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10010 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _structIident,_structIisGlobal,_structImts,_structIpsexpr,_structIself,_structIsexpr,_structIsexprs,_structIsort,_structIvtype) =
                  struct_ _structOmn _structOmts _structOsortexpr _structOtn _structOval
              ( _idxsImts,_idxsIself,_idxsIsexpr,_idxsIsexprs,_idxsIvtype) =
                  idxs_ _idxsOmn _idxsOmts _idxsOsortexpr _idxsOtn _idxsOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Trunc :: T_PC ->
                         T_Identifier ->
                         T_Value ->
                         T_Type ->
                         T_Instruction
sem_Instruction_Trunc pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmn :: (Maybe SSort)
              _vOmts :: TypeEnv
              _vOsortexpr :: (Maybe SSortExpr)
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10074 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Trunc _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10083 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 10088 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 10093 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 10098 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10103 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10108 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 10113 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 10118 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 10123 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_ZExt :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Type ->
                        T_Instruction
sem_Instruction_ZExt pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmn :: (Maybe SSort)
              _vOmts :: TypeEnv
              _vOsortexpr :: (Maybe SSortExpr)
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10185 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ZExt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10194 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 10199 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 10204 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 10209 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10214 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10219 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 10224 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 10229 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 10234 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_SExt :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Type ->
                        T_Instruction
sem_Instruction_SExt pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _vOmts :: TypeEnv
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _vOmn :: (Maybe SSort)
              _vOsortexpr :: (Maybe SSortExpr)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _idOtn =
                  ({-# LINE 208 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 10296 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 209 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 10301 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 210 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 10306 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 211 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 10311 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 212 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 10316 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 213 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   let n = getISize _tyIself - getISize _vIvtype
                   in  wrap sOr $ Prelude.map (\e -> sFn "=" _idIsexpr $ SignExtExpr e n) _vIsexpr
                   {-# LINE 10322 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 215 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                         in case npce of
                                             Nothing -> _lhsIspark k `sAnd` iexp
                                             Just e  -> let fnpce = if _npcev     == [] then error "SExt instruction" else sFn "=" e $ head _npcev
                                                        in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 10333 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 10341 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 10346 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 10351 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 10356 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 10363 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 10368 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  SExt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 10377 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10382 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 10387 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 10392 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_FPToUI :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_FPToUI pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmn :: (Maybe SSort)
              _vOmts :: TypeEnv
              _vOsortexpr :: (Maybe SSortExpr)
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10454 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FPToUI _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10463 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 10468 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 10473 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 10478 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10483 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10488 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 10493 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 10498 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 10503 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_FPToSI :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_FPToSI pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmn :: (Maybe SSort)
              _vOmts :: TypeEnv
              _vOsortexpr :: (Maybe SSortExpr)
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10565 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FPToSI _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10574 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 10579 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 10584 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 10589 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10594 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10599 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 10604 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 10609 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 10614 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_UIToFP :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_UIToFP pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmn :: (Maybe SSort)
              _vOmts :: TypeEnv
              _vOsortexpr :: (Maybe SSortExpr)
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10676 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  UIToFP _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10685 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 10690 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 10695 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 10700 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10705 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10710 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 10715 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 10720 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 10725 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_SIToFP :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_SIToFP pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmn :: (Maybe SSort)
              _vOmts :: TypeEnv
              _vOsortexpr :: (Maybe SSortExpr)
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10787 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  SIToFP _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10796 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 10801 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 10806 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 10811 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10816 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10821 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 10826 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 10831 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 10836 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_FPTrunc :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           T_Type ->
                           T_Instruction
sem_Instruction_FPTrunc pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmn :: (Maybe SSort)
              _vOmts :: TypeEnv
              _vOsortexpr :: (Maybe SSortExpr)
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10898 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FPTrunc _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10907 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 10912 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 10917 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 10922 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 10927 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10932 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 10937 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 10942 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 10947 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_FPExt :: T_PC ->
                         T_Identifier ->
                         T_Value ->
                         T_Type ->
                         T_Instruction
sem_Instruction_FPExt pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmn :: (Maybe SSort)
              _vOmts :: TypeEnv
              _vOsortexpr :: (Maybe SSortExpr)
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 11009 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FPExt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11018 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 11023 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 11028 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 11033 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11038 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 11043 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 11048 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 11053 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 11058 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_PtrToInt :: T_PC ->
                            T_Identifier ->
                            T_Value ->
                            T_Type ->
                            T_Instruction
sem_Instruction_PtrToInt pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmn :: (Maybe SSort)
              _vOmts :: TypeEnv
              _vOsortexpr :: (Maybe SSortExpr)
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 11120 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  PtrToInt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11129 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 11134 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 11139 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 11144 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11149 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 11154 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 11159 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 11164 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 11169 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_IntToPtr :: T_PC ->
                            T_Identifier ->
                            T_Value ->
                            T_Type ->
                            T_Instruction
sem_Instruction_IntToPtr pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmn :: (Maybe SSort)
              _vOmts :: TypeEnv
              _vOsortexpr :: (Maybe SSortExpr)
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 11231 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  IntToPtr _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11240 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 11245 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 11250 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 11255 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11260 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 11265 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 11270 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 11275 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 11280 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_BitCast :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           T_Type ->
                           T_Instruction
sem_Instruction_BitCast pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _vOmts :: TypeEnv
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _vOmn :: (Maybe SSort)
              _vOsortexpr :: (Maybe SSortExpr)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _idOtn =
                  ({-# LINE 191 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 11342 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 192 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 11347 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 193 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 11352 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 194 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 11357 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 195 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 11362 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 196 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   if getISize _tyIself <= getISize _vIvtype
                   then wrap sOr $ Prelude.map (\e -> sFn "=" _idIsexpr $ ExtractExpr [ IdentExpr $ IdxIdent (SimpleSym "extract") [(getISize _tyIself)-1, 0] , e ]) _vIsexpr
                   else let n = getISize _tyIself - getISize _vIvtype
                        in  wrap sOr $ Prelude.map (\e -> sFn "=" _idIsexpr $ ZeroExtExpr e n) _vIsexpr
                   {-# LINE 11370 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 200 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                         in case npce of
                                             Nothing -> _lhsIspark k `sAnd` iexp
                                             Just e  -> let fnpce = if _npcev     == [] then error "Bitcast instruction" else sFn "=" e $ head _npcev
                                                        in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 11381 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 11389 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 11394 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 11399 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 11404 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 11411 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 11416 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  BitCast _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 11425 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11430 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 11435 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 11440 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_ICmp :: T_PC ->
                        T_Identifier ->
                        T_IntPredicate ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_ICmp pc_ id_ cond_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
              _op2Otn :: String
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Osortexpr :: (Maybe SSortExpr)
              _op2Omn :: (Maybe SSort)
              _op2Osortexpr :: (Maybe SSortExpr)
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _condIpred :: String
              _condIself :: IntPredicate
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _idOtn =
                  ({-# LINE 125 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 11520 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 126 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 11525 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 127 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 11530 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 128 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 11535 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 129 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 11540 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 130 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 11545 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 131 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 11550 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 132 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 11555 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vse =
                  ({-# LINE 133 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ if _condIself == IntNE then FnAppExpr (SymIdent $ SimpleSym "not") [ sFn "=" e1 e2 ] else sFn _condIpred e1 e2 | e1 <- _op1Isexpr, e2 <- _op2Isexpr ]
                   {-# LINE 11560 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 134 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ Prelude.map (\se -> sFn "=" _idIsexpr se) _vse
                   {-# LINE 11565 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 135 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "ICmp instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 11576 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 11584 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 11589 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 11594 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 11599 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 11606 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 11611 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ICmp _pcIself _idIself _condIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 11620 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 11625 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 11630 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11635 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 11640 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11645 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _condIpred,_condIself) =
                  cond_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_FCmp :: T_PC ->
                        T_Identifier ->
                        T_RealPredicate ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FCmp pc_ id_ cond_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omn :: (Maybe SSort)
              _op1Omts :: TypeEnv
              _op1Osortexpr :: (Maybe SSortExpr)
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omn :: (Maybe SSort)
              _op2Omts :: TypeEnv
              _op2Osortexpr :: (Maybe SSortExpr)
              _op2Otn :: String
              _op2Oval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _condIself :: RealPredicate
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Iident :: (Maybe String)
              _op1IisGlobal :: Bool
              _op1Imts :: TypeEnv
              _op1Ipsexpr :: (Int -> [SExpr])
              _op1Iself :: Value
              _op1Isexpr :: ([SExpr])
              _op1Isexprs :: SExpressions
              _op1Isort :: SSortExpr
              _op1Ivtype :: Type
              _op2Iident :: (Maybe String)
              _op2IisGlobal :: Bool
              _op2Imts :: TypeEnv
              _op2Ipsexpr :: (Int -> [SExpr])
              _op2Iself :: Value
              _op2Isexpr :: ([SExpr])
              _op2Isexprs :: SExpressions
              _op2Isort :: SSortExpr
              _op2Ivtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 11728 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FCmp _pcIself _idIself _condIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11737 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 11742 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 11747 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 11752 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 11757 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 11762 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11767 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 11772 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 11777 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 11782 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 11787 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11792 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 11797 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 11802 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _condIself) =
                  cond_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omn _op1Omts _op1Osortexpr _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omn _op2Omts _op2Osortexpr _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_PHI :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_PValues ->
                       T_Instruction
sem_Instruction_PHI pc_ id_ ty_ vals_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _valsOmn :: (Maybe SSort)
              _valsOmts :: TypeEnv
              _valsOsortexpr :: (Maybe SSortExpr)
              _valsOtn :: String
              _valsOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _valsIself :: PValues
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 11860 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  PHI _pcIself _idIself _tyIself _valsIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11869 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 11874 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 11879 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 11884 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 11889 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 11894 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 11899 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 11904 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 11909 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _valsIself) =
                  vals_ _valsOmn _valsOmts _valsOsortexpr _valsOtn _valsOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Call :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Identifier ->
                        T_Values ->
                        T_Instruction
sem_Instruction_Call pc_ mres_ ty_ callee_ args_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _mresOsortexpr :: (Maybe SSortExpr)
              _mresOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _calleeOsortexpr :: (Maybe SSortExpr)
              _calleeOtn :: String
              _argsOmn :: (Maybe SSort)
              _argsOmts :: TypeEnv
              _argsOsortexpr :: (Maybe SSortExpr)
              _argsOtn :: String
              _argsOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _mresIdeclexpr :: ([SExpression])
              _mresIident :: String
              _mresIself :: Identifier
              _mresIsexpr :: SExpr
              _mresIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _calleeIdeclexpr :: ([SExpression])
              _calleeIident :: String
              _calleeIself :: Identifier
              _calleeIsexpr :: SExpr
              _calleeIssymbol :: SSymbol
              _argsImts :: TypeEnv
              _argsIself :: Values
              _argsIsexpr :: ([SExpr])
              _argsIsexprs :: SExpressions
              _argsIvtype :: ([Type])
              _ts =
                  ({-# LINE 242 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:preds
                                        in case npce of
                                             Nothing -> _lhsIspark k `sAnd` fpce
                                             Just e  -> let fnpce = if _npcev     == [] then error "Call instruction" else sFn "=" e $ head _npcev
                                                        in  _lhsIspark k `sAnd` fpce `sAnd` fnpce
                   {-# LINE 11981 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 291 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 11989 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 11994 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 11999 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 12004 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 12011 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 12016 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Call _pcIself _mresIself _tyIself _calleeIself _argsIself
              _lhsOself =
                  _self
              _mresOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12025 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mresOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 12030 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 12035 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 12040 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _calleeOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12045 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _calleeOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 12050 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12055 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOmts =
                  ({-# LINE 27 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 12060 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12065 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOtn =
                  ({-# LINE 29 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12070 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOval =
                  ({-# LINE 28 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12075 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _mresIdeclexpr,_mresIident,_mresIself,_mresIsexpr,_mresIssymbol) =
                  mres_ _mresOsortexpr _mresOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _calleeIdeclexpr,_calleeIident,_calleeIself,_calleeIsexpr,_calleeIssymbol) =
                  callee_ _calleeOsortexpr _calleeOtn
              ( _argsImts,_argsIself,_argsIsexpr,_argsIsexprs,_argsIvtype) =
                  args_ _argsOmn _argsOmts _argsOsortexpr _argsOtn _argsOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Select :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Value ->
                          T_Value ->
                          T_Instruction
sem_Instruction_Select pc_ id_ cond_ valt_ valf_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _condOmn :: (Maybe SSort)
              _condOmts :: TypeEnv
              _condOsortexpr :: (Maybe SSortExpr)
              _condOtn :: String
              _condOval :: (Map.Map Id (Type, [PC]))
              _valtOmn :: (Maybe SSort)
              _valtOmts :: TypeEnv
              _valtOsortexpr :: (Maybe SSortExpr)
              _valtOtn :: String
              _valtOval :: (Map.Map Id (Type, [PC]))
              _valfOmn :: (Maybe SSort)
              _valfOmts :: TypeEnv
              _valfOsortexpr :: (Maybe SSortExpr)
              _valfOtn :: String
              _valfOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _condIident :: (Maybe String)
              _condIisGlobal :: Bool
              _condImts :: TypeEnv
              _condIpsexpr :: (Int -> [SExpr])
              _condIself :: Value
              _condIsexpr :: ([SExpr])
              _condIsexprs :: SExpressions
              _condIsort :: SSortExpr
              _condIvtype :: Type
              _valtIident :: (Maybe String)
              _valtIisGlobal :: Bool
              _valtImts :: TypeEnv
              _valtIpsexpr :: (Int -> [SExpr])
              _valtIself :: Value
              _valtIsexpr :: ([SExpr])
              _valtIsexprs :: SExpressions
              _valtIsort :: SSortExpr
              _valtIvtype :: Type
              _valfIident :: (Maybe String)
              _valfIisGlobal :: Bool
              _valfImts :: TypeEnv
              _valfIpsexpr :: (Int -> [SExpr])
              _valfIself :: Value
              _valfIsexpr :: ([SExpr])
              _valfIsexprs :: SExpressions
              _valfIsort :: SSortExpr
              _valfIvtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 12161 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Select _pcIself _idIself _condIself _valtIself _valfIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12170 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 12175 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _condOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12180 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _condOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 12185 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _condOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12190 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _condOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12195 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _condOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12200 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valtOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12205 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valtOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _condImts
                   {-# LINE 12210 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valtOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12215 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valtOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12220 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valtOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12225 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valfOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12230 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valfOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _valtImts
                   {-# LINE 12235 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valfOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12240 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valfOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12245 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valfOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12250 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _condIident,_condIisGlobal,_condImts,_condIpsexpr,_condIself,_condIsexpr,_condIsexprs,_condIsort,_condIvtype) =
                  cond_ _condOmn _condOmts _condOsortexpr _condOtn _condOval
              ( _valtIident,_valtIisGlobal,_valtImts,_valtIpsexpr,_valtIself,_valtIsexpr,_valtIsexprs,_valtIsort,_valtIvtype) =
                  valt_ _valtOmn _valtOmts _valtOsortexpr _valtOtn _valtOval
              ( _valfIident,_valfIisGlobal,_valfImts,_valfIpsexpr,_valfIself,_valfIsexpr,_valfIsexprs,_valfIsort,_valfIvtype) =
                  valf_ _valfOmn _valfOmts _valfOsortexpr _valfOtn _valfOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_ExtractValue :: T_PC ->
                                T_Identifier ->
                                T_Value ->
                                T_Ints ->
                                T_Instruction
sem_Instruction_ExtractValue pc_ id_ aggr_ idxs_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _aggrOmn :: (Maybe SSort)
              _aggrOmts :: TypeEnv
              _aggrOsortexpr :: (Maybe SSortExpr)
              _aggrOtn :: String
              _aggrOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _aggrIident :: (Maybe String)
              _aggrIisGlobal :: Bool
              _aggrImts :: TypeEnv
              _aggrIpsexpr :: (Int -> [SExpr])
              _aggrIself :: Value
              _aggrIsexpr :: ([SExpr])
              _aggrIsexprs :: SExpressions
              _aggrIsort :: SSortExpr
              _aggrIvtype :: Type
              _idxsIself :: Ints
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 12308 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ExtractValue _pcIself _idIself _aggrIself _idxsIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12317 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 12322 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12327 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 12332 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12337 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12342 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12347 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _aggrIident,_aggrIisGlobal,_aggrImts,_aggrIpsexpr,_aggrIself,_aggrIsexpr,_aggrIsexprs,_aggrIsort,_aggrIvtype) =
                  aggr_ _aggrOmn _aggrOmts _aggrOsortexpr _aggrOtn _aggrOval
              ( _idxsIself) =
                  idxs_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_InsertValue :: T_PC ->
                               T_Identifier ->
                               T_Value ->
                               T_Value ->
                               T_Ints ->
                               T_Instruction
sem_Instruction_InsertValue pc_ id_ aggr_ ival_ idxs_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _aggrOmn :: (Maybe SSort)
              _aggrOmts :: TypeEnv
              _aggrOsortexpr :: (Maybe SSortExpr)
              _aggrOtn :: String
              _aggrOval :: (Map.Map Id (Type, [PC]))
              _ivalOmn :: (Maybe SSort)
              _ivalOmts :: TypeEnv
              _ivalOsortexpr :: (Maybe SSortExpr)
              _ivalOtn :: String
              _ivalOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _aggrIident :: (Maybe String)
              _aggrIisGlobal :: Bool
              _aggrImts :: TypeEnv
              _aggrIpsexpr :: (Int -> [SExpr])
              _aggrIself :: Value
              _aggrIsexpr :: ([SExpr])
              _aggrIsexprs :: SExpressions
              _aggrIsort :: SSortExpr
              _aggrIvtype :: Type
              _ivalIident :: (Maybe String)
              _ivalIisGlobal :: Bool
              _ivalImts :: TypeEnv
              _ivalIpsexpr :: (Int -> [SExpr])
              _ivalIself :: Value
              _ivalIsexpr :: ([SExpr])
              _ivalIsexprs :: SExpressions
              _ivalIsort :: SSortExpr
              _ivalIvtype :: Type
              _idxsIself :: Ints
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 12418 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  InsertValue _pcIself _idIself _aggrIself _ivalIself _idxsIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12427 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 12432 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12437 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 12442 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12447 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12452 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12457 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12462 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _aggrImts
                   {-# LINE 12467 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12472 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12477 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12482 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _aggrIident,_aggrIisGlobal,_aggrImts,_aggrIpsexpr,_aggrIself,_aggrIsexpr,_aggrIsexprs,_aggrIsort,_aggrIvtype) =
                  aggr_ _aggrOmn _aggrOmts _aggrOsortexpr _aggrOtn _aggrOval
              ( _ivalIident,_ivalIisGlobal,_ivalImts,_ivalIpsexpr,_ivalIself,_ivalIsexpr,_ivalIsexprs,_ivalIsort,_ivalIvtype) =
                  ival_ _ivalOmn _ivalOmts _ivalOsortexpr _ivalOtn _ivalOval
              ( _idxsIself) =
                  idxs_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Cmpxchg :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           T_Value ->
                           T_Value ->
                           T_AtomicOrdering ->
                           T_Instruction
sem_Instruction_Cmpxchg pc_ id_ mptr_ cval_ nval_ ord_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _mptrOmn :: (Maybe SSort)
              _mptrOmts :: TypeEnv
              _mptrOsortexpr :: (Maybe SSortExpr)
              _mptrOtn :: String
              _mptrOval :: (Map.Map Id (Type, [PC]))
              _cvalOmn :: (Maybe SSort)
              _cvalOmts :: TypeEnv
              _cvalOsortexpr :: (Maybe SSortExpr)
              _cvalOtn :: String
              _cvalOval :: (Map.Map Id (Type, [PC]))
              _nvalOmn :: (Maybe SSort)
              _nvalOmts :: TypeEnv
              _nvalOsortexpr :: (Maybe SSortExpr)
              _nvalOtn :: String
              _nvalOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _mptrIident :: (Maybe String)
              _mptrIisGlobal :: Bool
              _mptrImts :: TypeEnv
              _mptrIpsexpr :: (Int -> [SExpr])
              _mptrIself :: Value
              _mptrIsexpr :: ([SExpr])
              _mptrIsexprs :: SExpressions
              _mptrIsort :: SSortExpr
              _mptrIvtype :: Type
              _cvalIident :: (Maybe String)
              _cvalIisGlobal :: Bool
              _cvalImts :: TypeEnv
              _cvalIpsexpr :: (Int -> [SExpr])
              _cvalIself :: Value
              _cvalIsexpr :: ([SExpr])
              _cvalIsexprs :: SExpressions
              _cvalIsort :: SSortExpr
              _cvalIvtype :: Type
              _nvalIident :: (Maybe String)
              _nvalIisGlobal :: Bool
              _nvalImts :: TypeEnv
              _nvalIpsexpr :: (Int -> [SExpr])
              _nvalIself :: Value
              _nvalIsexpr :: ([SExpr])
              _nvalIsexprs :: SExpressions
              _nvalIsort :: SSortExpr
              _nvalIvtype :: Type
              _ordIself :: AtomicOrdering
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 12570 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Cmpxchg _pcIself _idIself _mptrIself _cvalIself _nvalIself _ordIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12579 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 12584 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mptrOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12589 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mptrOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 12594 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mptrOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12599 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mptrOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12604 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mptrOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12609 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cvalOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12614 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cvalOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _mptrImts
                   {-# LINE 12619 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cvalOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12624 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cvalOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12629 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cvalOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12634 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nvalOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12639 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nvalOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cvalImts
                   {-# LINE 12644 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nvalOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12649 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nvalOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12654 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nvalOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12659 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _mptrIident,_mptrIisGlobal,_mptrImts,_mptrIpsexpr,_mptrIself,_mptrIsexpr,_mptrIsexprs,_mptrIsort,_mptrIvtype) =
                  mptr_ _mptrOmn _mptrOmts _mptrOsortexpr _mptrOtn _mptrOval
              ( _cvalIident,_cvalIisGlobal,_cvalImts,_cvalIpsexpr,_cvalIself,_cvalIsexpr,_cvalIsexprs,_cvalIsort,_cvalIvtype) =
                  cval_ _cvalOmn _cvalOmts _cvalOsortexpr _cvalOtn _cvalOval
              ( _nvalIident,_nvalIisGlobal,_nvalImts,_nvalIpsexpr,_nvalIself,_nvalIsexpr,_nvalIsexprs,_nvalIsort,_nvalIvtype) =
                  nval_ _nvalOmn _nvalOmts _nvalOsortexpr _nvalOtn _nvalOval
              ( _ordIself) =
                  ord_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_AtomicRMW :: T_PC ->
                             T_Identifier ->
                             T_Values ->
                             T_BinOp ->
                             T_AtomicOrdering ->
                             T_Instruction
sem_Instruction_AtomicRMW pc_ id_ args_ op_ ord_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _argsOmn :: (Maybe SSort)
              _argsOmts :: TypeEnv
              _argsOsortexpr :: (Maybe SSortExpr)
              _argsOtn :: String
              _argsOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _idIdeclexpr :: ([SExpression])
              _idIident :: String
              _idIself :: Identifier
              _idIsexpr :: SExpr
              _idIssymbol :: SSymbol
              _argsImts :: TypeEnv
              _argsIself :: Values
              _argsIsexpr :: ([SExpr])
              _argsIsexprs :: SExpressions
              _argsIvtype :: ([Type])
              _opIself :: BinOp
              _ordIself :: AtomicOrdering
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 12717 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  AtomicRMW _pcIself _idIself _argsIself _opIself _ordIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12726 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 12731 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12736 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOmts =
                  ({-# LINE 27 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 12741 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12746 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOtn =
                  ({-# LINE 29 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12751 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOval =
                  ({-# LINE 28 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12756 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _argsImts,_argsIself,_argsIsexpr,_argsIsexprs,_argsIvtype) =
                  args_ _argsOmn _argsOmts _argsOsortexpr _argsOtn _argsOval
              ( _opIself) =
                  op_
              ( _ordIself) =
                  ord_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_CreateThread :: T_PC ->
                                T_Values ->
                                T_Instruction
sem_Instruction_CreateThread pc_ args_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _argsOmn :: (Maybe SSort)
              _argsOmts :: TypeEnv
              _argsOsortexpr :: (Maybe SSortExpr)
              _argsOtn :: String
              _argsOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _argsImts :: TypeEnv
              _argsIself :: Values
              _argsIsexpr :: ([SExpr])
              _argsIsexprs :: SExpressions
              _argsIvtype :: ([Type])
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 12800 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  CreateThread _pcIself _argsIself
              _lhsOself =
                  _self
              _argsOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12809 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOmts =
                  ({-# LINE 27 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 12814 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12819 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOtn =
                  ({-# LINE 29 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12824 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOval =
                  ({-# LINE 28 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12829 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _argsImts,_argsIself,_argsIsexpr,_argsIsexprs,_argsIvtype) =
                  args_ _argsOmn _argsOmts _argsOsortexpr _argsOtn _argsOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_MutexInit :: T_PC ->
                             T_Identifier ->
                             T_Value ->
                             T_Instruction
sem_Instruction_MutexInit pc_ rv_ mutex_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _rvOsortexpr :: (Maybe SSortExpr)
              _rvOtn :: String
              _mutexOmn :: (Maybe SSort)
              _mutexOmts :: TypeEnv
              _mutexOsortexpr :: (Maybe SSortExpr)
              _mutexOtn :: String
              _mutexOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _rvIdeclexpr :: ([SExpression])
              _rvIident :: String
              _rvIself :: Identifier
              _rvIsexpr :: SExpr
              _rvIssymbol :: SSymbol
              _mutexIident :: (Maybe String)
              _mutexIisGlobal :: Bool
              _mutexImts :: TypeEnv
              _mutexIpsexpr :: (Int -> [SExpr])
              _mutexIself :: Value
              _mutexIsexpr :: ([SExpr])
              _mutexIsexprs :: SExpressions
              _mutexIsort :: SSortExpr
              _mutexIvtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 12879 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  MutexInit _pcIself _rvIself _mutexIself
              _lhsOself =
                  _self
              _rvOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12888 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rvOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 12893 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 12898 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 12903 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 12908 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12913 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12918 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _rvIdeclexpr,_rvIident,_rvIself,_rvIsexpr,_rvIssymbol) =
                  rv_ _rvOsortexpr _rvOtn
              ( _mutexIident,_mutexIisGlobal,_mutexImts,_mutexIpsexpr,_mutexIself,_mutexIsexpr,_mutexIsexprs,_mutexIsort,_mutexIvtype) =
                  mutex_ _mutexOmn _mutexOmts _mutexOsortexpr _mutexOtn _mutexOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_MutexLock :: T_PC ->
                             T_Identifier ->
                             T_Value ->
                             T_Instruction
sem_Instruction_MutexLock pc_ rv_ mutex_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _rvOtn :: String
              _rvOsortexpr :: (Maybe SSortExpr)
              _mutexOmts :: TypeEnv
              _mutexOtn :: String
              _mutexOval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _mutexOmn :: (Maybe SSort)
              _mutexOsortexpr :: (Maybe SSortExpr)
              _pcIself :: PC
              _rvIdeclexpr :: ([SExpression])
              _rvIident :: String
              _rvIself :: Identifier
              _rvIsexpr :: SExpr
              _rvIssymbol :: SSymbol
              _mutexIident :: (Maybe String)
              _mutexIisGlobal :: Bool
              _mutexImts :: TypeEnv
              _mutexIpsexpr :: (Int -> [SExpr])
              _mutexIself :: Value
              _mutexIsexpr :: ([SExpr])
              _mutexIsexprs :: SExpressions
              _mutexIsort :: SSortExpr
              _mutexIvtype :: Type
              _rvOtn =
                  ({-# LINE 255 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 12970 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rvOsortexpr =
                  ({-# LINE 256 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 12975 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOmts =
                  ({-# LINE 257 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 12980 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOtn =
                  ({-# LINE 258 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 12985 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOval =
                  ({-# LINE 259 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 12990 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 260 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            mid  = fromJust _mutexIident
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            f = IdentExpr $ SymIdent $ SimpleSym "false"
                                            enc = sFn "=" (IdentExpr $ SymIdent $ SimpleSym $ mid ++ show k) f
                                            iexp = wrap sAnd $ fpce:enc:preds
                                         in case npce of
                                             Nothing -> _lhsIspark k `sAnd` iexp
                                             Just e  -> let fnpce = if _npcev     == [] then error "mutexlock instruction" else sFn "=" e $ head _npcev
                                                        in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 13004 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 296 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then error "Should not happen"
                         else let cmut = _lhsImutexes !! k
                                  xx = IdentExpr $ SymIdent $ SimpleSym $ fromJust _mutexIident ++ show k
                              in Prelude.foldr (\(x,y) r -> (maybe [] (\yy -> if x == xx then [yy] else [sFn "=" yy x] ) y) ++ r) [] cmut
                   {-# LINE 13013 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 13018 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 13023 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 13028 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 13035 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 13040 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  MutexLock _pcIself _rvIself _mutexIself
              _lhsOself =
                  _self
              _mutexOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 13049 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 13054 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _rvIdeclexpr,_rvIident,_rvIself,_rvIsexpr,_rvIssymbol) =
                  rv_ _rvOsortexpr _rvOtn
              ( _mutexIident,_mutexIisGlobal,_mutexImts,_mutexIpsexpr,_mutexIself,_mutexIsexpr,_mutexIsexprs,_mutexIsort,_mutexIvtype) =
                  mutex_ _mutexOmn _mutexOmts _mutexOsortexpr _mutexOtn _mutexOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_MutexUnlock :: T_PC ->
                               T_Identifier ->
                               T_Value ->
                               T_Instruction
sem_Instruction_MutexUnlock pc_ rv_ mutex_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _rvOtn :: String
              _rvOsortexpr :: (Maybe SSortExpr)
              _mutexOmts :: TypeEnv
              _mutexOtn :: String
              _mutexOval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _mutexOmn :: (Maybe SSort)
              _mutexOsortexpr :: (Maybe SSortExpr)
              _pcIself :: PC
              _rvIdeclexpr :: ([SExpression])
              _rvIident :: String
              _rvIself :: Identifier
              _rvIsexpr :: SExpr
              _rvIssymbol :: SSymbol
              _mutexIident :: (Maybe String)
              _mutexIisGlobal :: Bool
              _mutexImts :: TypeEnv
              _mutexIpsexpr :: (Int -> [SExpr])
              _mutexIself :: Value
              _mutexIsexpr :: ([SExpr])
              _mutexIsexprs :: SExpressions
              _mutexIsort :: SSortExpr
              _mutexIvtype :: Type
              _rvOtn =
                  ({-# LINE 273 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 13106 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rvOsortexpr =
                  ({-# LINE 274 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 13111 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOmts =
                  ({-# LINE 275 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 13116 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOtn =
                  ({-# LINE 276 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 13121 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOval =
                  ({-# LINE 277 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 13126 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 278 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            mid  = fromJust _mutexIident
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            enc = IdentExpr $ SymIdent $ SimpleSym $ mid ++ show k
                                            iexp = wrap sAnd $ fpce:enc:preds
                                         in case npce of
                                             Nothing -> _lhsIspark k `sAnd` iexp
                                             Just e  -> let fnpce = if _npcev     == [] then error "mutexunlock instruction" else sFn "=" e $ head _npcev
                                                        in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 13139 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 302 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then error "Should not happen"
                         else let cmut = _lhsImutexes !! k
                                  xx = IdentExpr $ SymIdent $ SimpleSym $ fromJust _mutexIident ++ show k
                                  f = IdentExpr $ SymIdent $ SimpleSym "false"
                              in Prelude.foldr (\(x,y) r -> (maybe [] (\yy -> if x == xx then [sFn "=" yy f] else [sFn "=" yy x] ) y) ++ r) [] cmut
                   {-# LINE 13149 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 309 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 13154 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 310 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 13159 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 311 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 13164 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 312 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 13171 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 315 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 13176 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  MutexUnlock _pcIself _rvIself _mutexIself
              _lhsOself =
                  _self
              _mutexOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 13185 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 13190 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _rvIdeclexpr,_rvIident,_rvIself,_rvIsexpr,_rvIssymbol) =
                  rv_ _rvOsortexpr _rvOtn
              ( _mutexIident,_mutexIisGlobal,_mutexImts,_mutexIpsexpr,_mutexIself,_mutexIsexpr,_mutexIsexprs,_mutexIsort,_mutexIvtype) =
                  mutex_ _mutexOmn _mutexOmts _mutexOsortexpr _mutexOtn _mutexOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_WaitEvent :: T_PC ->
                             Int ->
                             T_Instruction
sem_Instruction_WaitEvent pc_ event_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _pcIself :: PC
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 13220 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  WaitEvent _pcIself event_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_NotifyEvent :: T_PC ->
                               Int ->
                               T_Instruction
sem_Instruction_NotifyEvent pc_ event_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _pcIself :: PC
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 13250 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  NotifyEvent _pcIself event_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_WaitTime :: T_PC ->
                            T_Value ->
                            T_Instruction
sem_Instruction_WaitTime pc_ time_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _timeOmn :: (Maybe SSort)
              _timeOmts :: TypeEnv
              _timeOsortexpr :: (Maybe SSortExpr)
              _timeOtn :: String
              _timeOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _timeIident :: (Maybe String)
              _timeIisGlobal :: Bool
              _timeImts :: TypeEnv
              _timeIpsexpr :: (Int -> [SExpr])
              _timeIself :: Value
              _timeIsexpr :: ([SExpr])
              _timeIsexprs :: SExpressions
              _timeIsort :: SSortExpr
              _timeIvtype :: Type
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 13294 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  WaitTime _pcIself _timeIself
              _lhsOself =
                  _self
              _timeOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 13303 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _timeOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 13308 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _timeOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 13313 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _timeOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 13318 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _timeOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 13323 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _timeIident,_timeIisGlobal,_timeImts,_timeIpsexpr,_timeIself,_timeIsexpr,_timeIsexprs,_timeIsort,_timeIvtype) =
                  time_ _timeOmn _timeOmts _timeOsortexpr _timeOtn _timeOval
          in  ( _lhsOself,_lhsOts)))
-- Instructions ------------------------------------------------
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = CF ->
                      (Map.Map Identifier PC) ->
                      (Maybe SSort) ->
                      TypeEnv ->
                      ([[(SExpr, Maybe SExpr)]]) ->
                      (Map.Map Identifier [PC]) ->
                      PreEncoder ->
                      (Maybe SSortExpr) ->
                      (Int -> SExpr) ->
                      Id ->
                      (Map.Map Id (Type, [PC])) ->
                      ( Instructions,([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr]))
data Inh_Instructions = Inh_Instructions {cfg_Inh_Instructions :: CF,cte_Inh_Instructions :: (Map.Map Identifier PC),mn_Inh_Instructions :: (Maybe SSort),mts_Inh_Instructions :: TypeEnv,mutexes_Inh_Instructions :: ([[(SExpr, Maybe SExpr)]]),pcs_Inh_Instructions :: (Map.Map Identifier [PC]),prenc_Inh_Instructions :: PreEncoder,sortexpr_Inh_Instructions :: (Maybe SSortExpr),spark_Inh_Instructions :: (Int -> SExpr),tn_Inh_Instructions :: Id,val_Inh_Instructions :: (Map.Map Id (Type, [PC]))}
data Syn_Instructions = Syn_Instructions {self_Syn_Instructions :: Instructions,ts_Syn_Instructions :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])}
wrap_Instructions :: T_Instructions ->
                     Inh_Instructions ->
                     Syn_Instructions
wrap_Instructions sem (Inh_Instructions _lhsIcfg _lhsIcte _lhsImn _lhsImts _lhsImutexes _lhsIpcs _lhsIprenc _lhsIsortexpr _lhsIspark _lhsItn _lhsIval) =
    (let ( _lhsOself,_lhsOts) = sem _lhsIcfg _lhsIcte _lhsImn _lhsImts _lhsImutexes _lhsIpcs _lhsIprenc _lhsIsortexpr _lhsIspark _lhsItn _lhsIval
     in  (Syn_Instructions _lhsOself _lhsOts))
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instructions
              _hdOcfg :: CF
              _hdOcte :: (Map.Map Identifier PC)
              _hdOmn :: (Maybe SSort)
              _hdOmts :: TypeEnv
              _hdOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _hdOpcs :: (Map.Map Identifier [PC])
              _hdOprenc :: PreEncoder
              _hdOsortexpr :: (Maybe SSortExpr)
              _hdOspark :: (Int -> SExpr)
              _hdOtn :: Id
              _hdOval :: (Map.Map Id (Type, [PC]))
              _tlOcfg :: CF
              _tlOcte :: (Map.Map Identifier PC)
              _tlOmn :: (Maybe SSort)
              _tlOmts :: TypeEnv
              _tlOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _tlOpcs :: (Map.Map Identifier [PC])
              _tlOprenc :: PreEncoder
              _tlOsortexpr :: (Maybe SSortExpr)
              _tlOspark :: (Int -> SExpr)
              _tlOtn :: Id
              _tlOval :: (Map.Map Id (Type, [PC]))
              _hdIself :: Instruction
              _hdIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _tlIself :: Instructions
              _tlIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _hdIts ++ _tlIts
                   {-# LINE 13403 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOcfg =
                  ({-# LINE 68 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 13412 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOcte =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 13417 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 13422 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 13427 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmutexes =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 13432 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOpcs =
                  ({-# LINE 70 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIpcs
                   {-# LINE 13437 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOprenc =
                  ({-# LINE 72 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 13442 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 13447 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOspark =
                  ({-# LINE 73 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIspark
                   {-# LINE 13452 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 71 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 13457 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOval =
                  ({-# LINE 78 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 13462 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOcfg =
                  ({-# LINE 68 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 13467 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOcte =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 13472 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 13477 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 13482 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmutexes =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 13487 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOpcs =
                  ({-# LINE 70 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIpcs
                   {-# LINE 13492 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOprenc =
                  ({-# LINE 72 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 13497 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 13502 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOspark =
                  ({-# LINE 73 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIspark
                   {-# LINE 13507 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 71 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 13512 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 13517 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIself,_hdIts) =
                  hd_ _hdOcfg _hdOcte _hdOmn _hdOmts _hdOmutexes _hdOpcs _hdOprenc _hdOsortexpr _hdOspark _hdOtn _hdOval
              ( _tlIself,_tlIts) =
                  tl_ _tlOcfg _tlOcte _tlOmn _tlOmts _tlOmutexes _tlOpcs _tlOprenc _tlOsortexpr _tlOspark _tlOtn _tlOval
          in  ( _lhsOself,_lhsOts)))
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImn
       _lhsImts
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIsortexpr
       _lhsIspark
       _lhsItn
       _lhsIval ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instructions
              _lhsOts =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 13542 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOself,_lhsOts)))
-- IntPredicate ------------------------------------------------
-- cata
sem_IntPredicate :: IntPredicate ->
                    T_IntPredicate
sem_IntPredicate (IntEQ) =
    (sem_IntPredicate_IntEQ)
sem_IntPredicate (IntNE) =
    (sem_IntPredicate_IntNE)
sem_IntPredicate (IntUGT) =
    (sem_IntPredicate_IntUGT)
sem_IntPredicate (IntUGE) =
    (sem_IntPredicate_IntUGE)
sem_IntPredicate (IntULT) =
    (sem_IntPredicate_IntULT)
sem_IntPredicate (IntULE) =
    (sem_IntPredicate_IntULE)
sem_IntPredicate (IntSGT) =
    (sem_IntPredicate_IntSGT)
sem_IntPredicate (IntSGE) =
    (sem_IntPredicate_IntSGE)
sem_IntPredicate (IntSLT) =
    (sem_IntPredicate_IntSLT)
sem_IntPredicate (IntSLE) =
    (sem_IntPredicate_IntSLE)
-- semantic domain
type T_IntPredicate = ( String,IntPredicate)
data Inh_IntPredicate = Inh_IntPredicate {}
data Syn_IntPredicate = Syn_IntPredicate {pred_Syn_IntPredicate :: String,self_Syn_IntPredicate :: IntPredicate}
wrap_IntPredicate :: T_IntPredicate ->
                     Inh_IntPredicate ->
                     Syn_IntPredicate
wrap_IntPredicate sem (Inh_IntPredicate) =
    (let ( _lhsOpred,_lhsOself) = sem
     in  (Syn_IntPredicate _lhsOpred _lhsOself))
sem_IntPredicate_IntEQ :: T_IntPredicate
sem_IntPredicate_IntEQ =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 321 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              "="
              {-# LINE 13590 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntEQ
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntNE :: T_IntPredicate
sem_IntPredicate_IntNE =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 322 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              error "IntNE"
              {-# LINE 13604 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntNE
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntUGT :: T_IntPredicate
sem_IntPredicate_IntUGT =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 323 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvugt"
              {-# LINE 13618 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntUGT
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntUGE :: T_IntPredicate
sem_IntPredicate_IntUGE =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 324 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvuge"
              {-# LINE 13632 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntUGE
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntULT :: T_IntPredicate
sem_IntPredicate_IntULT =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 325 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvult"
              {-# LINE 13646 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntULT
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntULE :: T_IntPredicate
sem_IntPredicate_IntULE =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 326 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvule"
              {-# LINE 13660 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntULE
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntSGT :: T_IntPredicate
sem_IntPredicate_IntSGT =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 327 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvsgt"
              {-# LINE 13674 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntSGT
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntSGE :: T_IntPredicate
sem_IntPredicate_IntSGE =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 328 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvsge"
              {-# LINE 13688 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntSGE
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntSLT :: T_IntPredicate
sem_IntPredicate_IntSLT =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 329 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvslt"
              {-# LINE 13702 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntSLT
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntSLE :: T_IntPredicate
sem_IntPredicate_IntSLE =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 330 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvsle"
              {-# LINE 13716 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntSLE
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
-- Ints --------------------------------------------------------
-- cata
sem_Ints :: Ints ->
            T_Ints
sem_Ints list =
    (Prelude.foldr sem_Ints_Cons sem_Ints_Nil list)
-- semantic domain
type T_Ints = ( Ints)
data Inh_Ints = Inh_Ints {}
data Syn_Ints = Syn_Ints {self_Syn_Ints :: Ints}
wrap_Ints :: T_Ints ->
             Inh_Ints ->
             Syn_Ints
wrap_Ints sem (Inh_Ints) =
    (let ( _lhsOself) = sem
     in  (Syn_Ints _lhsOself))
sem_Ints_Cons :: Int ->
                 T_Ints ->
                 T_Ints
sem_Ints_Cons hd_ tl_ =
    (let _lhsOself :: Ints
         _tlIself :: Ints
         _self =
             (:) hd_ _tlIself
         _lhsOself =
             _self
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Ints_Nil :: T_Ints
sem_Ints_Nil =
    (let _lhsOself :: Ints
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Linkage -----------------------------------------------------
-- cata
sem_Linkage :: Linkage ->
               T_Linkage
sem_Linkage (ExternalLinkage) =
    (sem_Linkage_ExternalLinkage)
sem_Linkage (AvailableExternallyLinkage) =
    (sem_Linkage_AvailableExternallyLinkage)
sem_Linkage (LinkOnceAnyLinkage) =
    (sem_Linkage_LinkOnceAnyLinkage)
sem_Linkage (LinkOnceODRLinkage) =
    (sem_Linkage_LinkOnceODRLinkage)
sem_Linkage (WeakAnyLinkage) =
    (sem_Linkage_WeakAnyLinkage)
sem_Linkage (WeakODRLinkage) =
    (sem_Linkage_WeakODRLinkage)
sem_Linkage (AppendingLinkage) =
    (sem_Linkage_AppendingLinkage)
sem_Linkage (InternalLinkage) =
    (sem_Linkage_InternalLinkage)
sem_Linkage (PrivateLinkage) =
    (sem_Linkage_PrivateLinkage)
sem_Linkage (DLLImportLinkage) =
    (sem_Linkage_DLLImportLinkage)
sem_Linkage (DLLExportLinkage) =
    (sem_Linkage_DLLExportLinkage)
sem_Linkage (ExternalWeakLinkage) =
    (sem_Linkage_ExternalWeakLinkage)
sem_Linkage (GhostLinkage) =
    (sem_Linkage_GhostLinkage)
sem_Linkage (CommonLinkage) =
    (sem_Linkage_CommonLinkage)
sem_Linkage (LinkerPrivateLinkage) =
    (sem_Linkage_LinkerPrivateLinkage)
sem_Linkage (LinkerPrivateWeakLinkage) =
    (sem_Linkage_LinkerPrivateWeakLinkage)
sem_Linkage (LinkerPrivateWeakDefAutoLinkage) =
    (sem_Linkage_LinkerPrivateWeakDefAutoLinkage)
-- semantic domain
type T_Linkage = ( Linkage)
data Inh_Linkage = Inh_Linkage {}
data Syn_Linkage = Syn_Linkage {self_Syn_Linkage :: Linkage}
wrap_Linkage :: T_Linkage ->
                Inh_Linkage ->
                Syn_Linkage
wrap_Linkage sem (Inh_Linkage) =
    (let ( _lhsOself) = sem
     in  (Syn_Linkage _lhsOself))
sem_Linkage_ExternalLinkage :: T_Linkage
sem_Linkage_ExternalLinkage =
    (let _lhsOself :: Linkage
         _self =
             ExternalLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_AvailableExternallyLinkage :: T_Linkage
sem_Linkage_AvailableExternallyLinkage =
    (let _lhsOself :: Linkage
         _self =
             AvailableExternallyLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_LinkOnceAnyLinkage :: T_Linkage
sem_Linkage_LinkOnceAnyLinkage =
    (let _lhsOself :: Linkage
         _self =
             LinkOnceAnyLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_LinkOnceODRLinkage :: T_Linkage
sem_Linkage_LinkOnceODRLinkage =
    (let _lhsOself :: Linkage
         _self =
             LinkOnceODRLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_WeakAnyLinkage :: T_Linkage
sem_Linkage_WeakAnyLinkage =
    (let _lhsOself :: Linkage
         _self =
             WeakAnyLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_WeakODRLinkage :: T_Linkage
sem_Linkage_WeakODRLinkage =
    (let _lhsOself :: Linkage
         _self =
             WeakODRLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_AppendingLinkage :: T_Linkage
sem_Linkage_AppendingLinkage =
    (let _lhsOself :: Linkage
         _self =
             AppendingLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_InternalLinkage :: T_Linkage
sem_Linkage_InternalLinkage =
    (let _lhsOself :: Linkage
         _self =
             InternalLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_PrivateLinkage :: T_Linkage
sem_Linkage_PrivateLinkage =
    (let _lhsOself :: Linkage
         _self =
             PrivateLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_DLLImportLinkage :: T_Linkage
sem_Linkage_DLLImportLinkage =
    (let _lhsOself :: Linkage
         _self =
             DLLImportLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_DLLExportLinkage :: T_Linkage
sem_Linkage_DLLExportLinkage =
    (let _lhsOself :: Linkage
         _self =
             DLLExportLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_ExternalWeakLinkage :: T_Linkage
sem_Linkage_ExternalWeakLinkage =
    (let _lhsOself :: Linkage
         _self =
             ExternalWeakLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_GhostLinkage :: T_Linkage
sem_Linkage_GhostLinkage =
    (let _lhsOself :: Linkage
         _self =
             GhostLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_CommonLinkage :: T_Linkage
sem_Linkage_CommonLinkage =
    (let _lhsOself :: Linkage
         _self =
             CommonLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_LinkerPrivateLinkage :: T_Linkage
sem_Linkage_LinkerPrivateLinkage =
    (let _lhsOself :: Linkage
         _self =
             LinkerPrivateLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_LinkerPrivateWeakLinkage :: T_Linkage
sem_Linkage_LinkerPrivateWeakLinkage =
    (let _lhsOself :: Linkage
         _self =
             LinkerPrivateWeakLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_LinkerPrivateWeakDefAutoLinkage :: T_Linkage
sem_Linkage_LinkerPrivateWeakDefAutoLinkage =
    (let _lhsOself :: Linkage
         _self =
             LinkerPrivateWeakDefAutoLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MAlign ------------------------------------------------------
-- cata
sem_MAlign :: MAlign ->
              T_MAlign
sem_MAlign (Prelude.Just x) =
    (sem_MAlign_Just (sem_Align x))
sem_MAlign Prelude.Nothing =
    sem_MAlign_Nothing
-- semantic domain
type T_MAlign = ( MAlign)
data Inh_MAlign = Inh_MAlign {}
data Syn_MAlign = Syn_MAlign {self_Syn_MAlign :: MAlign}
wrap_MAlign :: T_MAlign ->
               Inh_MAlign ->
               Syn_MAlign
wrap_MAlign sem (Inh_MAlign) =
    (let ( _lhsOself) = sem
     in  (Syn_MAlign _lhsOself))
sem_MAlign_Just :: T_Align ->
                   T_MAlign
sem_MAlign_Just just_ =
    (let _lhsOself :: MAlign
         _justIself :: Align
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
     in  ( _lhsOself))
sem_MAlign_Nothing :: T_MAlign
sem_MAlign_Nothing =
    (let _lhsOself :: MAlign
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MAttributes -------------------------------------------------
-- cata
sem_MAttributes :: MAttributes ->
                   T_MAttributes
sem_MAttributes (Prelude.Just x) =
    (sem_MAttributes_Just (sem_Attributes x))
sem_MAttributes Prelude.Nothing =
    sem_MAttributes_Nothing
-- semantic domain
type T_MAttributes = ( MAttributes)
data Inh_MAttributes = Inh_MAttributes {}
data Syn_MAttributes = Syn_MAttributes {self_Syn_MAttributes :: MAttributes}
wrap_MAttributes :: T_MAttributes ->
                    Inh_MAttributes ->
                    Syn_MAttributes
wrap_MAttributes sem (Inh_MAttributes) =
    (let ( _lhsOself) = sem
     in  (Syn_MAttributes _lhsOself))
sem_MAttributes_Just :: T_Attributes ->
                        T_MAttributes
sem_MAttributes_Just just_ =
    (let _lhsOself :: MAttributes
         _justIself :: Attributes
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
     in  ( _lhsOself))
sem_MAttributes_Nothing :: T_MAttributes
sem_MAttributes_Nothing =
    (let _lhsOself :: MAttributes
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MCConv ------------------------------------------------------
-- cata
sem_MCConv :: MCConv ->
              T_MCConv
sem_MCConv (Prelude.Just x) =
    (sem_MCConv_Just (sem_CConv x))
sem_MCConv Prelude.Nothing =
    sem_MCConv_Nothing
-- semantic domain
type T_MCConv = ( MCConv)
data Inh_MCConv = Inh_MCConv {}
data Syn_MCConv = Syn_MCConv {self_Syn_MCConv :: MCConv}
wrap_MCConv :: T_MCConv ->
               Inh_MCConv ->
               Syn_MCConv
wrap_MCConv sem (Inh_MCConv) =
    (let ( _lhsOself) = sem
     in  (Syn_MCConv _lhsOself))
sem_MCConv_Just :: T_CConv ->
                   T_MCConv
sem_MCConv_Just just_ =
    (let _lhsOself :: MCConv
         _justIself :: CConv
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
     in  ( _lhsOself))
sem_MCConv_Nothing :: T_MCConv
sem_MCConv_Nothing =
    (let _lhsOself :: MCConv
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MConstant ---------------------------------------------------
-- cata
sem_MConstant :: MConstant ->
                 T_MConstant
sem_MConstant (Prelude.Just x) =
    (sem_MConstant_Just (sem_Constant x))
sem_MConstant Prelude.Nothing =
    sem_MConstant_Nothing
-- semantic domain
type T_MConstant = (Maybe SSort) ->
                   TypeEnv ->
                   (Maybe SSortExpr) ->
                   String ->
                   (Map.Map Id (Type, [PC])) ->
                   ( TypeEnv,MConstant,([SExpr]),SExpressions)
data Inh_MConstant = Inh_MConstant {mn_Inh_MConstant :: (Maybe SSort),mts_Inh_MConstant :: TypeEnv,sortexpr_Inh_MConstant :: (Maybe SSortExpr),tn_Inh_MConstant :: String,val_Inh_MConstant :: (Map.Map Id (Type, [PC]))}
data Syn_MConstant = Syn_MConstant {mts_Syn_MConstant :: TypeEnv,self_Syn_MConstant :: MConstant,sexpr_Syn_MConstant :: ([SExpr]),sexprs_Syn_MConstant :: SExpressions}
wrap_MConstant :: T_MConstant ->
                  Inh_MConstant ->
                  Syn_MConstant
wrap_MConstant sem (Inh_MConstant _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_MConstant _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs))
sem_MConstant_Just :: T_Constant ->
                      T_MConstant
sem_MConstant_Just just_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOself :: MConstant
              _lhsOmts :: TypeEnv
              _justOmn :: (Maybe SSort)
              _justOmts :: TypeEnv
              _justOsortexpr :: (Maybe SSortExpr)
              _justOtn :: String
              _justOval :: (Map.Map Id (Type, [PC]))
              _justIident :: (Maybe String)
              _justIisGlobal :: Bool
              _justImts :: TypeEnv
              _justIpsexpr :: (Int -> [SExpr])
              _justIself :: Constant
              _justIsexpr :: ([SExpr])
              _justIsexprs :: SExpressions
              _justIsort :: SSortExpr
              _justIvtype :: Type
              _lhsOsexpr =
                  ({-# LINE 23 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _justIsexpr
                   {-# LINE 14110 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 24 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _justIsexprs
                   {-# LINE 14115 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Just _justIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _justImts
                   {-# LINE 14124 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 14129 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 14134 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 14139 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 14144 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 14149 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _justIident,_justIisGlobal,_justImts,_justIpsexpr,_justIself,_justIsexpr,_justIsexprs,_justIsort,_justIvtype) =
                  just_ _justOmn _justOmts _justOsortexpr _justOtn _justOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs)))
sem_MConstant_Nothing :: T_MConstant
sem_MConstant_Nothing =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOself :: MConstant
              _lhsOmts :: TypeEnv
              _lhsOsexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 14168 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 14173 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Nothing
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 14182 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs)))
-- MDefinitionTy -----------------------------------------------
-- cata
sem_MDefinitionTy :: MDefinitionTy ->
                     T_MDefinitionTy
sem_MDefinitionTy (Prelude.Just x) =
    (sem_MDefinitionTy_Just (sem_DefinitionTy x))
sem_MDefinitionTy Prelude.Nothing =
    sem_MDefinitionTy_Nothing
-- semantic domain
type T_MDefinitionTy = ( MDefinitionTy)
data Inh_MDefinitionTy = Inh_MDefinitionTy {}
data Syn_MDefinitionTy = Syn_MDefinitionTy {self_Syn_MDefinitionTy :: MDefinitionTy}
wrap_MDefinitionTy :: T_MDefinitionTy ->
                      Inh_MDefinitionTy ->
                      Syn_MDefinitionTy
wrap_MDefinitionTy sem (Inh_MDefinitionTy) =
    (let ( _lhsOself) = sem
     in  (Syn_MDefinitionTy _lhsOself))
sem_MDefinitionTy_Just :: T_DefinitionTy ->
                          T_MDefinitionTy
sem_MDefinitionTy_Just just_ =
    (let _lhsOself :: MDefinitionTy
         _justIself :: DefinitionTy
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
     in  ( _lhsOself))
sem_MDefinitionTy_Nothing :: T_MDefinitionTy
sem_MDefinitionTy_Nothing =
    (let _lhsOself :: MDefinitionTy
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MGCName -----------------------------------------------------
-- cata
sem_MGCName :: MGCName ->
               T_MGCName
sem_MGCName (Prelude.Just x) =
    (sem_MGCName_Just (sem_GCName x))
sem_MGCName Prelude.Nothing =
    sem_MGCName_Nothing
-- semantic domain
type T_MGCName = ( MGCName)
data Inh_MGCName = Inh_MGCName {}
data Syn_MGCName = Syn_MGCName {self_Syn_MGCName :: MGCName}
wrap_MGCName :: T_MGCName ->
                Inh_MGCName ->
                Syn_MGCName
wrap_MGCName sem (Inh_MGCName) =
    (let ( _lhsOself) = sem
     in  (Syn_MGCName _lhsOself))
sem_MGCName_Just :: T_GCName ->
                    T_MGCName
sem_MGCName_Just just_ =
    (let _lhsOself :: MGCName
         _justIself :: GCName
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
     in  ( _lhsOself))
sem_MGCName_Nothing :: T_MGCName
sem_MGCName_Nothing =
    (let _lhsOself :: MGCName
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MId ---------------------------------------------------------
-- cata
sem_MId :: MId ->
           T_MId
sem_MId (Prelude.Just x) =
    (sem_MId_Just (sem_Id x))
sem_MId Prelude.Nothing =
    sem_MId_Nothing
-- semantic domain
type T_MId = ( MId)
data Inh_MId = Inh_MId {}
data Syn_MId = Syn_MId {self_Syn_MId :: MId}
wrap_MId :: T_MId ->
            Inh_MId ->
            Syn_MId
wrap_MId sem (Inh_MId) =
    (let ( _lhsOself) = sem
     in  (Syn_MId _lhsOself))
sem_MId_Just :: T_Id ->
                T_MId
sem_MId_Just just_ =
    (let _lhsOself :: MId
         _justIself :: Id
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
     in  ( _lhsOself))
sem_MId_Nothing :: T_MId
sem_MId_Nothing =
    (let _lhsOself :: MId
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MIdentifier -------------------------------------------------
-- cata
sem_MIdentifier :: MIdentifier ->
                   T_MIdentifier
sem_MIdentifier (Prelude.Just x) =
    (sem_MIdentifier_Just (sem_Identifier x))
sem_MIdentifier Prelude.Nothing =
    sem_MIdentifier_Nothing
-- semantic domain
type T_MIdentifier = (Maybe SSortExpr) ->
                     String ->
                     ( MIdentifier)
data Inh_MIdentifier = Inh_MIdentifier {sortexpr_Inh_MIdentifier :: (Maybe SSortExpr),tn_Inh_MIdentifier :: String}
data Syn_MIdentifier = Syn_MIdentifier {self_Syn_MIdentifier :: MIdentifier}
wrap_MIdentifier :: T_MIdentifier ->
                    Inh_MIdentifier ->
                    Syn_MIdentifier
wrap_MIdentifier sem (Inh_MIdentifier _lhsIsortexpr _lhsItn) =
    (let ( _lhsOself) = sem _lhsIsortexpr _lhsItn
     in  (Syn_MIdentifier _lhsOself))
sem_MIdentifier_Just :: T_Identifier ->
                        T_MIdentifier
sem_MIdentifier_Just just_ =
    (\ _lhsIsortexpr
       _lhsItn ->
         (let _lhsOself :: MIdentifier
              _justOsortexpr :: (Maybe SSortExpr)
              _justOtn :: String
              _justIdeclexpr :: ([SExpression])
              _justIident :: String
              _justIself :: Identifier
              _justIsexpr :: SExpr
              _justIssymbol :: SSymbol
              _self =
                  Just _justIself
              _lhsOself =
                  _self
              _justOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 14339 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 14344 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _justIdeclexpr,_justIident,_justIself,_justIsexpr,_justIssymbol) =
                  just_ _justOsortexpr _justOtn
          in  ( _lhsOself)))
sem_MIdentifier_Nothing :: T_MIdentifier
sem_MIdentifier_Nothing =
    (\ _lhsIsortexpr
       _lhsItn ->
         (let _lhsOself :: MIdentifier
              _self =
                  Nothing
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- MLinkageTy --------------------------------------------------
-- cata
sem_MLinkageTy :: MLinkageTy ->
                  T_MLinkageTy
sem_MLinkageTy (Prelude.Just x) =
    (sem_MLinkageTy_Just (sem_Linkage x))
sem_MLinkageTy Prelude.Nothing =
    sem_MLinkageTy_Nothing
-- semantic domain
type T_MLinkageTy = ( MLinkageTy)
data Inh_MLinkageTy = Inh_MLinkageTy {}
data Syn_MLinkageTy = Syn_MLinkageTy {self_Syn_MLinkageTy :: MLinkageTy}
wrap_MLinkageTy :: T_MLinkageTy ->
                   Inh_MLinkageTy ->
                   Syn_MLinkageTy
wrap_MLinkageTy sem (Inh_MLinkageTy) =
    (let ( _lhsOself) = sem
     in  (Syn_MLinkageTy _lhsOself))
sem_MLinkageTy_Just :: T_Linkage ->
                       T_MLinkageTy
sem_MLinkageTy_Just just_ =
    (let _lhsOself :: MLinkageTy
         _justIself :: Linkage
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
     in  ( _lhsOself))
sem_MLinkageTy_Nothing :: T_MLinkageTy
sem_MLinkageTy_Nothing =
    (let _lhsOself :: MLinkageTy
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MModuleAsms -------------------------------------------------
-- cata
sem_MModuleAsms :: MModuleAsms ->
                   T_MModuleAsms
sem_MModuleAsms (Prelude.Just x) =
    (sem_MModuleAsms_Just (sem_ModuleAsms x))
sem_MModuleAsms Prelude.Nothing =
    sem_MModuleAsms_Nothing
-- semantic domain
type T_MModuleAsms = ( MModuleAsms)
data Inh_MModuleAsms = Inh_MModuleAsms {}
data Syn_MModuleAsms = Syn_MModuleAsms {self_Syn_MModuleAsms :: MModuleAsms}
wrap_MModuleAsms :: T_MModuleAsms ->
                    Inh_MModuleAsms ->
                    Syn_MModuleAsms
wrap_MModuleAsms sem (Inh_MModuleAsms) =
    (let ( _lhsOself) = sem
     in  (Syn_MModuleAsms _lhsOself))
sem_MModuleAsms_Just :: T_ModuleAsms ->
                        T_MModuleAsms
sem_MModuleAsms_Just just_ =
    (let _lhsOself :: MModuleAsms
         _justIself :: ModuleAsms
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
     in  ( _lhsOself))
sem_MModuleAsms_Nothing :: T_MModuleAsms
sem_MModuleAsms_Nothing =
    (let _lhsOself :: MModuleAsms
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MSection ----------------------------------------------------
-- cata
sem_MSection :: MSection ->
                T_MSection
sem_MSection (Prelude.Just x) =
    (sem_MSection_Just (sem_Section x))
sem_MSection Prelude.Nothing =
    sem_MSection_Nothing
-- semantic domain
type T_MSection = ( MSection)
data Inh_MSection = Inh_MSection {}
data Syn_MSection = Syn_MSection {self_Syn_MSection :: MSection}
wrap_MSection :: T_MSection ->
                 Inh_MSection ->
                 Syn_MSection
wrap_MSection sem (Inh_MSection) =
    (let ( _lhsOself) = sem
     in  (Syn_MSection _lhsOself))
sem_MSection_Just :: T_Section ->
                     T_MSection
sem_MSection_Just just_ =
    (let _lhsOself :: MSection
         _justIself :: Section
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
     in  ( _lhsOself))
sem_MSection_Nothing :: T_MSection
sem_MSection_Nothing =
    (let _lhsOself :: MSection
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MUnnamedAddr ------------------------------------------------
-- cata
sem_MUnnamedAddr :: MUnnamedAddr ->
                    T_MUnnamedAddr
sem_MUnnamedAddr (Prelude.Just x) =
    (sem_MUnnamedAddr_Just x)
sem_MUnnamedAddr Prelude.Nothing =
    sem_MUnnamedAddr_Nothing
-- semantic domain
type T_MUnnamedAddr = ( MUnnamedAddr)
data Inh_MUnnamedAddr = Inh_MUnnamedAddr {}
data Syn_MUnnamedAddr = Syn_MUnnamedAddr {self_Syn_MUnnamedAddr :: MUnnamedAddr}
wrap_MUnnamedAddr :: T_MUnnamedAddr ->
                     Inh_MUnnamedAddr ->
                     Syn_MUnnamedAddr
wrap_MUnnamedAddr sem (Inh_MUnnamedAddr) =
    (let ( _lhsOself) = sem
     in  (Syn_MUnnamedAddr _lhsOself))
sem_MUnnamedAddr_Just :: Bool ->
                         T_MUnnamedAddr
sem_MUnnamedAddr_Just just_ =
    (let _lhsOself :: MUnnamedAddr
         _self =
             Just just_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_MUnnamedAddr_Nothing :: T_MUnnamedAddr
sem_MUnnamedAddr_Nothing =
    (let _lhsOself :: MUnnamedAddr
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MValue ------------------------------------------------------
-- cata
sem_MValue :: MValue ->
              T_MValue
sem_MValue (Prelude.Just x) =
    (sem_MValue_Just (sem_Value x))
sem_MValue Prelude.Nothing =
    sem_MValue_Nothing
-- semantic domain
type T_MValue = (Maybe SSort) ->
                TypeEnv ->
                (Maybe SSortExpr) ->
                String ->
                (Map.Map Id (Type, [PC])) ->
                ( MValue)
data Inh_MValue = Inh_MValue {mn_Inh_MValue :: (Maybe SSort),mts_Inh_MValue :: TypeEnv,sortexpr_Inh_MValue :: (Maybe SSortExpr),tn_Inh_MValue :: String,val_Inh_MValue :: (Map.Map Id (Type, [PC]))}
data Syn_MValue = Syn_MValue {self_Syn_MValue :: MValue}
wrap_MValue :: T_MValue ->
               Inh_MValue ->
               Syn_MValue
wrap_MValue sem (Inh_MValue _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_MValue _lhsOself))
sem_MValue_Just :: T_Value ->
                   T_MValue
sem_MValue_Just just_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: MValue
              _justOmn :: (Maybe SSort)
              _justOmts :: TypeEnv
              _justOsortexpr :: (Maybe SSortExpr)
              _justOtn :: String
              _justOval :: (Map.Map Id (Type, [PC]))
              _justIident :: (Maybe String)
              _justIisGlobal :: Bool
              _justImts :: TypeEnv
              _justIpsexpr :: (Int -> [SExpr])
              _justIself :: Value
              _justIsexpr :: ([SExpr])
              _justIsexprs :: SExpressions
              _justIsort :: SSortExpr
              _justIvtype :: Type
              _self =
                  Just _justIself
              _lhsOself =
                  _self
              _justOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 14561 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 14566 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 14571 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 14576 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 14581 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _justIident,_justIisGlobal,_justImts,_justIpsexpr,_justIself,_justIsexpr,_justIsexprs,_justIsort,_justIvtype) =
                  just_ _justOmn _justOmts _justOsortexpr _justOtn _justOval
          in  ( _lhsOself)))
sem_MValue_Nothing :: T_MValue
sem_MValue_Nothing =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: MValue
              _self =
                  Nothing
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- MVisibility -------------------------------------------------
-- cata
sem_MVisibility :: MVisibility ->
                   T_MVisibility
sem_MVisibility (Prelude.Just x) =
    (sem_MVisibility_Just (sem_Visibility x))
sem_MVisibility Prelude.Nothing =
    sem_MVisibility_Nothing
-- semantic domain
type T_MVisibility = ( MVisibility)
data Inh_MVisibility = Inh_MVisibility {}
data Syn_MVisibility = Syn_MVisibility {self_Syn_MVisibility :: MVisibility}
wrap_MVisibility :: T_MVisibility ->
                    Inh_MVisibility ->
                    Syn_MVisibility
wrap_MVisibility sem (Inh_MVisibility) =
    (let ( _lhsOself) = sem
     in  (Syn_MVisibility _lhsOself))
sem_MVisibility_Just :: T_Visibility ->
                        T_MVisibility
sem_MVisibility_Just just_ =
    (let _lhsOself :: MVisibility
         _justIself :: Visibility
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
     in  ( _lhsOself))
sem_MVisibility_Nothing :: T_MVisibility
sem_MVisibility_Nothing =
    (let _lhsOself :: MVisibility
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MapTyInt ----------------------------------------------------
-- cata
sem_MapTyInt :: MapTyInt ->
                T_MapTyInt
sem_MapTyInt m =
    (Data.Map.foldrWithKey sem_MapTyInt_Entry sem_MapTyInt_Nil (Data.Map.map sem_Triplet m))
-- semantic domain
type T_MapTyInt = ( MapTyInt)
data Inh_MapTyInt = Inh_MapTyInt {}
data Syn_MapTyInt = Syn_MapTyInt {self_Syn_MapTyInt :: MapTyInt}
wrap_MapTyInt :: T_MapTyInt ->
                 Inh_MapTyInt ->
                 Syn_MapTyInt
wrap_MapTyInt sem (Inh_MapTyInt) =
    (let ( _lhsOself) = sem
     in  (Syn_MapTyInt _lhsOself))
sem_MapTyInt_Entry :: Type ->
                      T_Triplet ->
                      T_MapTyInt ->
                      T_MapTyInt
sem_MapTyInt_Entry key_ val_ tl_ =
    (let _lhsOself :: MapTyInt
         _valIself :: Triplet
         _tlIself :: MapTyInt
         _self =
             Data.Map.insert key_ _valIself _tlIself
         _lhsOself =
             _self
         ( _valIself) =
             val_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_MapTyInt_Nil :: T_MapTyInt
sem_MapTyInt_Nil =
    (let _lhsOself :: MapTyInt
         _self =
             Data.Map.empty
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Module ------------------------------------------------------
-- cata
sem_Module :: Module ->
              T_Module
sem_Module (Module _id _layout _target _gvars _funs _nmdtys) =
    (sem_Module_Module _id (sem_DataLayout _layout) (sem_TargetData _target) (sem_Globals _gvars) (sem_Functions _funs) (sem_NamedTypes _nmdtys))
-- semantic domain
type T_Module = ( Module)
data Inh_Module = Inh_Module {}
data Syn_Module = Syn_Module {self_Syn_Module :: Module}
wrap_Module :: T_Module ->
               Inh_Module ->
               Syn_Module
wrap_Module sem (Inh_Module) =
    (let ( _lhsOself) = sem
     in  (Syn_Module _lhsOself))
sem_Module_Module :: String ->
                     T_DataLayout ->
                     T_TargetData ->
                     T_Globals ->
                     T_Functions ->
                     T_NamedTypes ->
                     T_Module
sem_Module_Module id_ layout_ target_ gvars_ funs_ nmdtys_ =
    (let _funsOcfg :: (Map.Map Identifier CF)
         _funsOcte :: (Map.Map Identifier PC)
         _funsOmutexes :: ([[(SExpr, Maybe SExpr)]])
         _gvarsOgs :: GlobalState
         _gvarsOsortexpr :: (Maybe SSortExpr)
         _gvarsOmn :: (Maybe SSort)
         _gvarsOtn :: String
         _gvarsOval :: (Map.Map Id (Type, [PC]))
         _funsOmn :: (Maybe SSort)
         _funsOmts :: TypeEnv
         _funsOval :: (Map.Map Id (Type, [PC]))
         _funsOprenc :: PreEncoder
         _funsOsortexpr :: (Maybe SSortExpr)
         _nmdtysOmn :: (Maybe SSort)
         _nmdtysOmts :: TypeEnv
         _nmdtysOval :: (Map.Map Id (Type, [PC]))
         _lhsOself :: Module
         _layoutIself :: DataLayout
         _targetIself :: TargetData
         _gvarsIgs :: GlobalState
         _gvarsIself :: Globals
         _gvarsIsexpr :: ([SExpr])
         _gvarsIsexprs :: SExpressions
         _funsIself :: Functions
         _funsIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
         _nmdtysIself :: NamedTypes
         _funsOcfg =
             ({-# LINE 46 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              Map.empty
              {-# LINE 14731 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _funsOcte =
             ({-# LINE 47 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              Map.empty
              {-# LINE 14736 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _funsOmutexes =
             ({-# LINE 48 "./src/Concurrent/Model/Encoder/Threads.ag" #-}
              []
              {-# LINE 14741 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _gvarsOgs =
             ({-# LINE 82 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              undefined
              {-# LINE 14746 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _gvarsOsortexpr =
             ({-# LINE 83 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              Nothing
              {-# LINE 14751 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _gvarsOmn =
             ({-# LINE 84 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              Nothing
              {-# LINE 14756 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _gvarsOtn =
             ({-# LINE 85 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              ""
              {-# LINE 14761 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _gvarsOval =
             ({-# LINE 86 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              Map.empty
              {-# LINE 14766 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _funsOmn =
             ({-# LINE 87 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              Nothing
              {-# LINE 14771 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _funsOmts =
             ({-# LINE 88 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              Map.empty
              {-# LINE 14776 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _funsOval =
             ({-# LINE 89 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              Map.empty
              {-# LINE 14781 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _funsOprenc =
             ({-# LINE 91 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              undefined
              {-# LINE 14786 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _funsOsortexpr =
             ({-# LINE 92 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              Nothing
              {-# LINE 14791 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _nmdtysOmn =
             ({-# LINE 93 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              Nothing
              {-# LINE 14796 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _nmdtysOmts =
             ({-# LINE 94 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              Map.empty
              {-# LINE 14801 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _nmdtysOval =
             ({-# LINE 95 "./src/Concurrent/Model/Encoder/Value.ag" #-}
              Map.empty
              {-# LINE 14806 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             Module id_ _layoutIself _targetIself _gvarsIself _funsIself _nmdtysIself
         _lhsOself =
             _self
         ( _layoutIself) =
             layout_
         ( _targetIself) =
             target_
         ( _gvarsIgs,_gvarsIself,_gvarsIsexpr,_gvarsIsexprs) =
             gvars_ _gvarsOgs _gvarsOmn _gvarsOsortexpr _gvarsOtn _gvarsOval
         ( _funsIself,_funsIts) =
             funs_ _funsOcfg _funsOcte _funsOmn _funsOmts _funsOmutexes _funsOprenc _funsOsortexpr _funsOval
         ( _nmdtysIself) =
             nmdtys_ _nmdtysOmn _nmdtysOmts _nmdtysOval
     in  ( _lhsOself))
-- ModuleAsm ---------------------------------------------------
-- cata
sem_ModuleAsm :: ModuleAsm ->
                 T_ModuleAsm
sem_ModuleAsm (ModuleAsm _asm) =
    (sem_ModuleAsm_ModuleAsm _asm)
-- semantic domain
type T_ModuleAsm = ( ModuleAsm)
data Inh_ModuleAsm = Inh_ModuleAsm {}
data Syn_ModuleAsm = Syn_ModuleAsm {self_Syn_ModuleAsm :: ModuleAsm}
wrap_ModuleAsm :: T_ModuleAsm ->
                  Inh_ModuleAsm ->
                  Syn_ModuleAsm
wrap_ModuleAsm sem (Inh_ModuleAsm) =
    (let ( _lhsOself) = sem
     in  (Syn_ModuleAsm _lhsOself))
sem_ModuleAsm_ModuleAsm :: String ->
                           T_ModuleAsm
sem_ModuleAsm_ModuleAsm asm_ =
    (let _lhsOself :: ModuleAsm
         _self =
             ModuleAsm asm_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- ModuleAsms --------------------------------------------------
-- cata
sem_ModuleAsms :: ModuleAsms ->
                  T_ModuleAsms
sem_ModuleAsms list =
    (Prelude.foldr sem_ModuleAsms_Cons sem_ModuleAsms_Nil (Prelude.map sem_ModuleAsm list))
-- semantic domain
type T_ModuleAsms = ( ModuleAsms)
data Inh_ModuleAsms = Inh_ModuleAsms {}
data Syn_ModuleAsms = Syn_ModuleAsms {self_Syn_ModuleAsms :: ModuleAsms}
wrap_ModuleAsms :: T_ModuleAsms ->
                   Inh_ModuleAsms ->
                   Syn_ModuleAsms
wrap_ModuleAsms sem (Inh_ModuleAsms) =
    (let ( _lhsOself) = sem
     in  (Syn_ModuleAsms _lhsOself))
sem_ModuleAsms_Cons :: T_ModuleAsm ->
                       T_ModuleAsms ->
                       T_ModuleAsms
sem_ModuleAsms_Cons hd_ tl_ =
    (let _lhsOself :: ModuleAsms
         _hdIself :: ModuleAsm
         _tlIself :: ModuleAsms
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_ModuleAsms_Nil :: T_ModuleAsms
sem_ModuleAsms_Nil =
    (let _lhsOself :: ModuleAsms
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- NamedTypes --------------------------------------------------
-- cata
sem_NamedTypes :: NamedTypes ->
                  T_NamedTypes
sem_NamedTypes m =
    (Data.Map.foldrWithKey sem_NamedTypes_Entry sem_NamedTypes_Nil (Data.Map.map sem_Type m))
-- semantic domain
type T_NamedTypes = (Maybe SSort) ->
                    TypeEnv ->
                    (Map.Map Id (Type, [PC])) ->
                    ( NamedTypes)
data Inh_NamedTypes = Inh_NamedTypes {mn_Inh_NamedTypes :: (Maybe SSort),mts_Inh_NamedTypes :: TypeEnv,val_Inh_NamedTypes :: (Map.Map Id (Type, [PC]))}
data Syn_NamedTypes = Syn_NamedTypes {self_Syn_NamedTypes :: NamedTypes}
wrap_NamedTypes :: T_NamedTypes ->
                   Inh_NamedTypes ->
                   Syn_NamedTypes
wrap_NamedTypes sem (Inh_NamedTypes _lhsImn _lhsImts _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIval
     in  (Syn_NamedTypes _lhsOself))
sem_NamedTypes_Entry :: Id ->
                        T_Type ->
                        T_NamedTypes ->
                        T_NamedTypes
sem_NamedTypes_Entry key_ val_ tl_ =
    (\ _lhsImn
       _lhsImts
       _lhsIval ->
         (let _lhsOself :: NamedTypes
              _valOmn :: (Maybe SSort)
              _valOmts :: TypeEnv
              _tlOmn :: (Maybe SSort)
              _tlOmts :: TypeEnv
              _tlOval :: (Map.Map Id (Type, [PC]))
              _valImts :: TypeEnv
              _valIself :: Type
              _valIsexprs :: SExpressions
              _valIsort :: SSortExpr
              _valIsortn :: SSort
              _tlIself :: NamedTypes
              _self =
                  Data.Map.insert key_ _valIself _tlIself
              _lhsOself =
                  _self
              _valOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 14934 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 14939 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 14944 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _valImts
                   {-# LINE 14949 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 14954 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _valImts,_valIself,_valIsexprs,_valIsort,_valIsortn) =
                  val_ _valOmn _valOmts
              ( _tlIself) =
                  tl_ _tlOmn _tlOmts _tlOval
          in  ( _lhsOself)))
sem_NamedTypes_Nil :: T_NamedTypes
sem_NamedTypes_Nil =
    (\ _lhsImn
       _lhsImts
       _lhsIval ->
         (let _lhsOself :: NamedTypes
              _self =
                  Data.Map.empty
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- PC ----------------------------------------------------------
-- cata
sem_PC :: PC ->
          T_PC
sem_PC ( x1) =
    (sem_PC_Tuple x1)
-- semantic domain
type T_PC = ( PC)
data Inh_PC = Inh_PC {}
data Syn_PC = Syn_PC {self_Syn_PC :: PC}
wrap_PC :: T_PC ->
           Inh_PC ->
           Syn_PC
wrap_PC sem (Inh_PC) =
    (let ( _lhsOself) = sem
     in  (Syn_PC _lhsOself))
sem_PC_Tuple :: Int ->
                T_PC
sem_PC_Tuple x1_ =
    (let _lhsOself :: PC
         _self =
             (x1_)
         _lhsOself =
             _self
     in  ( _lhsOself))
-- PTyInt ------------------------------------------------------
-- cata
sem_PTyInt :: PTyInt ->
              T_PTyInt
sem_PTyInt ( x1,x2) =
    (sem_PTyInt_Tuple (sem_Type x1) x2)
-- semantic domain
type T_PTyInt = (Maybe SSort) ->
                TypeEnv ->
                (Map.Map Id (Type, [PC])) ->
                ( PTyInt)
data Inh_PTyInt = Inh_PTyInt {mn_Inh_PTyInt :: (Maybe SSort),mts_Inh_PTyInt :: TypeEnv,val_Inh_PTyInt :: (Map.Map Id (Type, [PC]))}
data Syn_PTyInt = Syn_PTyInt {self_Syn_PTyInt :: PTyInt}
wrap_PTyInt :: T_PTyInt ->
               Inh_PTyInt ->
               Syn_PTyInt
wrap_PTyInt sem (Inh_PTyInt _lhsImn _lhsImts _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIval
     in  (Syn_PTyInt _lhsOself))
sem_PTyInt_Tuple :: T_Type ->
                    Int ->
                    T_PTyInt
sem_PTyInt_Tuple x1_ x2_ =
    (\ _lhsImn
       _lhsImts
       _lhsIval ->
         (let _lhsOself :: PTyInt
              _x1Omn :: (Maybe SSort)
              _x1Omts :: TypeEnv
              _x1Imts :: TypeEnv
              _x1Iself :: Type
              _x1Isexprs :: SExpressions
              _x1Isort :: SSortExpr
              _x1Isortn :: SSort
              _self =
                  (_x1Iself,x2_)
              _lhsOself =
                  _self
              _x1Omn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 15038 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Omts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 15043 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _x1Imts,_x1Iself,_x1Isexprs,_x1Isort,_x1Isortn) =
                  x1_ _x1Omn _x1Omts
          in  ( _lhsOself)))
-- PTyIntL -----------------------------------------------------
-- cata
sem_PTyIntL :: PTyIntL ->
               T_PTyIntL
sem_PTyIntL list =
    (Prelude.foldr sem_PTyIntL_Cons sem_PTyIntL_Nil (Prelude.map sem_PTyInt list))
-- semantic domain
type T_PTyIntL = (Maybe SSort) ->
                 TypeEnv ->
                 (Map.Map Id (Type, [PC])) ->
                 ( PTyIntL)
data Inh_PTyIntL = Inh_PTyIntL {mn_Inh_PTyIntL :: (Maybe SSort),mts_Inh_PTyIntL :: TypeEnv,val_Inh_PTyIntL :: (Map.Map Id (Type, [PC]))}
data Syn_PTyIntL = Syn_PTyIntL {self_Syn_PTyIntL :: PTyIntL}
wrap_PTyIntL :: T_PTyIntL ->
                Inh_PTyIntL ->
                Syn_PTyIntL
wrap_PTyIntL sem (Inh_PTyIntL _lhsImn _lhsImts _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIval
     in  (Syn_PTyIntL _lhsOself))
sem_PTyIntL_Cons :: T_PTyInt ->
                    T_PTyIntL ->
                    T_PTyIntL
sem_PTyIntL_Cons hd_ tl_ =
    (\ _lhsImn
       _lhsImts
       _lhsIval ->
         (let _lhsOself :: PTyIntL
              _hdOmn :: (Maybe SSort)
              _hdOmts :: TypeEnv
              _hdOval :: (Map.Map Id (Type, [PC]))
              _tlOmn :: (Maybe SSort)
              _tlOmts :: TypeEnv
              _tlOval :: (Map.Map Id (Type, [PC]))
              _hdIself :: PTyInt
              _tlIself :: PTyIntL
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 15090 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 15095 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 15100 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 15105 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 15110 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 15115 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIself) =
                  hd_ _hdOmn _hdOmts _hdOval
              ( _tlIself) =
                  tl_ _tlOmn _tlOmts _tlOval
          in  ( _lhsOself)))
sem_PTyIntL_Nil :: T_PTyIntL
sem_PTyIntL_Nil =
    (\ _lhsImn
       _lhsImts
       _lhsIval ->
         (let _lhsOself :: PTyIntL
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- PValue ------------------------------------------------------
-- cata
sem_PValue :: PValue ->
              T_PValue
sem_PValue ( x1,x2) =
    (sem_PValue_Tuple (sem_Value x1) (sem_Value x2))
-- semantic domain
type T_PValue = (Maybe SSort) ->
                TypeEnv ->
                (Maybe SSortExpr) ->
                String ->
                (Map.Map Id (Type, [PC])) ->
                ( PValue)
data Inh_PValue = Inh_PValue {mn_Inh_PValue :: (Maybe SSort),mts_Inh_PValue :: TypeEnv,sortexpr_Inh_PValue :: (Maybe SSortExpr),tn_Inh_PValue :: String,val_Inh_PValue :: (Map.Map Id (Type, [PC]))}
data Syn_PValue = Syn_PValue {self_Syn_PValue :: PValue}
wrap_PValue :: T_PValue ->
               Inh_PValue ->
               Syn_PValue
wrap_PValue sem (Inh_PValue _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_PValue _lhsOself))
sem_PValue_Tuple :: T_Value ->
                    T_Value ->
                    T_PValue
sem_PValue_Tuple x1_ x2_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: PValue
              _x1Omn :: (Maybe SSort)
              _x1Omts :: TypeEnv
              _x1Osortexpr :: (Maybe SSortExpr)
              _x1Otn :: String
              _x1Oval :: (Map.Map Id (Type, [PC]))
              _x2Omn :: (Maybe SSort)
              _x2Omts :: TypeEnv
              _x2Osortexpr :: (Maybe SSortExpr)
              _x2Otn :: String
              _x2Oval :: (Map.Map Id (Type, [PC]))
              _x1Iident :: (Maybe String)
              _x1IisGlobal :: Bool
              _x1Imts :: TypeEnv
              _x1Ipsexpr :: (Int -> [SExpr])
              _x1Iself :: Value
              _x1Isexpr :: ([SExpr])
              _x1Isexprs :: SExpressions
              _x1Isort :: SSortExpr
              _x1Ivtype :: Type
              _x2Iident :: (Maybe String)
              _x2IisGlobal :: Bool
              _x2Imts :: TypeEnv
              _x2Ipsexpr :: (Int -> [SExpr])
              _x2Iself :: Value
              _x2Isexpr :: ([SExpr])
              _x2Isexprs :: SExpressions
              _x2Isort :: SSortExpr
              _x2Ivtype :: Type
              _self =
                  (_x1Iself,_x2Iself)
              _lhsOself =
                  _self
              _x1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 15199 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 15204 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 15209 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 15214 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 15219 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x2Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 15224 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x2Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _x1Imts
                   {-# LINE 15229 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x2Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 15234 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x2Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 15239 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x2Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 15244 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _x1Iident,_x1IisGlobal,_x1Imts,_x1Ipsexpr,_x1Iself,_x1Isexpr,_x1Isexprs,_x1Isort,_x1Ivtype) =
                  x1_ _x1Omn _x1Omts _x1Osortexpr _x1Otn _x1Oval
              ( _x2Iident,_x2IisGlobal,_x2Imts,_x2Ipsexpr,_x2Iself,_x2Isexpr,_x2Isexprs,_x2Isort,_x2Ivtype) =
                  x2_ _x2Omn _x2Omts _x2Osortexpr _x2Otn _x2Oval
          in  ( _lhsOself)))
-- PValueIdx ---------------------------------------------------
-- cata
sem_PValueIdx :: PValueIdx ->
                 T_PValueIdx
sem_PValueIdx ( x1,x2) =
    (sem_PValueIdx_Tuple (sem_Value x1) x2)
-- semantic domain
type T_PValueIdx = (Maybe SSort) ->
                   TypeEnv ->
                   (Maybe SSortExpr) ->
                   String ->
                   (Map.Map Id (Type, [PC])) ->
                   ( PValueIdx)
data Inh_PValueIdx = Inh_PValueIdx {mn_Inh_PValueIdx :: (Maybe SSort),mts_Inh_PValueIdx :: TypeEnv,sortexpr_Inh_PValueIdx :: (Maybe SSortExpr),tn_Inh_PValueIdx :: String,val_Inh_PValueIdx :: (Map.Map Id (Type, [PC]))}
data Syn_PValueIdx = Syn_PValueIdx {self_Syn_PValueIdx :: PValueIdx}
wrap_PValueIdx :: T_PValueIdx ->
                  Inh_PValueIdx ->
                  Syn_PValueIdx
wrap_PValueIdx sem (Inh_PValueIdx _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_PValueIdx _lhsOself))
sem_PValueIdx_Tuple :: T_Value ->
                       Int ->
                       T_PValueIdx
sem_PValueIdx_Tuple x1_ x2_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: PValueIdx
              _x1Omn :: (Maybe SSort)
              _x1Omts :: TypeEnv
              _x1Osortexpr :: (Maybe SSortExpr)
              _x1Otn :: String
              _x1Oval :: (Map.Map Id (Type, [PC]))
              _x1Iident :: (Maybe String)
              _x1IisGlobal :: Bool
              _x1Imts :: TypeEnv
              _x1Ipsexpr :: (Int -> [SExpr])
              _x1Iself :: Value
              _x1Isexpr :: ([SExpr])
              _x1Isexprs :: SExpressions
              _x1Isort :: SSortExpr
              _x1Ivtype :: Type
              _self =
                  (_x1Iself,x2_)
              _lhsOself =
                  _self
              _x1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 15303 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 15308 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 15313 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 15318 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 15323 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _x1Iident,_x1IisGlobal,_x1Imts,_x1Ipsexpr,_x1Iself,_x1Isexpr,_x1Isexprs,_x1Isort,_x1Ivtype) =
                  x1_ _x1Omn _x1Omts _x1Osortexpr _x1Otn _x1Oval
          in  ( _lhsOself)))
-- PValues -----------------------------------------------------
-- cata
sem_PValues :: PValues ->
               T_PValues
sem_PValues list =
    (Prelude.foldr sem_PValues_Cons sem_PValues_Nil (Prelude.map sem_PValue list))
-- semantic domain
type T_PValues = (Maybe SSort) ->
                 TypeEnv ->
                 (Maybe SSortExpr) ->
                 String ->
                 (Map.Map Id (Type, [PC])) ->
                 ( PValues)
data Inh_PValues = Inh_PValues {mn_Inh_PValues :: (Maybe SSort),mts_Inh_PValues :: TypeEnv,sortexpr_Inh_PValues :: (Maybe SSortExpr),tn_Inh_PValues :: String,val_Inh_PValues :: (Map.Map Id (Type, [PC]))}
data Syn_PValues = Syn_PValues {self_Syn_PValues :: PValues}
wrap_PValues :: T_PValues ->
                Inh_PValues ->
                Syn_PValues
wrap_PValues sem (Inh_PValues _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_PValues _lhsOself))
sem_PValues_Cons :: T_PValue ->
                    T_PValues ->
                    T_PValues
sem_PValues_Cons hd_ tl_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: PValues
              _hdOmn :: (Maybe SSort)
              _hdOmts :: TypeEnv
              _hdOsortexpr :: (Maybe SSortExpr)
              _hdOtn :: String
              _hdOval :: (Map.Map Id (Type, [PC]))
              _tlOmn :: (Maybe SSort)
              _tlOmts :: TypeEnv
              _tlOsortexpr :: (Maybe SSortExpr)
              _tlOtn :: String
              _tlOval :: (Map.Map Id (Type, [PC]))
              _hdIself :: PValue
              _tlIself :: PValues
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 15378 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 15383 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 15388 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 15393 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 15398 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 15403 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 15408 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 15413 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 15418 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 15423 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIself) =
                  hd_ _hdOmn _hdOmts _hdOsortexpr _hdOtn _hdOval
              ( _tlIself) =
                  tl_ _tlOmn _tlOmts _tlOsortexpr _tlOtn _tlOval
          in  ( _lhsOself)))
sem_PValues_Nil :: T_PValues
sem_PValues_Nil =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: PValues
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- Parameter ---------------------------------------------------
-- cata
sem_Parameter :: Parameter ->
                 T_Parameter
sem_Parameter (Parameter _var _ty) =
    (sem_Parameter_Parameter (sem_Identifier _var) (sem_Type _ty))
-- semantic domain
type T_Parameter = TypeEnv ->
                   (Maybe SSortExpr) ->
                   String ->
                   ( String,TypeEnv,Parameter,SExpr,SExpressions)
data Inh_Parameter = Inh_Parameter {mts_Inh_Parameter :: TypeEnv,sortexpr_Inh_Parameter :: (Maybe SSortExpr),tn_Inh_Parameter :: String}
data Syn_Parameter = Syn_Parameter {ident_Syn_Parameter :: String,mts_Syn_Parameter :: TypeEnv,self_Syn_Parameter :: Parameter,sexpr_Syn_Parameter :: SExpr,sexprs_Syn_Parameter :: SExpressions}
wrap_Parameter :: T_Parameter ->
                  Inh_Parameter ->
                  Syn_Parameter
wrap_Parameter sem (Inh_Parameter _lhsImts _lhsIsortexpr _lhsItn) =
    (let ( _lhsOident,_lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs) = sem _lhsImts _lhsIsortexpr _lhsItn
     in  (Syn_Parameter _lhsOident _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs))
sem_Parameter_Parameter :: T_Identifier ->
                           T_Type ->
                           T_Parameter
sem_Parameter_Parameter var_ ty_ =
    (\ _lhsImts
       _lhsIsortexpr
       _lhsItn ->
         (let _lhsOsexpr :: SExpr
              _lhsOident :: String
              _tyOmts :: TypeEnv
              _tyOmn :: (Maybe SSort)
              _lhsOsexprs :: SExpressions
              _lhsOmts :: TypeEnv
              _lhsOself :: Parameter
              _varOsortexpr :: (Maybe SSortExpr)
              _varOtn :: String
              _varIdeclexpr :: ([SExpression])
              _varIident :: String
              _varIself :: Identifier
              _varIsexpr :: SExpr
              _varIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOsexpr =
                  ({-# LINE 292 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   IdentExpr $ SymIdent _sym
                   {-# LINE 15491 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sym =
                  ({-# LINE 293 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   SimpleSym $ _lhsItn ++ (getIdName _varIself)
                   {-# LINE 15496 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 294 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn ++ (getIdName _varIself)
                   {-# LINE 15501 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 295 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 15506 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 296 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 15511 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 297 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   [ declfun _sym     _tyIsort ] ++ _tyIsexprs
                   {-# LINE 15516 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 298 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 15521 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Parameter _varIself _tyIself
              _lhsOself =
                  _self
              _varOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 15530 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _varOtn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 15535 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _varIdeclexpr,_varIident,_varIself,_varIsexpr,_varIssymbol) =
                  var_ _varOsortexpr _varOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOident,_lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs)))
-- Parameters --------------------------------------------------
-- cata
sem_Parameters :: Parameters ->
                  T_Parameters
sem_Parameters list =
    (Prelude.foldr sem_Parameters_Cons sem_Parameters_Nil (Prelude.map sem_Parameter list))
-- semantic domain
type T_Parameters = TypeEnv ->
                    (Maybe SSortExpr) ->
                    String ->
                    (Map.Map Id (Type, [PC])) ->
                    ( Parameters)
data Inh_Parameters = Inh_Parameters {mts_Inh_Parameters :: TypeEnv,sortexpr_Inh_Parameters :: (Maybe SSortExpr),tn_Inh_Parameters :: String,val_Inh_Parameters :: (Map.Map Id (Type, [PC]))}
data Syn_Parameters = Syn_Parameters {self_Syn_Parameters :: Parameters}
wrap_Parameters :: T_Parameters ->
                   Inh_Parameters ->
                   Syn_Parameters
wrap_Parameters sem (Inh_Parameters _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOself) = sem _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_Parameters _lhsOself))
sem_Parameters_Cons :: T_Parameter ->
                       T_Parameters ->
                       T_Parameters
sem_Parameters_Cons hd_ tl_ =
    (\ _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: Parameters
              _hdOmts :: TypeEnv
              _hdOsortexpr :: (Maybe SSortExpr)
              _hdOtn :: String
              _tlOmts :: TypeEnv
              _tlOsortexpr :: (Maybe SSortExpr)
              _tlOtn :: String
              _tlOval :: (Map.Map Id (Type, [PC]))
              _hdIident :: String
              _hdImts :: TypeEnv
              _hdIself :: Parameter
              _hdIsexpr :: SExpr
              _hdIsexprs :: SExpressions
              _tlIself :: Parameters
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOmts =
                  ({-# LINE 284 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 15591 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 15596 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 287 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 15601 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 72 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _hdImts
                   {-# LINE 15606 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 15611 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 73 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 15616 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 15621 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIident,_hdImts,_hdIself,_hdIsexpr,_hdIsexprs) =
                  hd_ _hdOmts _hdOsortexpr _hdOtn
              ( _tlIself) =
                  tl_ _tlOmts _tlOsortexpr _tlOtn _tlOval
          in  ( _lhsOself)))
sem_Parameters_Nil :: T_Parameters
sem_Parameters_Nil =
    (\ _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: Parameters
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- RealPredicate -----------------------------------------------
-- cata
sem_RealPredicate :: RealPredicate ->
                     T_RealPredicate
sem_RealPredicate (LLVMRealPredicateFalse) =
    (sem_RealPredicate_LLVMRealPredicateFalse)
sem_RealPredicate (LLVMRealOEQ) =
    (sem_RealPredicate_LLVMRealOEQ)
sem_RealPredicate (LLVMRealOGT) =
    (sem_RealPredicate_LLVMRealOGT)
sem_RealPredicate (LLVMRealOGE) =
    (sem_RealPredicate_LLVMRealOGE)
sem_RealPredicate (LLVMRealOLT) =
    (sem_RealPredicate_LLVMRealOLT)
sem_RealPredicate (LLVMRealOLE) =
    (sem_RealPredicate_LLVMRealOLE)
sem_RealPredicate (LLVMRealONE) =
    (sem_RealPredicate_LLVMRealONE)
sem_RealPredicate (LLVMRealORD) =
    (sem_RealPredicate_LLVMRealORD)
sem_RealPredicate (LLVMRealUNO) =
    (sem_RealPredicate_LLVMRealUNO)
sem_RealPredicate (LLVMRealUEQ) =
    (sem_RealPredicate_LLVMRealUEQ)
sem_RealPredicate (LLVMRealUGT) =
    (sem_RealPredicate_LLVMRealUGT)
sem_RealPredicate (LLVMRealUGE) =
    (sem_RealPredicate_LLVMRealUGE)
sem_RealPredicate (LLVMRealULT) =
    (sem_RealPredicate_LLVMRealULT)
sem_RealPredicate (LLVMRealULE) =
    (sem_RealPredicate_LLVMRealULE)
sem_RealPredicate (LLVMRealUNE) =
    (sem_RealPredicate_LLVMRealUNE)
sem_RealPredicate (LLVMRealPredicateTrue) =
    (sem_RealPredicate_LLVMRealPredicateTrue)
-- semantic domain
type T_RealPredicate = ( RealPredicate)
data Inh_RealPredicate = Inh_RealPredicate {}
data Syn_RealPredicate = Syn_RealPredicate {self_Syn_RealPredicate :: RealPredicate}
wrap_RealPredicate :: T_RealPredicate ->
                      Inh_RealPredicate ->
                      Syn_RealPredicate
wrap_RealPredicate sem (Inh_RealPredicate) =
    (let ( _lhsOself) = sem
     in  (Syn_RealPredicate _lhsOself))
sem_RealPredicate_LLVMRealPredicateFalse :: T_RealPredicate
sem_RealPredicate_LLVMRealPredicateFalse =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealPredicateFalse
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealOEQ :: T_RealPredicate
sem_RealPredicate_LLVMRealOEQ =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealOEQ
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealOGT :: T_RealPredicate
sem_RealPredicate_LLVMRealOGT =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealOGT
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealOGE :: T_RealPredicate
sem_RealPredicate_LLVMRealOGE =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealOGE
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealOLT :: T_RealPredicate
sem_RealPredicate_LLVMRealOLT =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealOLT
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealOLE :: T_RealPredicate
sem_RealPredicate_LLVMRealOLE =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealOLE
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealONE :: T_RealPredicate
sem_RealPredicate_LLVMRealONE =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealONE
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealORD :: T_RealPredicate
sem_RealPredicate_LLVMRealORD =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealORD
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealUNO :: T_RealPredicate
sem_RealPredicate_LLVMRealUNO =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealUNO
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealUEQ :: T_RealPredicate
sem_RealPredicate_LLVMRealUEQ =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealUEQ
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealUGT :: T_RealPredicate
sem_RealPredicate_LLVMRealUGT =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealUGT
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealUGE :: T_RealPredicate
sem_RealPredicate_LLVMRealUGE =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealUGE
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealULT :: T_RealPredicate
sem_RealPredicate_LLVMRealULT =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealULT
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealULE :: T_RealPredicate
sem_RealPredicate_LLVMRealULE =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealULE
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealUNE :: T_RealPredicate
sem_RealPredicate_LLVMRealUNE =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealUNE
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_RealPredicate_LLVMRealPredicateTrue :: T_RealPredicate
sem_RealPredicate_LLVMRealPredicateTrue =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealPredicateTrue
         _lhsOself =
             _self
     in  ( _lhsOself))
-- RetInst -----------------------------------------------------
-- cata
sem_RetInst :: RetInst ->
               T_RetInst
sem_RetInst (ValueRet _v) =
    (sem_RetInst_ValueRet (sem_Value _v))
sem_RetInst (VoidRet) =
    (sem_RetInst_VoidRet)
-- semantic domain
type T_RetInst = (Maybe SSort) ->
                 TypeEnv ->
                 (Maybe SSortExpr) ->
                 String ->
                 (Map.Map Id (Type, [PC])) ->
                 ( RetInst)
data Inh_RetInst = Inh_RetInst {mn_Inh_RetInst :: (Maybe SSort),mts_Inh_RetInst :: TypeEnv,sortexpr_Inh_RetInst :: (Maybe SSortExpr),tn_Inh_RetInst :: String,val_Inh_RetInst :: (Map.Map Id (Type, [PC]))}
data Syn_RetInst = Syn_RetInst {self_Syn_RetInst :: RetInst}
wrap_RetInst :: T_RetInst ->
                Inh_RetInst ->
                Syn_RetInst
wrap_RetInst sem (Inh_RetInst _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_RetInst _lhsOself))
sem_RetInst_ValueRet :: T_Value ->
                        T_RetInst
sem_RetInst_ValueRet v_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: RetInst
              _vOmn :: (Maybe SSort)
              _vOmts :: TypeEnv
              _vOsortexpr :: (Maybe SSortExpr)
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _vIident :: (Maybe String)
              _vIisGlobal :: Bool
              _vImts :: TypeEnv
              _vIpsexpr :: (Int -> [SExpr])
              _vIself :: Value
              _vIsexpr :: ([SExpr])
              _vIsexprs :: SExpressions
              _vIsort :: SSortExpr
              _vIvtype :: Type
              _self =
                  ValueRet _vIself
              _lhsOself =
                  _self
              _vOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 15867 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 15872 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 15877 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 15882 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 15887 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmn _vOmts _vOsortexpr _vOtn _vOval
          in  ( _lhsOself)))
sem_RetInst_VoidRet :: T_RetInst
sem_RetInst_VoidRet =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: RetInst
              _self =
                  VoidRet
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- Section -----------------------------------------------------
-- cata
sem_Section :: Section ->
               T_Section
sem_Section (Section _s) =
    (sem_Section_Section _s)
-- semantic domain
type T_Section = ( Section)
data Inh_Section = Inh_Section {}
data Syn_Section = Syn_Section {self_Syn_Section :: Section}
wrap_Section :: T_Section ->
                Inh_Section ->
                Syn_Section
wrap_Section sem (Inh_Section) =
    (let ( _lhsOself) = sem
     in  (Syn_Section _lhsOself))
sem_Section_Section :: String ->
                       T_Section
sem_Section_Section s_ =
    (let _lhsOself :: Section
         _self =
             Section s_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- SimpleConstant ----------------------------------------------
-- cata
sem_SimpleConstant :: SimpleConstant ->
                      T_SimpleConstant
sem_SimpleConstant (ConstantInt _iv _ty) =
    (sem_SimpleConstant_ConstantInt _iv (sem_Type _ty))
sem_SimpleConstant (ConstantFP _fp) =
    (sem_SimpleConstant_ConstantFP (sem_ConstantFP _fp))
sem_SimpleConstant (ConstantPointerNull _ty) =
    (sem_SimpleConstant_ConstantPointerNull (sem_Type _ty))
-- semantic domain
type T_SimpleConstant = TypeEnv ->
                        String ->
                        (Map.Map Id (Type, [PC])) ->
                        ( TypeEnv,SimpleConstant,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_SimpleConstant = Inh_SimpleConstant {mts_Inh_SimpleConstant :: TypeEnv,tn_Inh_SimpleConstant :: String,val_Inh_SimpleConstant :: (Map.Map Id (Type, [PC]))}
data Syn_SimpleConstant = Syn_SimpleConstant {mts_Syn_SimpleConstant :: TypeEnv,self_Syn_SimpleConstant :: SimpleConstant,sexpr_Syn_SimpleConstant :: ([SExpr]),sexprs_Syn_SimpleConstant :: SExpressions,sort_Syn_SimpleConstant :: SSortExpr,vtype_Syn_SimpleConstant :: Type}
wrap_SimpleConstant :: T_SimpleConstant ->
                       Inh_SimpleConstant ->
                       Syn_SimpleConstant
wrap_SimpleConstant sem (Inh_SimpleConstant _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_SimpleConstant _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_SimpleConstant_ConstantInt :: Int ->
                                  T_Type ->
                                  T_SimpleConstant
sem_SimpleConstant_ConstantInt iv_ ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: SimpleConstant
              _lhsOmts :: TypeEnv
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _tyOmn =
                  ({-# LINE 167 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 15976 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 168 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 15981 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 169 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   [ IdentExpr $ IdxIdent (bv iv_) [getISize _tyIself] ]
                   {-# LINE 15986 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 170 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 15991 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 171 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 15996 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 172 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 16001 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantInt iv_ _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 16010 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_SimpleConstant_ConstantFP :: T_ConstantFP ->
                                 T_SimpleConstant
sem_SimpleConstant_ConstantFP fp_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: SimpleConstant
              _lhsOmts :: TypeEnv
              _fpOmts :: TypeEnv
              _fpOtn :: String
              _fpOval :: (Map.Map Id (Type, [PC]))
              _fpImts :: TypeEnv
              _fpIself :: ConstantFP
              _fpIsexpr :: ([SExpr])
              _fpIsexprs :: SExpressions
              _fpIsort :: SSortExpr
              _fpIvtype :: Type
              _lhsOsexpr =
                  ({-# LINE 161 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _fpIsexpr
                   {-# LINE 16039 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 162 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _fpIsexprs
                   {-# LINE 16044 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 163 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _fpIvtype
                   {-# LINE 16049 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 164 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _fpIsort
                   {-# LINE 16054 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantFP _fpIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _fpImts
                   {-# LINE 16063 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fpOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 16068 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fpOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 16073 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fpOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 16078 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _fpImts,_fpIself,_fpIsexpr,_fpIsexprs,_fpIsort,_fpIvtype) =
                  fp_ _fpOmts _fpOtn _fpOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_SimpleConstant_ConstantPointerNull :: T_Type ->
                                          T_SimpleConstant
sem_SimpleConstant_ConstantPointerNull ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _lhsOvtype :: Type
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOsort :: SSortExpr
              _lhsOself :: SimpleConstant
              _lhsOmts :: TypeEnv
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _tyOmn =
                  ({-# LINE 174 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 16105 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 175 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 16110 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 176 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 16115 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 177 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 16120 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 178 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 16125 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 179 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 16130 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantPointerNull _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 16139 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- Target ------------------------------------------------------
-- cata
sem_Target :: Target ->
              T_Target
sem_Target (MacOs) =
    (sem_Target_MacOs)
sem_Target (Linux) =
    (sem_Target_Linux)
-- semantic domain
type T_Target = ( Target)
data Inh_Target = Inh_Target {}
data Syn_Target = Syn_Target {self_Syn_Target :: Target}
wrap_Target :: T_Target ->
               Inh_Target ->
               Syn_Target
wrap_Target sem (Inh_Target) =
    (let ( _lhsOself) = sem
     in  (Syn_Target _lhsOself))
sem_Target_MacOs :: T_Target
sem_Target_MacOs =
    (let _lhsOself :: Target
         _self =
             MacOs
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Target_Linux :: T_Target
sem_Target_Linux =
    (let _lhsOself :: Target
         _self =
             Linux
         _lhsOself =
             _self
     in  ( _lhsOself))
-- TargetData --------------------------------------------------
-- cata
sem_TargetData :: TargetData ->
                  T_TargetData
sem_TargetData (TargetData _s _t) =
    (sem_TargetData_TargetData _s (sem_Target _t))
-- semantic domain
type T_TargetData = ( TargetData)
data Inh_TargetData = Inh_TargetData {}
data Syn_TargetData = Syn_TargetData {self_Syn_TargetData :: TargetData}
wrap_TargetData :: T_TargetData ->
                   Inh_TargetData ->
                   Syn_TargetData
wrap_TargetData sem (Inh_TargetData) =
    (let ( _lhsOself) = sem
     in  (Syn_TargetData _lhsOself))
sem_TargetData_TargetData :: String ->
                             T_Target ->
                             T_TargetData
sem_TargetData_TargetData s_ t_ =
    (let _lhsOself :: TargetData
         _tIself :: Target
         _self =
             TargetData s_ _tIself
         _lhsOself =
             _self
         ( _tIself) =
             t_
     in  ( _lhsOself))
-- Triplet -----------------------------------------------------
-- cata
sem_Triplet :: Triplet ->
               T_Triplet
sem_Triplet ( x1,x2,x3) =
    (sem_Triplet_Tuple x1 x2 x3)
-- semantic domain
type T_Triplet = ( Triplet)
data Inh_Triplet = Inh_Triplet {}
data Syn_Triplet = Syn_Triplet {self_Syn_Triplet :: Triplet}
wrap_Triplet :: T_Triplet ->
                Inh_Triplet ->
                Syn_Triplet
wrap_Triplet sem (Inh_Triplet) =
    (let ( _lhsOself) = sem
     in  (Syn_Triplet _lhsOself))
sem_Triplet_Tuple :: Int ->
                     Int ->
                     Int ->
                     T_Triplet
sem_Triplet_Tuple x1_ x2_ x3_ =
    (let _lhsOself :: Triplet
         _self =
             (x1_,x2_,x3_)
         _lhsOself =
             _self
     in  ( _lhsOself))
-- TyFloatPoint ------------------------------------------------
-- cata
sem_TyFloatPoint :: TyFloatPoint ->
                    T_TyFloatPoint
sem_TyFloatPoint (TyHalf) =
    (sem_TyFloatPoint_TyHalf)
sem_TyFloatPoint (TyFloat) =
    (sem_TyFloatPoint_TyFloat)
sem_TyFloatPoint (TyDouble) =
    (sem_TyFloatPoint_TyDouble)
sem_TyFloatPoint (TyFP128) =
    (sem_TyFloatPoint_TyFP128)
sem_TyFloatPoint (Tyx86FP80) =
    (sem_TyFloatPoint_Tyx86FP80)
sem_TyFloatPoint (TyPPCFP128) =
    (sem_TyFloatPoint_TyPPCFP128)
-- semantic domain
type T_TyFloatPoint = ( TyFloatPoint)
data Inh_TyFloatPoint = Inh_TyFloatPoint {}
data Syn_TyFloatPoint = Syn_TyFloatPoint {self_Syn_TyFloatPoint :: TyFloatPoint}
wrap_TyFloatPoint :: T_TyFloatPoint ->
                     Inh_TyFloatPoint ->
                     Syn_TyFloatPoint
wrap_TyFloatPoint sem (Inh_TyFloatPoint) =
    (let ( _lhsOself) = sem
     in  (Syn_TyFloatPoint _lhsOself))
sem_TyFloatPoint_TyHalf :: T_TyFloatPoint
sem_TyFloatPoint_TyHalf =
    (let _lhsOself :: TyFloatPoint
         _self =
             TyHalf
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_TyFloatPoint_TyFloat :: T_TyFloatPoint
sem_TyFloatPoint_TyFloat =
    (let _lhsOself :: TyFloatPoint
         _self =
             TyFloat
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_TyFloatPoint_TyDouble :: T_TyFloatPoint
sem_TyFloatPoint_TyDouble =
    (let _lhsOself :: TyFloatPoint
         _self =
             TyDouble
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_TyFloatPoint_TyFP128 :: T_TyFloatPoint
sem_TyFloatPoint_TyFP128 =
    (let _lhsOself :: TyFloatPoint
         _self =
             TyFP128
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_TyFloatPoint_Tyx86FP80 :: T_TyFloatPoint
sem_TyFloatPoint_Tyx86FP80 =
    (let _lhsOself :: TyFloatPoint
         _self =
             Tyx86FP80
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_TyFloatPoint_TyPPCFP128 :: T_TyFloatPoint
sem_TyFloatPoint_TyPPCFP128 =
    (let _lhsOself :: TyFloatPoint
         _self =
             TyPPCFP128
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Type --------------------------------------------------------
-- cata
sem_Type :: Type ->
            T_Type
sem_Type (TyVoid) =
    (sem_Type_TyVoid)
sem_Type (Tyx86MMX) =
    (sem_Type_Tyx86MMX)
sem_Type (TyLabel) =
    (sem_Type_TyLabel)
sem_Type (TyMetadata) =
    (sem_Type_TyMetadata)
sem_Type (TyOpaque) =
    (sem_Type_TyOpaque)
sem_Type (TyInt _p) =
    (sem_Type_TyInt _p)
sem_Type (TyFloatPoint _p) =
    (sem_Type_TyFloatPoint (sem_TyFloatPoint _p))
sem_Type (TyArray _numEl _ty) =
    (sem_Type_TyArray _numEl (sem_Type _ty))
sem_Type (TyFunction _party _retty _isVar) =
    (sem_Type_TyFunction (sem_Types _party) (sem_Type _retty) _isVar)
sem_Type (TyStruct _name _numEl _tys) =
    (sem_Type_TyStruct _name _numEl (sem_Types _tys))
sem_Type (TyPointer _ty) =
    (sem_Type_TyPointer (sem_Type _ty))
sem_Type (TyVector _numEl _ty) =
    (sem_Type_TyVector _numEl (sem_Type _ty))
sem_Type (TyUndefined) =
    (sem_Type_TyUndefined)
sem_Type (TyJumpTo _lb) =
    (sem_Type_TyJumpTo (sem_Identifiers _lb))
-- semantic domain
type T_Type = (Maybe SSort) ->
              TypeEnv ->
              ( TypeEnv,Type,SExpressions,SSortExpr,SSort)
data Inh_Type = Inh_Type {mn_Inh_Type :: (Maybe SSort),mts_Inh_Type :: TypeEnv}
data Syn_Type = Syn_Type {mts_Syn_Type :: TypeEnv,self_Syn_Type :: Type,sexprs_Syn_Type :: SExpressions,sort_Syn_Type :: SSortExpr,sortn_Syn_Type :: SSort}
wrap_Type :: T_Type ->
             Inh_Type ->
             Syn_Type
wrap_Type sem (Inh_Type _lhsImn _lhsImts) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn) = sem _lhsImn _lhsImts
     in  (Syn_Type _lhsOmts _lhsOself _lhsOsexprs _lhsOsort _lhsOsortn))
sem_Type_TyVoid :: T_Type
sem_Type_TyVoid =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsort :: SSortExpr
              _lhsOmts :: TypeEnv
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOsort =
                  ({-# LINE 25 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16364 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 26 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16369 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 27 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16374 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 16379 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyVoid
              _lhsOself =
                  _self
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_Tyx86MMX :: T_Type
sem_Type_Tyx86MMX =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsort :: SSortExpr
              _lhsOmts :: TypeEnv
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOsort =
                  ({-# LINE 29 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16398 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 30 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16403 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 31 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16408 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 16413 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Tyx86MMX
              _lhsOself =
                  _self
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyLabel :: T_Type
sem_Type_TyLabel =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsort :: SSortExpr
              _lhsOmts :: TypeEnv
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOsort =
                  ({-# LINE 33 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16432 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 34 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16437 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 35 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16442 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 16447 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyLabel
              _lhsOself =
                  _self
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyMetadata :: T_Type
sem_Type_TyMetadata =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsort :: SSortExpr
              _lhsOmts :: TypeEnv
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOsort =
                  ({-# LINE 37 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16466 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 38 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16471 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 39 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16476 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 16481 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyMetadata
              _lhsOself =
                  _self
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyOpaque :: T_Type
sem_Type_TyOpaque =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsort :: SSortExpr
              _lhsOmts :: TypeEnv
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOsort =
                  ({-# LINE 41 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16500 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 42 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16505 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 43 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16510 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 16515 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyOpaque
              _lhsOself =
                  _self
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyInt :: Int ->
                  T_Type
sem_Type_TyInt p_ =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsortn :: SSort
              _lhsOsort :: SSortExpr
              _lhsOmts :: TypeEnv
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _sortn =
                  ({-# LINE 45 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   if p_ == 1
                   then fromMaybe "Bool"           _lhsImn
                   else fromMaybe ("I" ++ show p_) _lhsImn
                   {-# LINE 16537 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sortn
                   {-# LINE 16542 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sort =
                  ({-# LINE 49 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   SymSort _sortn
                   {-# LINE 16547 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sort
                   {-# LINE 16552 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 51 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   Map.insert _self (_sort    , _sortn    ) _lhsImts
                   {-# LINE 16557 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 52 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   case Map.lookup _self _lhsImts of
                        Nothing -> if p_ == 1
                                   then []
                                   else [ defsorti p_ ]
                        Just _  -> []
                   {-# LINE 16566 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyInt p_
              _lhsOself =
                  _self
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyFloatPoint :: T_TyFloatPoint ->
                         T_Type
sem_Type_TyFloatPoint p_ =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsort :: SSortExpr
              _lhsOmts :: TypeEnv
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _pIself :: TyFloatPoint
              _lhsOsort =
                  ({-# LINE 58 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16587 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 59 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16592 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 60 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16597 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 16602 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyFloatPoint _pIself
              _lhsOself =
                  _self
              ( _pIself) =
                  p_
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyArray :: Int ->
                    T_Type ->
                    T_Type
sem_Type_TyArray numEl_ ty_ =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsortn :: SSort
              _tyOmts :: TypeEnv
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _tyOmn :: (Maybe SSort)
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _bsize =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   getBSize numEl_
                   {-# LINE 16632 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sortn =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   fromMaybe ("Array" ++ show numEl_ ++ _tyIsortn) _lhsImn
                   {-# LINE 16637 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 64 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sortn
                   {-# LINE 16642 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 65 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 16647 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 66 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   Map.insert _self (_sort    , _sortn    ) _tyImts
                   {-# LINE 16652 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sort =
                  ({-# LINE 67 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   SymSort _sortn
                   {-# LINE 16657 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 68 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sort
                   {-# LINE 16662 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sortd =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   ArraySort (BitVector _bsize    ) _tyIsort
                   {-# LINE 16667 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 70 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _tyIsexprs ++ [ SE $ DefSort (SimpleSym _sortn    ) [] $ toSExpr _sortd     ]
                   {-# LINE 16672 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyArray numEl_ _tyIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 16681 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyFunction :: T_Types ->
                       T_Type ->
                       Bool ->
                       T_Type
sem_Type_TyFunction party_ retty_ isVar_ =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsort :: SSortExpr
              _lhsOmts :: TypeEnv
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _rettyOmn :: (Maybe SSort)
              _rettyOmts :: TypeEnv
              _partyIself :: Types
              _rettyImts :: TypeEnv
              _rettyIself :: Type
              _rettyIsexprs :: SExpressions
              _rettyIsort :: SSortExpr
              _rettyIsortn :: SSort
              _lhsOsort =
                  ({-# LINE 72 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16709 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 73 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16714 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 74 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16719 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _rettyIsexprs
                   {-# LINE 16724 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyFunction _partyIself _rettyIself isVar_
              _lhsOself =
                  _self
              _rettyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 16733 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rettyOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 16738 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _partyIself) =
                  party_
              ( _rettyImts,_rettyIself,_rettyIsexprs,_rettyIsort,_rettyIsortn) =
                  retty_ _rettyOmn _rettyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyStruct :: String ->
                     Int ->
                     T_Types ->
                     T_Type
sem_Type_TyStruct name_ numEl_ tys_ =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsortn :: SSort
              _lhsOsort :: SSortExpr
              _lhsOmts :: TypeEnv
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _tysIself :: Types
              _sortn =
                  ({-# LINE 76 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   fromMaybe name_ _lhsImn
                   {-# LINE 16761 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 77 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sortn
                   {-# LINE 16766 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tysi =
                  ({-# LINE 78 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   case _sortn     of
                        "union.pthread_mutex_t"          -> (_lhsImts, [], SymSort "Bool")
                        "struct.__pthread_mutex_s"       -> (_lhsImts, [], SymSort "Bool")
                        "struct.__pthread_internal_list" -> (_lhsImts, [], SymSort "Bool")
                        n -> encTypes _tysIself _lhsImts
                   {-# LINE 16775 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sort =
                  ({-# LINE 83 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   case _sortn     of
                        "" -> trdu _tysi
                        n  -> SymSort _sortn
                   {-# LINE 16782 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 86 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sort
                   {-# LINE 16787 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 87 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   case _sortn     of
                        "" -> fstu _tysi
                        n  -> Map.insert _self (_sort    , n) $ fstu _tysi
                   {-# LINE 16794 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 90 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   case _sortn     of
                        "" -> sndu _tysi
                        n  -> sndu _tysi     ++ [ SE $ DefSort (SimpleSym _sortn    ) [] $ toSExpr $ trdu _tysi     ]
                   {-# LINE 16801 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyStruct name_ numEl_ _tysIself
              _lhsOself =
                  _self
              ( _tysIself) =
                  tys_
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyPointer :: T_Type ->
                      T_Type
sem_Type_TyPointer ty_ =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsortn :: SSort
              _lhsOsort :: SSortExpr
              _tyOmts :: TypeEnv
              _lhsOmts :: TypeEnv
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _tyOmn :: (Maybe SSort)
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _sortn =
                  ({-# LINE 94 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   fromMaybe _tyIsortn _lhsImn
                   {-# LINE 16830 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 95 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sortn
                   {-# LINE 16835 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sort =
                  ({-# LINE 96 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   SymSort _sortn
                   {-# LINE 16840 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 97 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sort
                   {-# LINE 16845 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 98 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 16850 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 99 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   Map.insert _self (_sort    , _sortn    ) _tyImts
                   {-# LINE 16855 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sortd =
                  ({-# LINE 100 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _tyIsort
                   {-# LINE 16860 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 101 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _tyIsexprs
                   {-# LINE 16865 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyPointer _tyIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 16874 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyVector :: Int ->
                     T_Type ->
                     T_Type
sem_Type_TyVector numEl_ ty_ =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsortn :: SSort
              _tyOmts :: TypeEnv
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _tyOmn :: (Maybe SSort)
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _bsize =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   getBSize numEl_
                   {-# LINE 16900 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sortn =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   fromMaybe ("Array" ++ show numEl_ ++ _tyIsortn) _lhsImn
                   {-# LINE 16905 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 64 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sortn
                   {-# LINE 16910 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 65 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 16915 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 66 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   Map.insert _self (_sort    , _sortn    ) _tyImts
                   {-# LINE 16920 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sort =
                  ({-# LINE 67 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   SymSort _sortn
                   {-# LINE 16925 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 68 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sort
                   {-# LINE 16930 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sortd =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   ArraySort (BitVector _bsize    ) _tyIsort
                   {-# LINE 16935 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 70 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _tyIsexprs ++ [ SE $ DefSort (SimpleSym _sortn    ) [] $ toSExpr _sortd     ]
                   {-# LINE 16940 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyVector numEl_ _tyIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 16949 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyUndefined :: T_Type
sem_Type_TyUndefined =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsort :: SSortExpr
              _lhsOmts :: TypeEnv
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOsort =
                  ({-# LINE 103 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16966 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 104 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16971 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 105 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 16976 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 16981 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyUndefined
              _lhsOself =
                  _self
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyJumpTo :: T_Identifiers ->
                     T_Type
sem_Type_TyJumpTo lb_ =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOsortn :: SSort
              _lbOsortexpr :: (Maybe SSortExpr)
              _lbOtn :: String
              _lbIself :: Identifiers
              _lhsOsexprs =
                  ({-# LINE 18 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 17004 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyJumpTo _lbIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 19 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 17013 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Type.TyJumpTo.lhs.sort"
                   {-# LINE 17018 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Type.TyJumpTo.lhs.sortn"
                   {-# LINE 17023 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lbOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Type.TyJumpTo.lb.sortexpr"
                   {-# LINE 17028 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lbOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Type.TyJumpTo.lb.tn"
                   {-# LINE 17033 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _lbIself) =
                  lb_ _lbOsortexpr _lbOtn
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
-- Types -------------------------------------------------------
-- cata
sem_Types :: Types ->
             T_Types
sem_Types list =
    (Prelude.foldr sem_Types_Cons sem_Types_Nil (Prelude.map sem_Type list))
-- semantic domain
type T_Types = ( Types)
data Inh_Types = Inh_Types {}
data Syn_Types = Syn_Types {self_Syn_Types :: Types}
wrap_Types :: T_Types ->
              Inh_Types ->
              Syn_Types
wrap_Types sem (Inh_Types) =
    (let ( _lhsOself) = sem
     in  (Syn_Types _lhsOself))
sem_Types_Cons :: T_Type ->
                  T_Types ->
                  T_Types
sem_Types_Cons hd_ tl_ =
    (let _hdOmn :: (Maybe SSort)
         _hdOmts :: TypeEnv
         _lhsOself :: Types
         _hdImts :: TypeEnv
         _hdIself :: Type
         _hdIsexprs :: SExpressions
         _hdIsort :: SSortExpr
         _hdIsortn :: SSort
         _tlIself :: Types
         _hdOmn =
             ({-# LINE 12 "./src/Concurrent/Model/Encoder/Types.ag" #-}
              Nothing
              {-# LINE 17070 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _hdOmts =
             ({-# LINE 13 "./src/Concurrent/Model/Encoder/Types.ag" #-}
              Map.empty
              {-# LINE 17075 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdImts,_hdIself,_hdIsexprs,_hdIsort,_hdIsortn) =
             hd_ _hdOmn _hdOmts
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Types_Nil :: T_Types
sem_Types_Nil =
    (let _lhsOself :: Types
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- ValId -------------------------------------------------------
-- cata
sem_ValId :: ValId ->
             T_ValId
sem_ValId ( x1,x2) =
    (sem_ValId_Tuple (sem_Value x1) (sem_Identifier x2))
-- semantic domain
type T_ValId = (Maybe SSort) ->
               TypeEnv ->
               (Maybe SSortExpr) ->
               String ->
               (Map.Map Id (Type, [PC])) ->
               ( ValId)
data Inh_ValId = Inh_ValId {mn_Inh_ValId :: (Maybe SSort),mts_Inh_ValId :: TypeEnv,sortexpr_Inh_ValId :: (Maybe SSortExpr),tn_Inh_ValId :: String,val_Inh_ValId :: (Map.Map Id (Type, [PC]))}
data Syn_ValId = Syn_ValId {self_Syn_ValId :: ValId}
wrap_ValId :: T_ValId ->
              Inh_ValId ->
              Syn_ValId
wrap_ValId sem (Inh_ValId _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_ValId _lhsOself))
sem_ValId_Tuple :: T_Value ->
                   T_Identifier ->
                   T_ValId
sem_ValId_Tuple x1_ x2_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: ValId
              _x1Omn :: (Maybe SSort)
              _x1Omts :: TypeEnv
              _x1Osortexpr :: (Maybe SSortExpr)
              _x1Otn :: String
              _x1Oval :: (Map.Map Id (Type, [PC]))
              _x2Osortexpr :: (Maybe SSortExpr)
              _x2Otn :: String
              _x1Iident :: (Maybe String)
              _x1IisGlobal :: Bool
              _x1Imts :: TypeEnv
              _x1Ipsexpr :: (Int -> [SExpr])
              _x1Iself :: Value
              _x1Isexpr :: ([SExpr])
              _x1Isexprs :: SExpressions
              _x1Isort :: SSortExpr
              _x1Ivtype :: Type
              _x2Ideclexpr :: ([SExpression])
              _x2Iident :: String
              _x2Iself :: Identifier
              _x2Isexpr :: SExpr
              _x2Issymbol :: SSymbol
              _self =
                  (_x1Iself,_x2Iself)
              _lhsOself =
                  _self
              _x1Omn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 17153 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Omts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 17158 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Osortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 17163 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Otn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 17168 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x1Oval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 17173 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x2Osortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 17178 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _x2Otn =
                  ({-# LINE 12 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 17183 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _x1Iident,_x1IisGlobal,_x1Imts,_x1Ipsexpr,_x1Iself,_x1Isexpr,_x1Isexprs,_x1Isort,_x1Ivtype) =
                  x1_ _x1Omn _x1Omts _x1Osortexpr _x1Otn _x1Oval
              ( _x2Ideclexpr,_x2Iident,_x2Iself,_x2Isexpr,_x2Issymbol) =
                  x2_ _x2Osortexpr _x2Otn
          in  ( _lhsOself)))
-- ValIdL ------------------------------------------------------
-- cata
sem_ValIdL :: ValIdL ->
              T_ValIdL
sem_ValIdL list =
    (Prelude.foldr sem_ValIdL_Cons sem_ValIdL_Nil (Prelude.map sem_ValId list))
-- semantic domain
type T_ValIdL = (Maybe SSort) ->
                TypeEnv ->
                (Maybe SSortExpr) ->
                String ->
                (Map.Map Id (Type, [PC])) ->
                ( ValIdL)
data Inh_ValIdL = Inh_ValIdL {mn_Inh_ValIdL :: (Maybe SSort),mts_Inh_ValIdL :: TypeEnv,sortexpr_Inh_ValIdL :: (Maybe SSortExpr),tn_Inh_ValIdL :: String,val_Inh_ValIdL :: (Map.Map Id (Type, [PC]))}
data Syn_ValIdL = Syn_ValIdL {self_Syn_ValIdL :: ValIdL}
wrap_ValIdL :: T_ValIdL ->
               Inh_ValIdL ->
               Syn_ValIdL
wrap_ValIdL sem (Inh_ValIdL _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_ValIdL _lhsOself))
sem_ValIdL_Cons :: T_ValId ->
                   T_ValIdL ->
                   T_ValIdL
sem_ValIdL_Cons hd_ tl_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: ValIdL
              _hdOmn :: (Maybe SSort)
              _hdOmts :: TypeEnv
              _hdOsortexpr :: (Maybe SSortExpr)
              _hdOtn :: String
              _hdOval :: (Map.Map Id (Type, [PC]))
              _tlOmn :: (Maybe SSort)
              _tlOmts :: TypeEnv
              _tlOsortexpr :: (Maybe SSortExpr)
              _tlOtn :: String
              _tlOval :: (Map.Map Id (Type, [PC]))
              _hdIself :: ValId
              _tlIself :: ValIdL
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 17240 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 17245 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 17250 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 17255 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 17260 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 17265 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 17270 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 17275 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 17280 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 17285 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIself) =
                  hd_ _hdOmn _hdOmts _hdOsortexpr _hdOtn _hdOval
              ( _tlIself) =
                  tl_ _tlOmn _tlOmts _tlOsortexpr _tlOtn _tlOval
          in  ( _lhsOself)))
sem_ValIdL_Nil :: T_ValIdL
sem_ValIdL_Nil =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: ValIdL
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- Value -------------------------------------------------------
-- cata
sem_Value :: Value ->
             T_Value
sem_Value (Id _v _ty) =
    (sem_Value_Id (sem_Identifier _v) (sem_Type _ty))
sem_Value (Constant _c) =
    (sem_Value_Constant (sem_Constant _c))
-- semantic domain
type T_Value = (Maybe SSort) ->
               TypeEnv ->
               (Maybe SSortExpr) ->
               String ->
               (Map.Map Id (Type, [PC])) ->
               ( (Maybe String),Bool,TypeEnv,(Int -> [SExpr]),Value,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_Value = Inh_Value {mn_Inh_Value :: (Maybe SSort),mts_Inh_Value :: TypeEnv,sortexpr_Inh_Value :: (Maybe SSortExpr),tn_Inh_Value :: String,val_Inh_Value :: (Map.Map Id (Type, [PC]))}
data Syn_Value = Syn_Value {ident_Syn_Value :: (Maybe String),isGlobal_Syn_Value :: Bool,mts_Syn_Value :: TypeEnv,psexpr_Syn_Value :: (Int -> [SExpr]),self_Syn_Value :: Value,sexpr_Syn_Value :: ([SExpr]),sexprs_Syn_Value :: SExpressions,sort_Syn_Value :: SSortExpr,vtype_Syn_Value :: Type}
wrap_Value :: T_Value ->
              Inh_Value ->
              Syn_Value
wrap_Value sem (Inh_Value _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_Value _lhsOident _lhsOisGlobal _lhsOmts _lhsOpsexpr _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_Value_Id :: T_Identifier ->
                T_Type ->
                T_Value
sem_Value_Id v_ ty_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _tyOmts :: TypeEnv
              _tyOmn :: (Maybe SSort)
              _vOtn :: String
              _lhsOsexpr :: ([SExpr])
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOmts :: TypeEnv
              _lhsOvtype :: Type
              _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOsort :: SSortExpr
              _lhsOself :: Value
              _vOsortexpr :: (Maybe SSortExpr)
              _vIdeclexpr :: ([SExpression])
              _vIident :: String
              _vIself :: Identifier
              _vIsexpr :: SExpr
              _vIssymbol :: SSymbol
              _tyImts :: TypeEnv
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _tyOmts =
                  ({-# LINE 105 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 17363 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 106 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 17368 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 107 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 17373 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 108 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   case Map.lookup _vIident _lhsIval of
                        Nothing -> [ _vIsexpr ]
                        Just (t,lp)  -> Prelude.map (\p -> IdentExpr $ SymIdent $ SimpleSym $ _lhsItn ++ _vIident ++ show p) [0.. length lp]
                   {-# LINE 17380 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 111 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> case Map.lookup _vIident _lhsIval of
                              Nothing -> [ ]
                              Just (t,lp)  -> Prelude.map (\p -> IdentExpr $ SymIdent $ SimpleSym $ "p" ++ _lhsItn ++ _vIident ++ show p) [0.. length lp]
                   {-# LINE 17387 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 114 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsexprs
                   {-# LINE 17392 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 115 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 17397 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 116 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 17402 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOisGlobal =
                  ({-# LINE 117 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 17407 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 118 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   Just _vIident
                   {-# LINE 17412 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 119 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 17417 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Id _vIself _tyIself
              _lhsOself =
                  _self
              _vOsortexpr =
                  ({-# LINE 17 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 17426 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _vIdeclexpr,_vIident,_vIself,_vIsexpr,_vIssymbol) =
                  v_ _vOsortexpr _vOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Value_Constant :: T_Constant ->
                      T_Value
sem_Value_Constant c_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOsort :: SSortExpr
              _lhsOself :: Value
              _lhsOmts :: TypeEnv
              _cOmn :: (Maybe SSort)
              _cOmts :: TypeEnv
              _cOsortexpr :: (Maybe SSortExpr)
              _cOtn :: String
              _cOval :: (Map.Map Id (Type, [PC]))
              _cIident :: (Maybe String)
              _cIisGlobal :: Bool
              _cImts :: TypeEnv
              _cIpsexpr :: (Int -> [SExpr])
              _cIself :: Constant
              _cIsexpr :: ([SExpr])
              _cIsexprs :: SExpressions
              _cIsort :: SSortExpr
              _cIvtype :: Type
              _lhsOsexpr =
                  ({-# LINE 122 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIsexpr
                   {-# LINE 17467 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 123 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIpsexpr
                   {-# LINE 17472 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 124 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIsexprs
                   {-# LINE 17477 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 125 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIvtype
                   {-# LINE 17482 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOisGlobal =
                  ({-# LINE 126 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIisGlobal
                   {-# LINE 17487 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 127 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIident
                   {-# LINE 17492 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 128 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIsort
                   {-# LINE 17497 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Constant _cIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cImts
                   {-# LINE 17506 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 17511 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cOmts =
                  ({-# LINE 44 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 17516 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 17521 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 17526 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 17531 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _cIident,_cIisGlobal,_cImts,_cIpsexpr,_cIself,_cIsexpr,_cIsexprs,_cIsort,_cIvtype) =
                  c_ _cOmn _cOmts _cOsortexpr _cOtn _cOval
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- ValueIdxs ---------------------------------------------------
-- cata
sem_ValueIdxs :: ValueIdxs ->
                 T_ValueIdxs
sem_ValueIdxs list =
    (Prelude.foldr sem_ValueIdxs_Cons sem_ValueIdxs_Nil (Prelude.map sem_PValueIdx list))
-- semantic domain
type T_ValueIdxs = (Maybe SSort) ->
                   TypeEnv ->
                   (Maybe SSortExpr) ->
                   String ->
                   (Map.Map Id (Type, [PC])) ->
                   ( ValueIdxs)
data Inh_ValueIdxs = Inh_ValueIdxs {mn_Inh_ValueIdxs :: (Maybe SSort),mts_Inh_ValueIdxs :: TypeEnv,sortexpr_Inh_ValueIdxs :: (Maybe SSortExpr),tn_Inh_ValueIdxs :: String,val_Inh_ValueIdxs :: (Map.Map Id (Type, [PC]))}
data Syn_ValueIdxs = Syn_ValueIdxs {self_Syn_ValueIdxs :: ValueIdxs}
wrap_ValueIdxs :: T_ValueIdxs ->
                  Inh_ValueIdxs ->
                  Syn_ValueIdxs
wrap_ValueIdxs sem (Inh_ValueIdxs _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOself) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_ValueIdxs _lhsOself))
sem_ValueIdxs_Cons :: T_PValueIdx ->
                      T_ValueIdxs ->
                      T_ValueIdxs
sem_ValueIdxs_Cons hd_ tl_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: ValueIdxs
              _hdOmn :: (Maybe SSort)
              _hdOmts :: TypeEnv
              _hdOsortexpr :: (Maybe SSortExpr)
              _hdOtn :: String
              _hdOval :: (Map.Map Id (Type, [PC]))
              _tlOmn :: (Maybe SSort)
              _tlOmts :: TypeEnv
              _tlOsortexpr :: (Maybe SSortExpr)
              _tlOtn :: String
              _tlOval :: (Map.Map Id (Type, [PC]))
              _hdIself :: PValueIdx
              _tlIself :: ValueIdxs
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 17586 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 17591 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 17596 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 17601 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 17606 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmn =
                  ({-# LINE 61 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 17611 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 62 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 17616 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 17621 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 69 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 17626 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 63 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 17631 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIself) =
                  hd_ _hdOmn _hdOmts _hdOsortexpr _hdOtn _hdOval
              ( _tlIself) =
                  tl_ _tlOmn _tlOmts _tlOsortexpr _tlOtn _tlOval
          in  ( _lhsOself)))
sem_ValueIdxs_Nil :: T_ValueIdxs
sem_ValueIdxs_Nil =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOself :: ValueIdxs
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOself)))
-- Values ------------------------------------------------------
-- cata
sem_Values :: Values ->
              T_Values
sem_Values list =
    (Prelude.foldr sem_Values_Cons sem_Values_Nil (Prelude.map sem_Value list))
-- semantic domain
type T_Values = (Maybe SSort) ->
                TypeEnv ->
                (Maybe SSortExpr) ->
                String ->
                (Map.Map Id (Type, [PC])) ->
                ( TypeEnv,Values,([SExpr]),SExpressions,([Type]))
data Inh_Values = Inh_Values {mn_Inh_Values :: (Maybe SSort),mts_Inh_Values :: TypeEnv,sortexpr_Inh_Values :: (Maybe SSortExpr),tn_Inh_Values :: String,val_Inh_Values :: (Map.Map Id (Type, [PC]))}
data Syn_Values = Syn_Values {mts_Syn_Values :: TypeEnv,self_Syn_Values :: Values,sexpr_Syn_Values :: ([SExpr]),sexprs_Syn_Values :: SExpressions,vtype_Syn_Values :: ([Type])}
wrap_Values :: T_Values ->
               Inh_Values ->
               Syn_Values
wrap_Values sem (Inh_Values _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype) = sem _lhsImn _lhsImts _lhsIsortexpr _lhsItn _lhsIval
     in  (Syn_Values _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOvtype))
sem_Values_Cons :: T_Value ->
                   T_Values ->
                   T_Values
sem_Values_Cons hd_ tl_ =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _hdOmts :: TypeEnv
              _tlOmts :: TypeEnv
              _lhsOmts :: TypeEnv
              _lhsOvtype :: ([Type])
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOself :: Values
              _hdOmn :: (Maybe SSort)
              _hdOsortexpr :: (Maybe SSortExpr)
              _hdOtn :: String
              _hdOval :: (Map.Map Id (Type, [PC]))
              _tlOmn :: (Maybe SSort)
              _tlOsortexpr :: (Maybe SSortExpr)
              _tlOtn :: String
              _tlOval :: (Map.Map Id (Type, [PC]))
              _hdIident :: (Maybe String)
              _hdIisGlobal :: Bool
              _hdImts :: TypeEnv
              _hdIpsexpr :: (Int -> [SExpr])
              _hdIself :: Value
              _hdIsexpr :: ([SExpr])
              _hdIsexprs :: SExpressions
              _hdIsort :: SSortExpr
              _hdIvtype :: Type
              _tlImts :: TypeEnv
              _tlIself :: Values
              _tlIsexpr :: ([SExpr])
              _tlIsexprs :: SExpressions
              _tlIvtype :: ([Type])
              _hdOmts =
                  ({-# LINE 38 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 17713 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 39 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _hdImts
                   {-# LINE 17718 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 40 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tlImts
                   {-# LINE 17723 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 41 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _hdIvtype:(_tlIvtype)
                   {-# LINE 17728 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 31 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _hdIsexpr ++ _tlIsexpr
                   {-# LINE 17733 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 30 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _hdIsexprs ++ _tlIsexprs
                   {-# LINE 17738 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 17747 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 17752 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 47 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 17757 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOval =
                  ({-# LINE 48 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 17762 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmn =
                  ({-# LINE 54 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImn
                   {-# LINE 17767 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOsortexpr =
                  ({-# LINE 20 "./src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsIsortexpr
                   {-# LINE 17772 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 29 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 17777 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 28 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 17782 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIident,_hdIisGlobal,_hdImts,_hdIpsexpr,_hdIself,_hdIsexpr,_hdIsexprs,_hdIsort,_hdIvtype) =
                  hd_ _hdOmn _hdOmts _hdOsortexpr _hdOtn _hdOval
              ( _tlImts,_tlIself,_tlIsexpr,_tlIsexprs,_tlIvtype) =
                  tl_ _tlOmn _tlOmts _tlOsortexpr _tlOtn _tlOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Values_Nil :: T_Values
sem_Values_Nil =
    (\ _lhsImn
       _lhsImts
       _lhsIsortexpr
       _lhsItn
       _lhsIval ->
         (let _lhsOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: ([Type])
              _lhsOself :: Values
              _lhsOmts =
                  ({-# LINE 36 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 17804 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 31 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 17809 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 30 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 17814 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 32 "./src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 17819 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
-- Visibility --------------------------------------------------
-- cata
sem_Visibility :: Visibility ->
                  T_Visibility
sem_Visibility (Default) =
    (sem_Visibility_Default)
sem_Visibility (Hidden) =
    (sem_Visibility_Hidden)
sem_Visibility (Protected) =
    (sem_Visibility_Protected)
-- semantic domain
type T_Visibility = ( Visibility)
data Inh_Visibility = Inh_Visibility {}
data Syn_Visibility = Syn_Visibility {self_Syn_Visibility :: Visibility}
wrap_Visibility :: T_Visibility ->
                   Inh_Visibility ->
                   Syn_Visibility
wrap_Visibility sem (Inh_Visibility) =
    (let ( _lhsOself) = sem
     in  (Syn_Visibility _lhsOself))
sem_Visibility_Default :: T_Visibility
sem_Visibility_Default =
    (let _lhsOself :: Visibility
         _self =
             Default
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Visibility_Hidden :: T_Visibility
sem_Visibility_Hidden =
    (let _lhsOself :: Visibility
         _self =
             Hidden
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Visibility_Protected :: T_Visibility
sem_Visibility_Protected =
    (let _lhsOself :: Visibility
         _self =
             Protected
         _lhsOself =
             _self
     in  ( _lhsOself))