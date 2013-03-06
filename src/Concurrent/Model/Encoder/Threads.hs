

{-#LANGUAGE RecordWildCards #-}
-- UUAGC 0.9.40.3 (src/Concurrent/Model/Encoder/Threads.ag)
module Concurrent.Model.Encoder.Threads where

{-# LINE 23 "src/Concurrent/Model/Encoder/Threads.ag" #-}

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
#if __GLASGOW_HASKELL__ >= 704
import Data.Map hiding (foldr)
#else
import Data.Map 
#endif
{-# LINE 41 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 11 "src/Concurrent/Model/Encoder/Global.ag" #-}

import Control.Applicative ((<$>))
import Control.Monad       (mplus)
{-# LINE 47 "src/Concurrent/Model/Encoder/Threads.hs" #-}
{-# LINE 5 "src/Concurrent/Model/Encoder/Threads.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder.Threads
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 54 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 322 "src/Concurrent/Model/Encoder/Threads.ag" #-}

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

encTs :: Functions -> [SExpr] -> [Map.Map String (SExpr, Maybe SExpr)] -> [[(SExpr, Maybe SExpr)]] -> [[(SExpr, Maybe SExpr)]] -> PreEncoder -> Map.Map String PC -> Map.Map String CF -> SExpr
encTs fs sparks cpcs prds muts p ep cfg = let ts = ts_Syn_Functions $ wrap_Functions (sem_Functions fs) $ Inh_Functions { prenc_Inh_Functions = p, cfg_Inh_Functions = cfg, cte_Inh_Functions = ep, mutexes_Inh_Functions = muts }
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
{-# LINE 162 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Grammar/Base.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Base
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 170 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 152 "src/Language/LLVMIR/Grammar/Base.ag" #-}

emptyFunction :: Function
emptyFunction = FunctionDef (Global "undefined") ExternalLinkage TyVoid [] []
{-# LINE 176 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Grammar/Instruction.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Grammar.Instruction
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 184 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Type/Type.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Type.Type
-- Copyright :  (c) 2012 Marcelo Sousa
-- Standard LLVM IR Types
-------------------------------------------------------------------------------
{-# LINE 193 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "src/Concurrent/Model/Encoder/Types.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder.Types
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 201 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 107 "src/Concurrent/Model/Encoder/Types.ag" #-}

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

{-# LINE 251 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "src/Concurrent/Model/Encoder/Global.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder.Global
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 259 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 52 "src/Concurrent/Model/Encoder/Global.ag" #-}

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
encGlobalVars gvars gs = let gw = wrap_Globals (sem_Globals gvars) $ Inh_Globals { gs_Inh_Globals = gs }
                             me  = case sexpr_Syn_Globals gw of
                                        []  -> []
                                        [e] -> [assert e]
                                        _        -> error "encGlobalVars" 
                         in (gs_Syn_Globals gw, sexprs_Syn_Globals gw ++ me)

{-# LINE 290 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "src/Concurrent/Model/Encoder/Value.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder.Value
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 298 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 251 "src/Concurrent/Model/Encoder/Value.ag" #-}

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
encValue v mts val tn = let vw = wrap_Value (sem_Value v) $ Inh_Value {mts_Inh_Value = mts, tn_Inh_Value = tn, val_Inh_Value = val}
                        in (mts_Syn_Value vw, sexprs_Syn_Value vw, sexpr_Syn_Value vw)

encParameter :: Parameter -> TypeEnv -> String -> (TypeEnv, SExpressions, SExpr)
encParameter p mts tn = let pw = wrap_Parameter (sem_Parameter p) $ Inh_Parameter {mts_Inh_Parameter = mts, tn_Inh_Parameter = tn}
                        in (mts_Syn_Parameter pw, sexprs_Syn_Parameter pw, sexpr_Syn_Parameter pw)


getValueType :: Value -> Type 
getValueType v = vtype_Syn_Value $ wrap_Value (sem_Value v) $ Inh_Value {mts_Inh_Value = undefined, tn_Inh_Value = undefined, val_Inh_Value = undefined}

getFnValueName :: Value -> Id
getFnValueName (Constant (GlobalValue (FunctionValue (Global n) _))) = n
getFnValueName _ = error "getFnValueName failed"
{-# LINE 354 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 1 "src/Concurrent/Model/Encoder/Identifier.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Encoder.Identifier
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 362 "src/Concurrent/Model/Encoder/Threads.hs" #-}

{-# LINE 35 "src/Concurrent/Model/Encoder/Identifier.ag" #-}


freshId :: Id -> Id
freshId x = x ++ "0"

{-# LINE 370 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
type T_Argument = ( Argument)
data Inh_Argument = Inh_Argument {}
data Syn_Argument = Syn_Argument {self_Syn_Argument :: Argument}
wrap_Argument :: T_Argument ->
                 Inh_Argument ->
                 Syn_Argument
wrap_Argument sem (Inh_Argument) =
    (let ( _lhsOself) = sem
     in  (Syn_Argument _lhsOself))
sem_Argument_Argument :: T_Value ->
                         T_Argument
sem_Argument_Argument arg_ =
    (let _lhsOself :: Argument
         _argOmts :: TypeEnv
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
         _argOmts =
             ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: Argument.Argument.arg.mts"
              {-# LINE 503 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _argOtn =
             ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: Argument.Argument.arg.tn"
              {-# LINE 508 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _argOval =
             ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: Argument.Argument.arg.val"
              {-# LINE 513 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         ( _argIident,_argIisGlobal,_argImts,_argIpsexpr,_argIself,_argIsexpr,_argIsexprs,_argIsort,_argIvtype) =
             arg_ _argOmts _argOtn _argOval
     in  ( _lhsOself))
-- Arguments ---------------------------------------------------
-- cata
sem_Arguments :: Arguments ->
                 T_Arguments
sem_Arguments list =
    (Prelude.foldr sem_Arguments_Cons sem_Arguments_Nil (Prelude.map sem_Argument list))
-- semantic domain
type T_Arguments = ( Arguments)
data Inh_Arguments = Inh_Arguments {}
data Syn_Arguments = Syn_Arguments {self_Syn_Arguments :: Arguments}
wrap_Arguments :: T_Arguments ->
                  Inh_Arguments ->
                  Syn_Arguments
wrap_Arguments sem (Inh_Arguments) =
    (let ( _lhsOself) = sem
     in  (Syn_Arguments _lhsOself))
sem_Arguments_Cons :: T_Argument ->
                      T_Arguments ->
                      T_Arguments
sem_Arguments_Cons hd_ tl_ =
    (let _lhsOself :: Arguments
         _hdIself :: Argument
         _tlIself :: Arguments
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Arguments_Nil :: T_Arguments
sem_Arguments_Nil =
    (let _lhsOself :: Arguments
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- AtomicOrdering ----------------------------------------------
-- cata
sem_AtomicOrdering :: AtomicOrdering ->
                      T_AtomicOrdering
sem_AtomicOrdering (Acquire) =
    (sem_AtomicOrdering_Acquire)
sem_AtomicOrdering (AcquireRelease) =
    (sem_AtomicOrdering_AcquireRelease)
sem_AtomicOrdering (Monotonic) =
    (sem_AtomicOrdering_Monotonic)
sem_AtomicOrdering (NotAtomic) =
    (sem_AtomicOrdering_NotAtomic)
sem_AtomicOrdering (Release) =
    (sem_AtomicOrdering_Release)
sem_AtomicOrdering (SequentiallyConsistent) =
    (sem_AtomicOrdering_SequentiallyConsistent)
sem_AtomicOrdering (Unordered) =
    (sem_AtomicOrdering_Unordered)
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
sem_AtomicOrdering_Acquire :: T_AtomicOrdering
sem_AtomicOrdering_Acquire =
    (let _lhsOself :: AtomicOrdering
         _self =
             Acquire
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
sem_AtomicOrdering_Monotonic :: T_AtomicOrdering
sem_AtomicOrdering_Monotonic =
    (let _lhsOself :: AtomicOrdering
         _self =
             Monotonic
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_AtomicOrdering_NotAtomic :: T_AtomicOrdering
sem_AtomicOrdering_NotAtomic =
    (let _lhsOself :: AtomicOrdering
         _self =
             NotAtomic
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
sem_AtomicOrdering_SequentiallyConsistent :: T_AtomicOrdering
sem_AtomicOrdering_SequentiallyConsistent =
    (let _lhsOself :: AtomicOrdering
         _self =
             SequentiallyConsistent
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
-- Attribute ---------------------------------------------------
-- cata
sem_Attribute :: Attribute ->
                 T_Attribute
sem_Attribute (AlwaysInlineAttribute) =
    (sem_Attribute_AlwaysInlineAttribute)
sem_Attribute (ByValAttribute) =
    (sem_Attribute_ByValAttribute)
sem_Attribute (InRegAttribute) =
    (sem_Attribute_InRegAttribute)
sem_Attribute (NakedAttribute) =
    (sem_Attribute_NakedAttribute)
sem_Attribute (NestAttribute) =
    (sem_Attribute_NestAttribute)
sem_Attribute (NoAliasAttribute) =
    (sem_Attribute_NoAliasAttribute)
sem_Attribute (NoCaptureAttribute) =
    (sem_Attribute_NoCaptureAttribute)
sem_Attribute (NoImplicitFloatAttribute) =
    (sem_Attribute_NoImplicitFloatAttribute)
sem_Attribute (NoInlineAttribute) =
    (sem_Attribute_NoInlineAttribute)
sem_Attribute (NoRedZoneAttribute) =
    (sem_Attribute_NoRedZoneAttribute)
sem_Attribute (NoReturnAttribute) =
    (sem_Attribute_NoReturnAttribute)
sem_Attribute (NoUnwindAttribute) =
    (sem_Attribute_NoUnwindAttribute)
sem_Attribute (OptimizeForSizeAttribute) =
    (sem_Attribute_OptimizeForSizeAttribute)
sem_Attribute (ReadNoneAttribute) =
    (sem_Attribute_ReadNoneAttribute)
sem_Attribute (ReadOnlyAttribute) =
    (sem_Attribute_ReadOnlyAttribute)
sem_Attribute (SExtAttribute) =
    (sem_Attribute_SExtAttribute)
sem_Attribute (StackProtectAttribute) =
    (sem_Attribute_StackProtectAttribute)
sem_Attribute (StackProtectReqAttribute) =
    (sem_Attribute_StackProtectReqAttribute)
sem_Attribute (StructRetAttribute) =
    (sem_Attribute_StructRetAttribute)
sem_Attribute (ZExtAttribute) =
    (sem_Attribute_ZExtAttribute)
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
sem_Attribute_AlwaysInlineAttribute :: T_Attribute
sem_Attribute_AlwaysInlineAttribute =
    (let _lhsOself :: Attribute
         _self =
             AlwaysInlineAttribute
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
sem_Attribute_InRegAttribute :: T_Attribute
sem_Attribute_InRegAttribute =
    (let _lhsOself :: Attribute
         _self =
             InRegAttribute
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
sem_Attribute_NestAttribute :: T_Attribute
sem_Attribute_NestAttribute =
    (let _lhsOself :: Attribute
         _self =
             NestAttribute
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
sem_Attribute_NoCaptureAttribute :: T_Attribute
sem_Attribute_NoCaptureAttribute =
    (let _lhsOself :: Attribute
         _self =
             NoCaptureAttribute
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
sem_Attribute_NoInlineAttribute :: T_Attribute
sem_Attribute_NoInlineAttribute =
    (let _lhsOself :: Attribute
         _self =
             NoInlineAttribute
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
sem_Attribute_NoReturnAttribute :: T_Attribute
sem_Attribute_NoReturnAttribute =
    (let _lhsOself :: Attribute
         _self =
             NoReturnAttribute
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
sem_Attribute_OptimizeForSizeAttribute :: T_Attribute
sem_Attribute_OptimizeForSizeAttribute =
    (let _lhsOself :: Attribute
         _self =
             OptimizeForSizeAttribute
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
sem_Attribute_SExtAttribute :: T_Attribute
sem_Attribute_SExtAttribute =
    (let _lhsOself :: Attribute
         _self =
             SExtAttribute
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
sem_Attribute_StructRetAttribute :: T_Attribute
sem_Attribute_StructRetAttribute =
    (let _lhsOself :: Attribute
         _self =
             StructRetAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Attribute_ZExtAttribute :: T_Attribute
sem_Attribute_ZExtAttribute =
    (let _lhsOself :: Attribute
         _self =
             ZExtAttribute
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
    (sem_BasicBlock_BasicBlock (sem_Label _label) (sem_Instructions _instrs))
-- semantic domain
type T_BasicBlock = CF ->
                    (Map.Map String PC) ->
                    ([[(SExpr, Maybe SExpr)]]) ->
                    (Map.Map String [PC]) ->
                    PreEncoder ->
                    (Int -> SExpr) ->
                    Id ->
                    ( BasicBlock,([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr]))
data Inh_BasicBlock = Inh_BasicBlock {cfg_Inh_BasicBlock :: CF,cte_Inh_BasicBlock :: (Map.Map String PC),mutexes_Inh_BasicBlock :: ([[(SExpr, Maybe SExpr)]]),pcs_Inh_BasicBlock :: (Map.Map String [PC]),prenc_Inh_BasicBlock :: PreEncoder,spark_Inh_BasicBlock :: (Int -> SExpr),tn_Inh_BasicBlock :: Id}
data Syn_BasicBlock = Syn_BasicBlock {self_Syn_BasicBlock :: BasicBlock,ts_Syn_BasicBlock :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])}
wrap_BasicBlock :: T_BasicBlock ->
                   Inh_BasicBlock ->
                   Syn_BasicBlock
wrap_BasicBlock sem (Inh_BasicBlock _lhsIcfg _lhsIcte _lhsImutexes _lhsIpcs _lhsIprenc _lhsIspark _lhsItn) =
    (let ( _lhsOself,_lhsOts) = sem _lhsIcfg _lhsIcte _lhsImutexes _lhsIpcs _lhsIprenc _lhsIspark _lhsItn
     in  (Syn_BasicBlock _lhsOself _lhsOts))
sem_BasicBlock_BasicBlock :: T_Label ->
                             T_Instructions ->
                             T_BasicBlock
sem_BasicBlock_BasicBlock label_ instrs_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: BasicBlock
              _instrsOcfg :: CF
              _instrsOcte :: (Map.Map String PC)
              _instrsOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _instrsOpcs :: (Map.Map String [PC])
              _instrsOprenc :: PreEncoder
              _instrsOspark :: (Int -> SExpr)
              _instrsOtn :: Id
              _labelIself :: Label
              _instrsIself :: Instructions
              _instrsIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOts =
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _instrsIts
                   {-# LINE 945 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  BasicBlock _labelIself _instrsIself
              _lhsOself =
                  _self
              _instrsOcfg =
                  ({-# LINE 62 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 954 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOcte =
                  ({-# LINE 63 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 959 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOmutexes =
                  ({-# LINE 68 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 964 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOpcs =
                  ({-# LINE 64 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIpcs
                   {-# LINE 969 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOprenc =
                  ({-# LINE 66 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 974 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOspark =
                  ({-# LINE 67 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIspark
                   {-# LINE 979 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _instrsOtn =
                  ({-# LINE 65 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 984 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _labelIself) =
                  label_
              ( _instrsIself,_instrsIts) =
                  instrs_ _instrsOcfg _instrsOcte _instrsOmutexes _instrsOpcs _instrsOprenc _instrsOspark _instrsOtn
          in  ( _lhsOself,_lhsOts)))
-- BasicBlocks -------------------------------------------------
-- cata
sem_BasicBlocks :: BasicBlocks ->
                   T_BasicBlocks
sem_BasicBlocks list =
    (Prelude.foldr sem_BasicBlocks_Cons sem_BasicBlocks_Nil (Prelude.map sem_BasicBlock list))
-- semantic domain
type T_BasicBlocks = CF ->
                     (Map.Map String PC) ->
                     ([[(SExpr, Maybe SExpr)]]) ->
                     (Map.Map String [PC]) ->
                     PreEncoder ->
                     (Int -> SExpr) ->
                     Id ->
                     ( BasicBlocks,([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr]))
data Inh_BasicBlocks = Inh_BasicBlocks {cfg_Inh_BasicBlocks :: CF,cte_Inh_BasicBlocks :: (Map.Map String PC),mutexes_Inh_BasicBlocks :: ([[(SExpr, Maybe SExpr)]]),pcs_Inh_BasicBlocks :: (Map.Map String [PC]),prenc_Inh_BasicBlocks :: PreEncoder,spark_Inh_BasicBlocks :: (Int -> SExpr),tn_Inh_BasicBlocks :: Id}
data Syn_BasicBlocks = Syn_BasicBlocks {self_Syn_BasicBlocks :: BasicBlocks,ts_Syn_BasicBlocks :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])}
wrap_BasicBlocks :: T_BasicBlocks ->
                    Inh_BasicBlocks ->
                    Syn_BasicBlocks
wrap_BasicBlocks sem (Inh_BasicBlocks _lhsIcfg _lhsIcte _lhsImutexes _lhsIpcs _lhsIprenc _lhsIspark _lhsItn) =
    (let ( _lhsOself,_lhsOts) = sem _lhsIcfg _lhsIcte _lhsImutexes _lhsIpcs _lhsIprenc _lhsIspark _lhsItn
     in  (Syn_BasicBlocks _lhsOself _lhsOts))
sem_BasicBlocks_Cons :: T_BasicBlock ->
                        T_BasicBlocks ->
                        T_BasicBlocks
sem_BasicBlocks_Cons hd_ tl_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: BasicBlocks
              _hdOcfg :: CF
              _hdOcte :: (Map.Map String PC)
              _hdOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _hdOpcs :: (Map.Map String [PC])
              _hdOprenc :: PreEncoder
              _hdOspark :: (Int -> SExpr)
              _hdOtn :: Id
              _tlOcfg :: CF
              _tlOcte :: (Map.Map String PC)
              _tlOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _tlOpcs :: (Map.Map String [PC])
              _tlOprenc :: PreEncoder
              _tlOspark :: (Int -> SExpr)
              _tlOtn :: Id
              _hdIself :: BasicBlock
              _hdIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _tlIself :: BasicBlocks
              _tlIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOts =
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _hdIts ++ _tlIts
                   {-# LINE 1048 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOcfg =
                  ({-# LINE 62 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 1057 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOcte =
                  ({-# LINE 63 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 1062 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmutexes =
                  ({-# LINE 68 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 1067 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOpcs =
                  ({-# LINE 64 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIpcs
                   {-# LINE 1072 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOprenc =
                  ({-# LINE 66 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 1077 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOspark =
                  ({-# LINE 67 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIspark
                   {-# LINE 1082 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 65 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 1087 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOcfg =
                  ({-# LINE 62 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 1092 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOcte =
                  ({-# LINE 63 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 1097 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmutexes =
                  ({-# LINE 68 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 1102 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOpcs =
                  ({-# LINE 64 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIpcs
                   {-# LINE 1107 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOprenc =
                  ({-# LINE 66 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 1112 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOspark =
                  ({-# LINE 67 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIspark
                   {-# LINE 1117 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 65 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 1122 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIself,_hdIts) =
                  hd_ _hdOcfg _hdOcte _hdOmutexes _hdOpcs _hdOprenc _hdOspark _hdOtn
              ( _tlIself,_tlIts) =
                  tl_ _tlOcfg _tlOcte _tlOmutexes _tlOpcs _tlOprenc _tlOspark _tlOtn
          in  ( _lhsOself,_lhsOts)))
sem_BasicBlocks_Nil :: T_BasicBlocks
sem_BasicBlocks_Nil =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: BasicBlocks
              _lhsOts =
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 1143 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
sem_BinOp (OpAdd) =
    (sem_BinOp_OpAdd)
sem_BinOp (OpAnd) =
    (sem_BinOp_OpAnd)
sem_BinOp (OpMax) =
    (sem_BinOp_OpMax)
sem_BinOp (OpMin) =
    (sem_BinOp_OpMin)
sem_BinOp (OpNand) =
    (sem_BinOp_OpNand)
sem_BinOp (OpOr) =
    (sem_BinOp_OpOr)
sem_BinOp (OpSub) =
    (sem_BinOp_OpSub)
sem_BinOp (OpUMax) =
    (sem_BinOp_OpUMax)
sem_BinOp (OpUMin) =
    (sem_BinOp_OpUMin)
sem_BinOp (OpXchg) =
    (sem_BinOp_OpXchg)
sem_BinOp (OpXor) =
    (sem_BinOp_OpXor)
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
sem_BinOp_OpAdd :: T_BinOp
sem_BinOp_OpAdd =
    (let _lhsOself :: BinOp
         _self =
             OpAdd
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
sem_BinOp_OpSub :: T_BinOp
sem_BinOp_OpSub =
    (let _lhsOself :: BinOp
         _self =
             OpSub
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
sem_BinOp_OpXchg :: T_BinOp
sem_BinOp_OpXchg =
    (let _lhsOself :: BinOp
         _self =
             OpXchg
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
-- CConv -------------------------------------------------------
-- cata
sem_CConv :: CConv ->
             T_CConv
sem_CConv (Cc _n) =
    (sem_CConv_Cc _n)
sem_CConv (Cc10) =
    (sem_CConv_Cc10)
sem_CConv (Ccc) =
    (sem_CConv_Ccc)
sem_CConv (Coldcc) =
    (sem_CConv_Coldcc)
sem_CConv (Fastcc) =
    (sem_CConv_Fastcc)
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
sem_CConv_Cc :: Int ->
                T_CConv
sem_CConv_Cc n_ =
    (let _lhsOself :: CConv
         _self =
             Cc n_
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
sem_CConv_Ccc :: T_CConv
sem_CConv_Ccc =
    (let _lhsOself :: CConv
         _self =
             Ccc
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
sem_CConv_Fastcc :: T_CConv
sem_CConv_Fastcc =
    (let _lhsOself :: CConv
         _self =
             Fastcc
         _lhsOself =
             _self
     in  ( _lhsOself))
-- CompareConstantExpr -----------------------------------------
-- cata
sem_CompareConstantExpr :: CompareConstantExpr ->
                           T_CompareConstantExpr
sem_CompareConstantExpr (FCmpExpr _cond _ty _op1 _op2) =
    (sem_CompareConstantExpr_FCmpExpr (sem_RealPredicate _cond) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_CompareConstantExpr (ICmpExpr _cond _ty _op1 _op2) =
    (sem_CompareConstantExpr_ICmpExpr (sem_IntPredicate _cond) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
-- semantic domain
type T_CompareConstantExpr = TypeEnv ->
                             String ->
                             (Map.Map Id (Type, [PC])) ->
                             ( TypeEnv,CompareConstantExpr,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_CompareConstantExpr = Inh_CompareConstantExpr {mts_Inh_CompareConstantExpr :: TypeEnv,tn_Inh_CompareConstantExpr :: String,val_Inh_CompareConstantExpr :: (Map.Map Id (Type, [PC]))}
data Syn_CompareConstantExpr = Syn_CompareConstantExpr {mts_Syn_CompareConstantExpr :: TypeEnv,self_Syn_CompareConstantExpr :: CompareConstantExpr,sexpr_Syn_CompareConstantExpr :: ([SExpr]),sexprs_Syn_CompareConstantExpr :: SExpressions,sort_Syn_CompareConstantExpr :: SSortExpr,vtype_Syn_CompareConstantExpr :: Type}
wrap_CompareConstantExpr :: T_CompareConstantExpr ->
                            Inh_CompareConstantExpr ->
                            Syn_CompareConstantExpr
wrap_CompareConstantExpr sem (Inh_CompareConstantExpr _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_CompareConstantExpr _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_CompareConstantExpr_FCmpExpr :: T_RealPredicate ->
                                    T_Type ->
                                    T_Value ->
                                    T_Value ->
                                    T_CompareConstantExpr
sem_CompareConstantExpr_FCmpExpr cond_ ty_ op1_ op2_ =
    (\ _lhsImts
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
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 226 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 1410 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 227 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1415 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 228 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1420 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 229 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1425 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 230 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 1430 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 231 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 1435 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FCmpExpr _condIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op2Imts
                   {-# LINE 1444 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1449 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1454 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1459 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 1464 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1469 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1474 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _condIself) =
                  cond_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_CompareConstantExpr_ICmpExpr :: T_IntPredicate ->
                                    T_Type ->
                                    T_Value ->
                                    T_Value ->
                                    T_CompareConstantExpr
sem_CompareConstantExpr_ICmpExpr cond_ ty_ op1_ op2_ =
    (\ _lhsImts
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
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 226 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 1536 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 227 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1541 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 228 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1546 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 229 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1551 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 230 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 1556 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 231 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 1561 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ICmpExpr _condIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op2Imts
                   {-# LINE 1570 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1575 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1580 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1585 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 1590 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1595 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1600 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _condIpred,_condIself) =
                  cond_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- ComplexConstant ---------------------------------------------
-- cata
sem_ComplexConstant :: ComplexConstant ->
                       T_ComplexConstant
sem_ComplexConstant (ConstantAggregateZero _ty) =
    (sem_ComplexConstant_ConstantAggregateZero (sem_Type _ty))
sem_ComplexConstant (ConstantArray _ty _vals) =
    (sem_ComplexConstant_ConstantArray (sem_Type _ty) (sem_Values _vals))
sem_ComplexConstant (ConstantDataSequential _cds) =
    (sem_ComplexConstant_ConstantDataSequential (sem_ConstantDataSequential _cds))
sem_ComplexConstant (ConstantStruct _ty _vals) =
    (sem_ComplexConstant_ConstantStruct (sem_Type _ty) (sem_Values _vals))
sem_ComplexConstant (ConstantVector _ty _vals) =
    (sem_ComplexConstant_ConstantVector (sem_Type _ty) (sem_Values _vals))
-- semantic domain
type T_ComplexConstant = TypeEnv ->
                         String ->
                         (Map.Map Id (Type, [PC])) ->
                         ( TypeEnv,ComplexConstant,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_ComplexConstant = Inh_ComplexConstant {mts_Inh_ComplexConstant :: TypeEnv,tn_Inh_ComplexConstant :: String,val_Inh_ComplexConstant :: (Map.Map Id (Type, [PC]))}
data Syn_ComplexConstant = Syn_ComplexConstant {mts_Syn_ComplexConstant :: TypeEnv,self_Syn_ComplexConstant :: ComplexConstant,sexpr_Syn_ComplexConstant :: ([SExpr]),sexprs_Syn_ComplexConstant :: SExpressions,sort_Syn_ComplexConstant :: SSortExpr,vtype_Syn_ComplexConstant :: Type}
wrap_ComplexConstant :: T_ComplexConstant ->
                        Inh_ComplexConstant ->
                        Syn_ComplexConstant
wrap_ComplexConstant sem (Inh_ComplexConstant _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_ComplexConstant _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_ComplexConstant_ConstantAggregateZero :: T_Type ->
                                             T_ComplexConstant
sem_ComplexConstant_ConstantAggregateZero ty_ =
    (\ _lhsImts
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
                  ({-# LINE 141 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 1660 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 142 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1665 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 143 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 1670 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 144 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 1675 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 145 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 1680 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 146 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 1685 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantAggregateZero _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1694 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ComplexConstant_ConstantArray :: T_Type ->
                                     T_Values ->
                                     T_ComplexConstant
sem_ComplexConstant_ConstantArray ty_ vals_ =
    (\ _lhsImts
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
              _valsOmts :: TypeEnv
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
                  ({-# LINE 141 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 1730 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 142 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1735 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 143 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 1740 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 144 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 1745 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 145 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 1750 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 146 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 1755 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantArray _tyIself _valsIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _valsImts
                   {-# LINE 1764 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOmts =
                  ({-# LINE 27 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1769 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOtn =
                  ({-# LINE 29 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1774 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOval =
                  ({-# LINE 28 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1779 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _valsImts,_valsIself,_valsIsexpr,_valsIsexprs,_valsIvtype) =
                  vals_ _valsOmts _valsOtn _valsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ComplexConstant_ConstantDataSequential :: T_ConstantDataSequential ->
                                              T_ComplexConstant
sem_ComplexConstant_ConstantDataSequential cds_ =
    (\ _lhsImts
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
                  ({-# LINE 136 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cdsIsexpr
                   {-# LINE 1810 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 137 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cdsIsexprs
                   {-# LINE 1815 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 138 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cdsIvtype
                   {-# LINE 1820 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 139 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cdsIsort
                   {-# LINE 1825 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantDataSequential _cdsIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cdsImts
                   {-# LINE 1834 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cdsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1839 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cdsOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1844 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cdsOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1849 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _cdsImts,_cdsIself,_cdsIsexpr,_cdsIsexprs,_cdsIsort,_cdsIvtype) =
                  cds_ _cdsOmts _cdsOtn _cdsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ComplexConstant_ConstantStruct :: T_Type ->
                                      T_Values ->
                                      T_ComplexConstant
sem_ComplexConstant_ConstantStruct ty_ vals_ =
    (\ _lhsImts
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
              _valsOmts :: TypeEnv
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
                  ({-# LINE 141 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 1885 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 142 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1890 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 143 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 1895 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 144 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 1900 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 145 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 1905 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 146 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 1910 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantStruct _tyIself _valsIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _valsImts
                   {-# LINE 1919 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOmts =
                  ({-# LINE 27 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1924 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOtn =
                  ({-# LINE 29 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1929 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOval =
                  ({-# LINE 28 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1934 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _valsImts,_valsIself,_valsIsexpr,_valsIsexprs,_valsIvtype) =
                  vals_ _valsOmts _valsOtn _valsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ComplexConstant_ConstantVector :: T_Type ->
                                      T_Values ->
                                      T_ComplexConstant
sem_ComplexConstant_ConstantVector ty_ vals_ =
    (\ _lhsImts
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
              _valsOmts :: TypeEnv
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
                  ({-# LINE 148 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 1972 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 149 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 1977 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 150 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1982 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantVector _tyIself _valsIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _valsImts
                   {-# LINE 1991 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 1996 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: ComplexConstant.ConstantVector.ty.mn"
                   {-# LINE 2001 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 2006 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOmts =
                  ({-# LINE 27 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 2011 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOtn =
                  ({-# LINE 29 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2016 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valsOval =
                  ({-# LINE 28 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2021 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _valsImts,_valsIself,_valsIsexpr,_valsIsexprs,_valsIvtype) =
                  vals_ _valsOmts _valsOtn _valsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- Constant ----------------------------------------------------
-- cata
sem_Constant :: Constant ->
                T_Constant
sem_Constant (BlockAddr) =
    (sem_Constant_BlockAddr)
sem_Constant (CmpConst _cc) =
    (sem_Constant_CmpConst (sem_ComplexConstant _cc))
sem_Constant (ConstantExpr _expr) =
    (sem_Constant_ConstantExpr (sem_ConstantExpr _expr))
sem_Constant (GlobalValue _gv) =
    (sem_Constant_GlobalValue (sem_GlobalValue _gv))
sem_Constant (PoisonValue) =
    (sem_Constant_PoisonValue)
sem_Constant (SmpConst _sc) =
    (sem_Constant_SmpConst (sem_SimpleConstant _sc))
sem_Constant (UndefValue) =
    (sem_Constant_UndefValue)
-- semantic domain
type T_Constant = TypeEnv ->
                  String ->
                  (Map.Map Id (Type, [PC])) ->
                  ( (Maybe String),Bool,TypeEnv,(Int -> [SExpr]),Constant,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_Constant = Inh_Constant {mts_Inh_Constant :: TypeEnv,tn_Inh_Constant :: String,val_Inh_Constant :: (Map.Map Id (Type, [PC]))}
data Syn_Constant = Syn_Constant {ident_Syn_Constant :: (Maybe String),isGlobal_Syn_Constant :: Bool,mts_Syn_Constant :: TypeEnv,psexpr_Syn_Constant :: (Int -> [SExpr]),self_Syn_Constant :: Constant,sexpr_Syn_Constant :: ([SExpr]),sexprs_Syn_Constant :: SExpressions,sort_Syn_Constant :: SSortExpr,vtype_Syn_Constant :: Type}
wrap_Constant :: T_Constant ->
                 Inh_Constant ->
                 Syn_Constant
wrap_Constant sem (Inh_Constant _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_Constant _lhsOident _lhsOisGlobal _lhsOmts _lhsOpsexpr _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_Constant_BlockAddr :: T_Constant
sem_Constant_BlockAddr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOisGlobal =
                  ({-# LINE 90 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 2076 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 91 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2081 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 92 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> []
                   {-# LINE 2086 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 96 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2091 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 97 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2096 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 98 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2101 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  BlockAddr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2110 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Constant.BlockAddr.lhs.sort"
                   {-# LINE 2115 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Constant_CmpConst :: T_ComplexConstant ->
                         T_Constant
sem_Constant_CmpConst cc_ =
    (\ _lhsImts
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
              _ccOmts :: TypeEnv
              _ccOtn :: String
              _ccOval :: (Map.Map Id (Type, [PC]))
              _ccImts :: TypeEnv
              _ccIself :: ComplexConstant
              _ccIsexpr :: ([SExpr])
              _ccIsexprs :: SExpressions
              _ccIsort :: SSortExpr
              _ccIvtype :: Type
              _lhsOisGlobal =
                  ({-# LINE 90 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 2145 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 91 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2150 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 92 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> []
                   {-# LINE 2155 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  CmpConst _ccIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _ccImts
                   {-# LINE 2164 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 46 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _ccIsexpr
                   {-# LINE 2169 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 45 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _ccIsexprs
                   {-# LINE 2174 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _ccIsort
                   {-# LINE 2179 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 49 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _ccIvtype
                   {-# LINE 2184 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ccOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2189 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ccOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2194 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ccOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2199 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _ccImts,_ccIself,_ccIsexpr,_ccIsexprs,_ccIsort,_ccIvtype) =
                  cc_ _ccOmts _ccOtn _ccOval
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Constant_ConstantExpr :: T_ConstantExpr ->
                             T_Constant
sem_Constant_ConstantExpr expr_ =
    (\ _lhsImts
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
              _exprOmts :: TypeEnv
              _exprOtn :: String
              _exprOval :: (Map.Map Id (Type, [PC]))
              _exprImts :: TypeEnv
              _exprIself :: ConstantExpr
              _exprIsexpr :: ([SExpr])
              _exprIsexprs :: SExpressions
              _exprIsort :: SSortExpr
              _exprIvtype :: Type
              _lhsOisGlobal =
                  ({-# LINE 90 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 2231 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 91 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2236 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 92 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> []
                   {-# LINE 2241 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 101 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _exprIsexpr
                   {-# LINE 2246 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 102 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _exprIsexprs
                   {-# LINE 2251 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 103 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _exprIvtype
                   {-# LINE 2256 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 104 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _exprIsort
                   {-# LINE 2261 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantExpr _exprIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _exprImts
                   {-# LINE 2270 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _exprOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2275 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _exprOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2280 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _exprOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2285 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _exprImts,_exprIself,_exprIsexpr,_exprIsexprs,_exprIsort,_exprIvtype) =
                  expr_ _exprOmts _exprOtn _exprOval
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Constant_GlobalValue :: T_GlobalValue ->
                            T_Constant
sem_Constant_GlobalValue gv_ =
    (\ _lhsImts
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
                  ({-# LINE 86 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIisGlobal
                   {-# LINE 2320 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 87 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIident
                   {-# LINE 2325 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 88 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIpsexpr
                   {-# LINE 2330 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 107 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIsexpr
                   {-# LINE 2335 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 108 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIsexprs
                   {-# LINE 2340 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 109 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIvtype
                   {-# LINE 2345 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 110 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvIsort
                   {-# LINE 2350 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  GlobalValue _gvIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _gvImts
                   {-# LINE 2359 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _gvOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2364 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _gvOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2369 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _gvOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2374 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _gvIident,_gvIisGlobal,_gvImts,_gvIpsexpr,_gvIself,_gvIsexpr,_gvIsexprs,_gvIsort,_gvIvtype) =
                  gv_ _gvOmts _gvOtn _gvOval
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Constant_PoisonValue :: T_Constant
sem_Constant_PoisonValue =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOisGlobal =
                  ({-# LINE 90 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 2396 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 91 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2401 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 92 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> []
                   {-# LINE 2406 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 96 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2411 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 97 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2416 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 98 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2421 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  PoisonValue
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2430 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Constant.PoisonValue.lhs.sort"
                   {-# LINE 2435 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Constant_SmpConst :: T_SimpleConstant ->
                         T_Constant
sem_Constant_SmpConst sc_ =
    (\ _lhsImts
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
                  ({-# LINE 90 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 2465 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 91 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2470 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 92 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> []
                   {-# LINE 2475 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  SmpConst _scIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _scImts
                   {-# LINE 2484 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 46 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _scIsexpr
                   {-# LINE 2489 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 45 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _scIsexprs
                   {-# LINE 2494 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _scIsort
                   {-# LINE 2499 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 49 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _scIvtype
                   {-# LINE 2504 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _scOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2509 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _scOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2514 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _scOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2519 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _scImts,_scIself,_scIsexpr,_scIsexprs,_scIsort,_scIvtype) =
                  sc_ _scOmts _scOtn _scOval
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Constant_UndefValue :: T_Constant
sem_Constant_UndefValue =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOisGlobal :: Bool
              _lhsOident :: (Maybe String)
              _lhsOpsexpr :: (Int -> [SExpr])
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOisGlobal =
                  ({-# LINE 90 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 2541 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 91 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2546 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 92 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> []
                   {-# LINE 2551 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 96 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2556 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 97 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2561 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 98 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2566 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  UndefValue
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2575 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Constant.UndefValue.lhs.sort"
                   {-# LINE 2580 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
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
                  ({-# LINE 184 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2627 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 185 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2632 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 186 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2637 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 187 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2642 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 188 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 2647 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 189 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 2652 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantDataArray _tyIself val_
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 2661 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                  ({-# LINE 184 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 2689 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 185 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2694 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 186 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2699 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 187 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 2704 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 188 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 2709 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 189 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 2714 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantDataVector _tyIself val_
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 2723 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
type T_ConstantExpr = TypeEnv ->
                      String ->
                      (Map.Map Id (Type, [PC])) ->
                      ( TypeEnv,ConstantExpr,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_ConstantExpr = Inh_ConstantExpr {mts_Inh_ConstantExpr :: TypeEnv,tn_Inh_ConstantExpr :: String,val_Inh_ConstantExpr :: (Map.Map Id (Type, [PC]))}
data Syn_ConstantExpr = Syn_ConstantExpr {mts_Syn_ConstantExpr :: TypeEnv,self_Syn_ConstantExpr :: ConstantExpr,sexpr_Syn_ConstantExpr :: ([SExpr]),sexprs_Syn_ConstantExpr :: SExpressions,sort_Syn_ConstantExpr :: SSortExpr,vtype_Syn_ConstantExpr :: Type}
wrap_ConstantExpr :: T_ConstantExpr ->
                     Inh_ConstantExpr ->
                     Syn_ConstantExpr
wrap_ConstantExpr sem (Inh_ConstantExpr _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_ConstantExpr _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_ConstantExpr_BinaryConstantExpr :: T_ConstantExpr
sem_ConstantExpr_BinaryConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOsexpr =
                  ({-# LINE 206 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2779 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 207 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2784 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 208 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2789 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  BinaryConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2798 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: ConstantExpr.BinaryConstantExpr.lhs.sort"
                   {-# LINE 2803 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_CompareConstantExpr :: T_CompareConstantExpr ->
                                        T_ConstantExpr
sem_ConstantExpr_CompareConstantExpr cmpExpr_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOsort :: SSortExpr
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _cmpExprOmts :: TypeEnv
              _cmpExprOtn :: String
              _cmpExprOval :: (Map.Map Id (Type, [PC]))
              _cmpExprImts :: TypeEnv
              _cmpExprIself :: CompareConstantExpr
              _cmpExprIsexpr :: ([SExpr])
              _cmpExprIsexprs :: SExpressions
              _cmpExprIsort :: SSortExpr
              _cmpExprIvtype :: Type
              _lhsOsexpr =
                  ({-# LINE 219 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cmpExprIsexpr
                   {-# LINE 2830 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 220 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cmpExprIsexprs
                   {-# LINE 2835 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 221 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cmpExprIvtype
                   {-# LINE 2840 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 222 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cmpExprIsort
                   {-# LINE 2845 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  CompareConstantExpr _cmpExprIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cmpExprImts
                   {-# LINE 2854 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cmpExprOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2859 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cmpExprOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2864 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cmpExprOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2869 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _cmpExprImts,_cmpExprIself,_cmpExprIsexpr,_cmpExprIsexprs,_cmpExprIsort,_cmpExprIvtype) =
                  cmpExpr_ _cmpExprOmts _cmpExprOtn _cmpExprOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_ExtractElementConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ExtractElementConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOsexpr =
                  ({-# LINE 206 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2888 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 207 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2893 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 208 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2898 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ExtractElementConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2907 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: ConstantExpr.ExtractElementConstantExpr.lhs.sort"
                   {-# LINE 2912 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_ExtractValueConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ExtractValueConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOsexpr =
                  ({-# LINE 206 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2929 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 207 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2934 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 208 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2939 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ExtractValueConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2948 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: ConstantExpr.ExtractValueConstantExpr.lhs.sort"
                   {-# LINE 2953 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_GetElementPtrConstantExpr :: T_Value ->
                                              T_Values ->
                                              T_ConstantExpr
sem_ConstantExpr_GetElementPtrConstantExpr struct_ idxs_ =
    (\ _lhsImts
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
              _lhsOself :: ConstantExpr
              _lhsOsort :: SSortExpr
              _structOval :: (Map.Map Id (Type, [PC]))
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
                  ({-# LINE 194 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2992 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOmts =
                  ({-# LINE 195 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _structImts
                   {-# LINE 2997 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 196 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _idxsImts
                   {-# LINE 3002 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOtn =
                  ({-# LINE 197 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 3007 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOtn =
                  ({-# LINE 198 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 3012 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 199 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _structIvtype
                   {-# LINE 3017 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxn =
                  ({-# LINE 200 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   let x = getIdxN _structIvtype
                   in x
                   {-# LINE 3023 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 202 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _structIsexprs ++ _idxsIsexprs
                   {-# LINE 3028 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 203 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   [ foldr (\(n, s1) s2 -> sFn "select" s2 $ changeN s1 n) (head _structIsexpr) $ zip _idxn     $ init' _idxsIsexpr ]
                   {-# LINE 3033 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  GetElementPtrConstantExpr _structIself _idxsIself
              _lhsOself =
                  _self
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _structIsort
                   {-# LINE 3042 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 3047 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOval =
                  ({-# LINE 28 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 3052 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _structIident,_structIisGlobal,_structImts,_structIpsexpr,_structIself,_structIsexpr,_structIsexprs,_structIsort,_structIvtype) =
                  struct_ _structOmts _structOtn _structOval
              ( _idxsImts,_idxsIself,_idxsIsexpr,_idxsIsexprs,_idxsIvtype) =
                  idxs_ _idxsOmts _idxsOtn _idxsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_InsertElementConstantExpr :: T_ConstantExpr
sem_ConstantExpr_InsertElementConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOsexpr =
                  ({-# LINE 206 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3073 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 207 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3078 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 208 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3083 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  InsertElementConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3092 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: ConstantExpr.InsertElementConstantExpr.lhs.sort"
                   {-# LINE 3097 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_InsertValueConstantExpr :: T_ConstantExpr
sem_ConstantExpr_InsertValueConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOsexpr =
                  ({-# LINE 206 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3114 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 207 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3119 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 208 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3124 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  InsertValueConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3133 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: ConstantExpr.InsertValueConstantExpr.lhs.sort"
                   {-# LINE 3138 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_SelectConstantExpr :: T_ConstantExpr
sem_ConstantExpr_SelectConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOsexpr =
                  ({-# LINE 206 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3155 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 207 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3160 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 208 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3165 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  SelectConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3174 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: ConstantExpr.SelectConstantExpr.lhs.sort"
                   {-# LINE 3179 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_ShuffleVectorConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ShuffleVectorConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: TypeEnv
              _lhsOsort :: SSortExpr
              _lhsOsexpr =
                  ({-# LINE 206 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3196 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 207 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3201 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 208 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 3206 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ShuffleVectorConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3215 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: ConstantExpr.ShuffleVectorConstantExpr.lhs.sort"
                   {-# LINE 3220 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_ConstantExpr_UnaryConstantExpr :: String ->
                                      Int ->
                                      T_Value ->
                                      T_Type ->
                                      T_ConstantExpr
sem_ConstantExpr_UnaryConstantExpr name_ op_ val_ ty_ =
    (\ _lhsImts
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
              _valOmts :: TypeEnv
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
                  ({-# LINE 211 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 3260 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 212 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3265 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 213 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3270 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 214 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3275 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 215 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 3280 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 216 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 3285 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  UnaryConstantExpr name_ op_ _valIself _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 3294 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3299 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 3304 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 3309 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _valIident,_valIisGlobal,_valImts,_valIpsexpr,_valIself,_valIsexpr,_valIsexprs,_valIsort,_valIvtype) =
                  val_ _valOmts _valOtn _valOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- ConstantFP --------------------------------------------------
-- cata
sem_ConstantFP :: ConstantFP ->
                  T_ConstantFP
sem_ConstantFP (ConstantFPDouble _dbv _ty) =
    (sem_ConstantFP_ConstantFPDouble _dbv (sem_Type _ty))
sem_ConstantFP (ConstantFPFloat _fpv _ty) =
    (sem_ConstantFP_ConstantFPFloat _fpv (sem_Type _ty))
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
                  ({-# LINE 155 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 3360 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 156 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3365 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 157 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3370 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 158 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3375 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 159 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 3380 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 160 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 3385 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantFPDouble dbv_ _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 3394 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
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
                  ({-# LINE 155 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 3422 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 156 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 3427 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 157 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3432 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 158 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 3437 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 159 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 3442 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 160 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 3447 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantFPFloat fpv_ _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 3456 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
sem_DefinitionTy (ConstantD) =
    (sem_DefinitionTy_ConstantD)
sem_DefinitionTy (ThreadLocal) =
    (sem_DefinitionTy_ThreadLocal)
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
sem_DefinitionTy_ConstantD :: T_DefinitionTy
sem_DefinitionTy_ConstantD =
    (let _lhsOself :: DefinitionTy
         _self =
             ConstantD
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_DefinitionTy_ThreadLocal :: T_DefinitionTy
sem_DefinitionTy_ThreadLocal =
    (let _lhsOself :: DefinitionTy
         _self =
             ThreadLocal
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
sem_FunAttr (Inlinehint) =
    (sem_FunAttr_Inlinehint)
sem_FunAttr (Naked) =
    (sem_FunAttr_Naked)
sem_FunAttr (Noimplicitfloat) =
    (sem_FunAttr_Noimplicitfloat)
sem_FunAttr (Noinline) =
    (sem_FunAttr_Noinline)
sem_FunAttr (Nonlazybind) =
    (sem_FunAttr_Nonlazybind)
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
sem_FunAttr_Nonlazybind :: T_FunAttr
sem_FunAttr_Nonlazybind =
    (let _lhsOself :: FunAttr
         _self =
             Nonlazybind
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
sem_Function (FunctionDecl _name _linkage _retty _params) =
    (sem_Function_FunctionDecl (sem_Identifier _name) (sem_Linkage _linkage) (sem_Type _retty) (sem_Parameters _params))
sem_Function (FunctionDef _name _linkage _retty _params _body) =
    (sem_Function_FunctionDef (sem_Identifier _name) (sem_Linkage _linkage) (sem_Type _retty) (sem_Parameters _params) (sem_BasicBlocks _body))
-- semantic domain
type T_Function = CF ->
                  (Map.Map String PC) ->
                  ([[(SExpr, Maybe SExpr)]]) ->
                  (Map.Map String [PC]) ->
                  PreEncoder ->
                  (Int -> SExpr) ->
                  Id ->
                  ( Function,([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr]))
data Inh_Function = Inh_Function {cfg_Inh_Function :: CF,cte_Inh_Function :: (Map.Map String PC),mutexes_Inh_Function :: ([[(SExpr, Maybe SExpr)]]),pcs_Inh_Function :: (Map.Map String [PC]),prenc_Inh_Function :: PreEncoder,spark_Inh_Function :: (Int -> SExpr),tn_Inh_Function :: Id}
data Syn_Function = Syn_Function {self_Syn_Function :: Function,ts_Syn_Function :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])}
wrap_Function :: T_Function ->
                 Inh_Function ->
                 Syn_Function
wrap_Function sem (Inh_Function _lhsIcfg _lhsIcte _lhsImutexes _lhsIpcs _lhsIprenc _lhsIspark _lhsItn) =
    (let ( _lhsOself,_lhsOts) = sem _lhsIcfg _lhsIcte _lhsImutexes _lhsIpcs _lhsIprenc _lhsIspark _lhsItn
     in  (Syn_Function _lhsOself _lhsOts))
sem_Function_FunctionDecl :: T_Identifier ->
                             T_Linkage ->
                             T_Type ->
                             T_Parameters ->
                             T_Function
sem_Function_FunctionDecl name_ linkage_ retty_ params_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Function
              _nameOsortexpr :: (Maybe SSortExpr)
              _nameOtn :: String
              _rettyOmn :: (Maybe SSort)
              _rettyOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 3854 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FunctionDecl _nameIself _linkageIself _rettyIself _paramsIself
              _lhsOself =
                  _self
              _nameOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Function.FunctionDecl.name.sortexpr"
                   {-# LINE 3863 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nameOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 3868 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rettyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Function.FunctionDecl.retty.mn"
                   {-# LINE 3873 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rettyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Function.FunctionDecl.retty.mts"
                   {-# LINE 3878 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _nameIdeclexpr,_nameIident,_nameIself,_nameIsexpr,_nameIssymbol) =
                  name_ _nameOsortexpr _nameOtn
              ( _linkageIself) =
                  linkage_
              ( _rettyImts,_rettyIself,_rettyIsexprs,_rettyIsort,_rettyIsortn) =
                  retty_ _rettyOmn _rettyOmts
              ( _paramsIself) =
                  params_
          in  ( _lhsOself,_lhsOts)))
sem_Function_FunctionDef :: T_Identifier ->
                            T_Linkage ->
                            T_Type ->
                            T_Parameters ->
                            T_BasicBlocks ->
                            T_Function
sem_Function_FunctionDef name_ linkage_ retty_ params_ body_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Function
              _nameOsortexpr :: (Maybe SSortExpr)
              _nameOtn :: String
              _rettyOmn :: (Maybe SSort)
              _rettyOmts :: TypeEnv
              _bodyOcfg :: CF
              _bodyOcte :: (Map.Map String PC)
              _bodyOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _bodyOpcs :: (Map.Map String [PC])
              _bodyOprenc :: PreEncoder
              _bodyOspark :: (Int -> SExpr)
              _bodyOtn :: Id
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _bodyIts
                   {-# LINE 3933 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FunctionDef _nameIself _linkageIself _rettyIself _paramsIself _bodyIself
              _lhsOself =
                  _self
              _nameOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Function.FunctionDef.name.sortexpr"
                   {-# LINE 3942 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nameOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 3947 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rettyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Function.FunctionDef.retty.mn"
                   {-# LINE 3952 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rettyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Function.FunctionDef.retty.mts"
                   {-# LINE 3957 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOcfg =
                  ({-# LINE 62 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 3962 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOcte =
                  ({-# LINE 63 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 3967 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOmutexes =
                  ({-# LINE 68 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 3972 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOpcs =
                  ({-# LINE 64 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIpcs
                   {-# LINE 3977 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOprenc =
                  ({-# LINE 66 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 3982 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOspark =
                  ({-# LINE 67 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIspark
                   {-# LINE 3987 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _bodyOtn =
                  ({-# LINE 65 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 3992 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _nameIdeclexpr,_nameIident,_nameIself,_nameIsexpr,_nameIssymbol) =
                  name_ _nameOsortexpr _nameOtn
              ( _linkageIself) =
                  linkage_
              ( _rettyImts,_rettyIself,_rettyIsexprs,_rettyIsort,_rettyIsortn) =
                  retty_ _rettyOmn _rettyOmts
              ( _paramsIself) =
                  params_
              ( _bodyIself,_bodyIts) =
                  body_ _bodyOcfg _bodyOcte _bodyOmutexes _bodyOpcs _bodyOprenc _bodyOspark _bodyOtn
          in  ( _lhsOself,_lhsOts)))
-- Functions ---------------------------------------------------
-- cata
sem_Functions :: Functions ->
                 T_Functions
sem_Functions m =
    (Data.Map.foldrWithKey sem_Functions_Entry sem_Functions_Nil (Data.Map.map sem_Function m))
-- semantic domain
type T_Functions = (Map.Map String CF) ->
                   (Map.Map String PC) ->
                   ([[(SExpr, Maybe SExpr)]]) ->
                   PreEncoder ->
                   ( Functions,([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr]))
data Inh_Functions = Inh_Functions {cfg_Inh_Functions :: (Map.Map String CF),cte_Inh_Functions :: (Map.Map String PC),mutexes_Inh_Functions :: ([[(SExpr, Maybe SExpr)]]),prenc_Inh_Functions :: PreEncoder}
data Syn_Functions = Syn_Functions {self_Syn_Functions :: Functions,ts_Syn_Functions :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])}
wrap_Functions :: T_Functions ->
                  Inh_Functions ->
                  Syn_Functions
wrap_Functions sem (Inh_Functions _lhsIcfg _lhsIcte _lhsImutexes _lhsIprenc) =
    (let ( _lhsOself,_lhsOts) = sem _lhsIcfg _lhsIcte _lhsImutexes _lhsIprenc
     in  (Syn_Functions _lhsOself _lhsOts))
sem_Functions_Entry :: String ->
                       T_Function ->
                       T_Functions ->
                       T_Functions
sem_Functions_Entry key_ val_ tl_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIprenc ->
         (let _valOprenc :: PreEncoder
              _valOcfg :: CF
              _valOtn :: Id
              _valOspark :: (Int -> SExpr)
              _valOcte :: (Map.Map String PC)
              _valOpcs :: (Map.Map String [PC])
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Functions
              _valOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _tlOcfg :: (Map.Map String CF)
              _tlOcte :: (Map.Map String PC)
              _tlOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _tlOprenc :: PreEncoder
              _valIself :: Function
              _valIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _tlIself :: Functions
              _tlIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _valOprenc =
                  ({-# LINE 53 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 4054 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOcfg =
                  ({-# LINE 54 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fromMaybe (error $ "no cfg for " ++ show key_) $ Map.lookup key_ _lhsIcfg
                   {-# LINE 4059 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOtn =
                  ({-# LINE 55 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   key_
                   {-# LINE 4064 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOspark =
                  ({-# LINE 56 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \i -> IdentExpr $ SymIdent $ SimpleSym $ key_ ++ show i
                   {-# LINE 4069 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOcte =
                  ({-# LINE 57 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Map.delete key_ _lhsIcte
                   {-# LINE 4074 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valOpcs =
                  ({-# LINE 58 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Map.map (\cf -> nub $ Prelude.map snd cf) $ Map.delete key_ _lhsIcfg
                   {-# LINE 4079 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 49 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _valIts ++ _tlIts
                   {-# LINE 4084 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Data.Map.insert key_ _valIself _tlIself
              _lhsOself =
                  _self
              _valOmutexes =
                  ({-# LINE 68 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 4093 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOcfg =
                  ({-# LINE 46 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 4098 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOcte =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 4103 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmutexes =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 4108 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOprenc =
                  ({-# LINE 45 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 4113 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _valIself,_valIts) =
                  val_ _valOcfg _valOcte _valOmutexes _valOpcs _valOprenc _valOspark _valOtn
              ( _tlIself,_tlIts) =
                  tl_ _tlOcfg _tlOcte _tlOmutexes _tlOprenc
          in  ( _lhsOself,_lhsOts)))
sem_Functions_Nil :: T_Functions
sem_Functions_Nil =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIprenc ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Functions
              _lhsOts =
                  ({-# LINE 49 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 4131 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                ( GlobalState,Global,([SExpr]),SExpressions)
data Inh_Global = Inh_Global {gs_Inh_Global :: GlobalState}
data Syn_Global = Syn_Global {gs_Syn_Global :: GlobalState,self_Syn_Global :: Global,sexpr_Syn_Global :: ([SExpr]),sexprs_Syn_Global :: SExpressions}
wrap_Global :: T_Global ->
               Inh_Global ->
               Syn_Global
wrap_Global sem (Inh_Global _lhsIgs) =
    (let ( _lhsOgs,_lhsOself,_lhsOsexpr,_lhsOsexprs) = sem _lhsIgs
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
    (\ _lhsIgs ->
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
                  ({-# LINE 39 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   defsorts _lhsIgs
                   {-# LINE 4220 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 40 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   Nothing
                   {-# LINE 4225 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOmts =
                  ({-# LINE 41 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   _tyImts
                   {-# LINE 4230 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOtn =
                  ({-# LINE 42 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   ""
                   {-# LINE 4235 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 43 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   _tyIsexprs ++ _ivalIsexprs ++ [ declfun _sym     _tyIsort , declfun _psym     (SymSort "I32") ]
                   {-# LINE 4240 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   let v = IdentExpr $ IdxIdent (bv 0) [32]
                   in Prelude.map (\ve -> sFn "=" (IdentExpr $ SymIdent $ _sym    ) ve `sAnd` sFn "=" (IdentExpr $ SymIdent $ _psym    ) v) _ivalIsexpr
                   {-# LINE 4246 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sym =
                  ({-# LINE 46 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   SimpleSym _rawname
                   {-# LINE 4251 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _psym =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   SimpleSym $ "l" ++ _rawname
                   {-# LINE 4256 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOgs =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   let ogs@GlobalState{..} = _lhsIgs
                       gvals' = maybe gvals (\v -> Map.insert _rawname     (Right v) gvals) $ Constant <$> _ivalIself
                   in ogs { defsorts = _ivalImts, gvals = gvals' }
                   {-# LINE 4263 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rawname =
                  ({-# LINE 51 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   getIdName _nameIself
                   {-# LINE 4268 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  GlobalVar _nameIself _linkageIself isConst_ isUaddr_ _tyIself _ivalIself _alignIself
              _lhsOself =
                  _self
              _nameOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Global.GlobalVar.name.sortexpr"
                   {-# LINE 4277 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nameOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Global.GlobalVar.name.tn"
                   {-# LINE 4282 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOval =
                  ({-# LINE 16 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Global.GlobalVar.ival.val"
                   {-# LINE 4287 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _nameIdeclexpr,_nameIident,_nameIself,_nameIsexpr,_nameIssymbol) =
                  name_ _nameOsortexpr _nameOtn
              ( _linkageIself) =
                  linkage_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _ivalImts,_ivalIself,_ivalIsexpr,_ivalIsexprs) =
                  ival_ _ivalOmts _ivalOtn _ivalOval
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
                     String ->
                     (Map.Map Id (Type, [PC])) ->
                     ( (Maybe String),Bool,TypeEnv,(Int -> [SExpr]),GlobalValue,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_GlobalValue = Inh_GlobalValue {mts_Inh_GlobalValue :: TypeEnv,tn_Inh_GlobalValue :: String,val_Inh_GlobalValue :: (Map.Map Id (Type, [PC]))}
data Syn_GlobalValue = Syn_GlobalValue {ident_Syn_GlobalValue :: (Maybe String),isGlobal_Syn_GlobalValue :: Bool,mts_Syn_GlobalValue :: TypeEnv,psexpr_Syn_GlobalValue :: (Int -> [SExpr]),self_Syn_GlobalValue :: GlobalValue,sexpr_Syn_GlobalValue :: ([SExpr]),sexprs_Syn_GlobalValue :: SExpressions,sort_Syn_GlobalValue :: SSortExpr,vtype_Syn_GlobalValue :: Type}
wrap_GlobalValue :: T_GlobalValue ->
                    Inh_GlobalValue ->
                    Syn_GlobalValue
wrap_GlobalValue sem (Inh_GlobalValue _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_GlobalValue _lhsOident _lhsOisGlobal _lhsOmts _lhsOpsexpr _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_GlobalValue_FunctionValue :: T_Identifier ->
                                 T_Type ->
                                 T_GlobalValue
sem_GlobalValue_FunctionValue n_ ty_ =
    (\ _lhsImts
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
                  ({-# LINE 164 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4356 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 165 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 4361 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 166 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 4366 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sexpr' =
                  ({-# LINE 167 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   case Map.lookup _nIident _lhsIval of
                     Nothing -> []
                     Just (_,l) -> [ IdentExpr $ SymIdent $ SimpleSym $ _nIident ++ show n | n <- [0..(length l)-1]]
                   {-# LINE 4373 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 170 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   [ IdentExpr $ SymIdent _nIssymbol ] ++ _sexpr'
                   {-# LINE 4378 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 171 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 4383 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 172 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   \i -> let pi = IdentExpr $ SymIdent $ SimpleSym $ "p" ++ _nIident ++ show i
                             z  = IdentExpr $ SymIdent $ SimpleSym $ "l" ++ _nIident
                         in case Map.lookup _nIident _lhsIval of
                              Nothing    -> [ sFn "=" (IdentExpr $ SymIdent $ SimpleSym $ "l" ++ _nIident) z ]
                              Just (_,l) -> [ sFn "=" pi z ] ++ Prelude.map (\v -> sFn "=" pi $ IdentExpr $ IdxIdent (bv v) [32]) l
                   {-# LINE 4392 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 177 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 4397 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOisGlobal =
                  ({-# LINE 178 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   True
                   {-# LINE 4402 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 179 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Just _nIident
                   {-# LINE 4407 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 180 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 4412 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FunctionValue _nIself _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 4421 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: GlobalValue.FunctionValue.n.sortexpr"
                   {-# LINE 4426 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                  ({-# LINE 164 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4466 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 165 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 4471 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 166 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 4476 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sexpr' =
                  ({-# LINE 167 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   case Map.lookup _nIident _lhsIval of
                     Nothing -> []
                     Just (_,l) -> [ IdentExpr $ SymIdent $ SimpleSym $ _nIident ++ show n | n <- [0..(length l)-1]]
                   {-# LINE 4483 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 170 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   [ IdentExpr $ SymIdent _nIssymbol ] ++ _sexpr'
                   {-# LINE 4488 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 171 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 4493 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 172 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   \i -> let pi = IdentExpr $ SymIdent $ SimpleSym $ "p" ++ _nIident ++ show i
                             z  = IdentExpr $ SymIdent $ SimpleSym $ "l" ++ _nIident
                         in case Map.lookup _nIident _lhsIval of
                              Nothing    -> [ sFn "=" (IdentExpr $ SymIdent $ SimpleSym $ "l" ++ _nIident) z ]
                              Just (_,l) -> [ sFn "=" pi z ] ++ Prelude.map (\v -> sFn "=" pi $ IdentExpr $ IdxIdent (bv v) [32]) l
                   {-# LINE 4502 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 177 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 4507 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOisGlobal =
                  ({-# LINE 178 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   True
                   {-# LINE 4512 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 179 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Just _nIident
                   {-# LINE 4517 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 180 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 4522 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  GlobalAlias _nIself _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 4531 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: GlobalValue.GlobalAlias.n.sortexpr"
                   {-# LINE 4536 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                  ({-# LINE 164 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4576 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 165 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 4581 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 166 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 4586 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sexpr' =
                  ({-# LINE 167 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   case Map.lookup _nIident _lhsIval of
                     Nothing -> []
                     Just (_,l) -> [ IdentExpr $ SymIdent $ SimpleSym $ _nIident ++ show n | n <- [0..(length l)-1]]
                   {-# LINE 4593 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 170 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   [ IdentExpr $ SymIdent _nIssymbol ] ++ _sexpr'
                   {-# LINE 4598 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 171 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 4603 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 172 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   \i -> let pi = IdentExpr $ SymIdent $ SimpleSym $ "p" ++ _nIident ++ show i
                             z  = IdentExpr $ SymIdent $ SimpleSym $ "l" ++ _nIident
                         in case Map.lookup _nIident _lhsIval of
                              Nothing    -> [ sFn "=" (IdentExpr $ SymIdent $ SimpleSym $ "l" ++ _nIident) z ]
                              Just (_,l) -> [ sFn "=" pi z ] ++ Prelude.map (\v -> sFn "=" pi $ IdentExpr $ IdxIdent (bv v) [32]) l
                   {-# LINE 4612 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 177 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 4617 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOisGlobal =
                  ({-# LINE 178 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   True
                   {-# LINE 4622 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 179 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Just _nIident
                   {-# LINE 4627 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 180 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 4632 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  GlobalVariable _nIself _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 4641 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: GlobalValue.GlobalVariable.n.sortexpr"
                   {-# LINE 4646 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                 ( GlobalState,Globals,([SExpr]),SExpressions)
data Inh_Globals = Inh_Globals {gs_Inh_Globals :: GlobalState}
data Syn_Globals = Syn_Globals {gs_Syn_Globals :: GlobalState,self_Syn_Globals :: Globals,sexpr_Syn_Globals :: ([SExpr]),sexprs_Syn_Globals :: SExpressions}
wrap_Globals :: T_Globals ->
                Inh_Globals ->
                Syn_Globals
wrap_Globals sem (Inh_Globals _lhsIgs) =
    (let ( _lhsOgs,_lhsOself,_lhsOsexpr,_lhsOsexprs) = sem _lhsIgs
     in  (Syn_Globals _lhsOgs _lhsOself _lhsOsexpr _lhsOsexprs))
sem_Globals_Cons :: T_Global ->
                    T_Globals ->
                    T_Globals
sem_Globals_Cons hd_ tl_ =
    (\ _lhsIgs ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOself :: Globals
              _lhsOgs :: GlobalState
              _hdOgs :: GlobalState
              _tlOgs :: GlobalState
              _hdIgs :: GlobalState
              _hdIself :: Global
              _hdIsexpr :: ([SExpr])
              _hdIsexprs :: SExpressions
              _tlIgs :: GlobalState
              _tlIself :: Globals
              _tlIsexpr :: ([SExpr])
              _tlIsexprs :: SExpressions
              _lhsOsexpr =
                  ({-# LINE 26 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   let l = _hdIsexpr ++ _tlIsexpr
                   in  if l == []
                       then []
                       else [ wrap sAnd l ]
                   {-# LINE 4695 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 30 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   nub $  _hdIsexprs ++ _tlIsexprs
                   {-# LINE 4700 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOgs =
                  ({-# LINE 20 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   _tlIgs
                   {-# LINE 4709 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOgs =
                  ({-# LINE 35 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   _lhsIgs
                   {-# LINE 4714 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOgs =
                  ({-# LINE 20 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   _hdIgs
                   {-# LINE 4719 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIgs,_hdIself,_hdIsexpr,_hdIsexprs) =
                  hd_ _hdOgs
              ( _tlIgs,_tlIself,_tlIsexpr,_tlIsexprs) =
                  tl_ _tlOgs
          in  ( _lhsOgs,_lhsOself,_lhsOsexpr,_lhsOsexprs)))
sem_Globals_Nil :: T_Globals
sem_Globals_Nil =
    (\ _lhsIgs ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOself :: Globals
              _lhsOgs :: GlobalState
              _lhsOsexpr =
                  ({-# LINE 24 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   []
                   {-# LINE 4736 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   []
                   {-# LINE 4741 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOgs =
                  ({-# LINE 20 "src/Concurrent/Model/Encoder/Global.ag" #-}
                   _lhsIgs
                   {-# LINE 4750 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _sym
                   {-# LINE 4812 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 22 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _nameIself
                   {-# LINE 4817 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 23 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   IdentExpr $ SymIdent _sym
                   {-# LINE 4822 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOdeclexpr =
                  ({-# LINE 24 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   maybe [] (\se -> [declfun _sym     se]) _lhsIsortexpr
                   {-# LINE 4827 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sym =
                  ({-# LINE 25 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   SimpleSym _nameIself
                   {-# LINE 4832 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                  ({-# LINE 27 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _sym
                   {-# LINE 4855 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 28 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn ++ _nameIself
                   {-# LINE 4860 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 29 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   IdentExpr $ SymIdent _sym
                   {-# LINE 4865 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOdeclexpr =
                  ({-# LINE 30 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   if _nameIself == ""
                   then []
                   else maybe [] (\se -> [declfun _sym     se]) _lhsIsortexpr
                   {-# LINE 4872 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sym =
                  ({-# LINE 33 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   SimpleSym $ _lhsItn ++ _nameIself
                   {-# LINE 4877 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
type T_Identifiers = ( Identifiers)
data Inh_Identifiers = Inh_Identifiers {}
data Syn_Identifiers = Syn_Identifiers {self_Syn_Identifiers :: Identifiers}
wrap_Identifiers :: T_Identifiers ->
                    Inh_Identifiers ->
                    Syn_Identifiers
wrap_Identifiers sem (Inh_Identifiers) =
    (let ( _lhsOself) = sem
     in  (Syn_Identifiers _lhsOself))
sem_Identifiers_Cons :: T_Identifier ->
                        T_Identifiers ->
                        T_Identifiers
sem_Identifiers_Cons hd_ tl_ =
    (let _lhsOself :: Identifiers
         _hdOsortexpr :: (Maybe SSortExpr)
         _hdOtn :: String
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
             ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
              error "missing rule: Identifiers.Cons.hd.sortexpr"
              {-# LINE 4922 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _hdOtn =
             ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
              error "missing rule: Identifiers.Cons.hd.tn"
              {-# LINE 4927 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         ( _hdIdeclexpr,_hdIident,_hdIself,_hdIsexpr,_hdIssymbol) =
             hd_ _hdOsortexpr _hdOtn
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Identifiers_Nil :: T_Identifiers
sem_Identifiers_Nil =
    (let _lhsOself :: Identifiers
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Instruction -------------------------------------------------
-- cata
sem_Instruction :: Instruction ->
                   T_Instruction
sem_Instruction (AShr _pc _id _ty _op1 _op2) =
    (sem_Instruction_AShr (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Add _pc _id _ty _op1 _op2) =
    (sem_Instruction_Add (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Alloca _pc _id _ty _align) =
    (sem_Instruction_Alloca (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Align _align))
sem_Instruction (And _pc _id _ty _op1 _op2) =
    (sem_Instruction_And (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (AtomicRMW _pc _id _args _op _ord) =
    (sem_Instruction_AtomicRMW (sem_PC _pc) (sem_Identifier _id) (sem_Values _args) (sem_BinOp _op) (sem_AtomicOrdering _ord))
sem_Instruction (BitCast _pc _id _v _ty) =
    (sem_Instruction_BitCast (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (Br _pc _v _t _f) =
    (sem_Instruction_Br (sem_PC _pc) (sem_Value _v) (sem_Value _t) (sem_Value _f))
sem_Instruction (Call _pc _mres _ty _callee _args) =
    (sem_Instruction_Call (sem_PC _pc) (sem_Identifier _mres) (sem_Type _ty) (sem_Identifier _callee) (sem_Values _args))
sem_Instruction (Cmpxchg _pc _id _mptr _cval _nval _ord) =
    (sem_Instruction_Cmpxchg (sem_PC _pc) (sem_Identifier _id) (sem_Value _mptr) (sem_Value _cval) (sem_Value _nval) (sem_AtomicOrdering _ord))
sem_Instruction (CreateThread _pc _args) =
    (sem_Instruction_CreateThread (sem_PC _pc) (sem_Values _args))
sem_Instruction (ExtractValue _pc _id _aggr _idxs) =
    (sem_Instruction_ExtractValue (sem_PC _pc) (sem_Identifier _id) (sem_Value _aggr) (sem_Ints _idxs))
sem_Instruction (FAdd _pc _id _ty _op1 _op2) =
    (sem_Instruction_FAdd (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FCmp _pc _id _cond _ty _op1 _op2) =
    (sem_Instruction_FCmp (sem_PC _pc) (sem_Identifier _id) (sem_RealPredicate _cond) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FDiv _pc _id _ty _op1 _op2) =
    (sem_Instruction_FDiv (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FMul _pc _id _ty _op1 _op2) =
    (sem_Instruction_FMul (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FPExt _pc _id _v _ty) =
    (sem_Instruction_FPExt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (FPToSI _pc _id _v _ty) =
    (sem_Instruction_FPToSI (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (FPToUI _pc _id _v _ty) =
    (sem_Instruction_FPToUI (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (FPTrunc _pc _id _v _ty) =
    (sem_Instruction_FPTrunc (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (FRem _pc _id _ty _op1 _op2) =
    (sem_Instruction_FRem (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FSub _pc _id _ty _op1 _op2) =
    (sem_Instruction_FSub (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (GetElementPtr _pc _id _ty _struct _idxs) =
    (sem_Instruction_GetElementPtr (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _struct) (sem_Values _idxs))
sem_Instruction (ICmp _pc _id _cond _ty _op1 _op2) =
    (sem_Instruction_ICmp (sem_PC _pc) (sem_Identifier _id) (sem_IntPredicate _cond) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (InsertValue _pc _id _aggr _ival _idxs) =
    (sem_Instruction_InsertValue (sem_PC _pc) (sem_Identifier _id) (sem_Value _aggr) (sem_Value _ival) (sem_Ints _idxs))
sem_Instruction (IntToPtr _pc _id _v _ty) =
    (sem_Instruction_IntToPtr (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (LShr _pc _id _ty _op1 _op2) =
    (sem_Instruction_LShr (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Load _pc _id _v _align) =
    (sem_Instruction_Load (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Align _align))
sem_Instruction (Mul _pc _id _ty _op1 _op2) =
    (sem_Instruction_Mul (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (MutexInit _pc _rv _mutex) =
    (sem_Instruction_MutexInit (sem_PC _pc) (sem_Identifier _rv) (sem_Value _mutex))
sem_Instruction (MutexLock _pc _rv _mutex) =
    (sem_Instruction_MutexLock (sem_PC _pc) (sem_Identifier _rv) (sem_Value _mutex))
sem_Instruction (MutexUnlock _pc _rv _mutex) =
    (sem_Instruction_MutexUnlock (sem_PC _pc) (sem_Identifier _rv) (sem_Value _mutex))
sem_Instruction (NotifyEvent _pc _event) =
    (sem_Instruction_NotifyEvent (sem_PC _pc) _event)
sem_Instruction (Or _pc _id _ty _op1 _op2) =
    (sem_Instruction_Or (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (PHI _pc _id _ty _vals) =
    (sem_Instruction_PHI (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_PValues _vals))
sem_Instruction (PtrToInt _pc _id _v _ty) =
    (sem_Instruction_PtrToInt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (Ret _pc _r) =
    (sem_Instruction_Ret (sem_PC _pc) (sem_RetInst _r))
sem_Instruction (SDiv _pc _id _ty _op1 _op2) =
    (sem_Instruction_SDiv (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (SExt _pc _id _v _ty) =
    (sem_Instruction_SExt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (SIToFP _pc _id _v _ty) =
    (sem_Instruction_SIToFP (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (SRem _pc _id _ty _op1 _op2) =
    (sem_Instruction_SRem (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Select _pc _id _cond _valt _valf) =
    (sem_Instruction_Select (sem_PC _pc) (sem_Identifier _id) (sem_Value _cond) (sem_Value _valt) (sem_Value _valf))
sem_Instruction (Shl _pc _id _ty _op1 _op2) =
    (sem_Instruction_Shl (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Store _pc _ty _v1 _v2 _align) =
    (sem_Instruction_Store (sem_PC _pc) (sem_Type _ty) (sem_Value _v1) (sem_Value _v2) (sem_Align _align))
sem_Instruction (Sub _pc _id _ty _op1 _op2) =
    (sem_Instruction_Sub (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Switch _pc _elems) =
    (sem_Instruction_Switch (sem_PC _pc) (sem_IntTyValIdL _elems))
sem_Instruction (Trunc _pc _id _v _ty) =
    (sem_Instruction_Trunc (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (UBr _pc _d) =
    (sem_Instruction_UBr (sem_PC _pc) (sem_Value _d))
sem_Instruction (UDiv _pc _id _ty _op1 _op2) =
    (sem_Instruction_UDiv (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (UIToFP _pc _id _v _ty) =
    (sem_Instruction_UIToFP (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (URem _pc _id _ty _op1 _op2) =
    (sem_Instruction_URem (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Unreachable _pc) =
    (sem_Instruction_Unreachable (sem_PC _pc))
sem_Instruction (WaitEvent _pc _event) =
    (sem_Instruction_WaitEvent (sem_PC _pc) _event)
sem_Instruction (WaitTime _pc _time) =
    (sem_Instruction_WaitTime (sem_PC _pc) (sem_Value _time))
sem_Instruction (Xor _pc _id _ty _op1 _op2) =
    (sem_Instruction_Xor (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (ZExt _pc _id _v _ty) =
    (sem_Instruction_ZExt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
-- semantic domain
type T_Instruction = CF ->
                     (Map.Map String PC) ->
                     ([[(SExpr, Maybe SExpr)]]) ->
                     (Map.Map String [PC]) ->
                     PreEncoder ->
                     (Int -> SExpr) ->
                     Id ->
                     ( Instruction,([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr]))
data Inh_Instruction = Inh_Instruction {cfg_Inh_Instruction :: CF,cte_Inh_Instruction :: (Map.Map String PC),mutexes_Inh_Instruction :: ([[(SExpr, Maybe SExpr)]]),pcs_Inh_Instruction :: (Map.Map String [PC]),prenc_Inh_Instruction :: PreEncoder,spark_Inh_Instruction :: (Int -> SExpr),tn_Inh_Instruction :: Id}
data Syn_Instruction = Syn_Instruction {self_Syn_Instruction :: Instruction,ts_Syn_Instruction :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])}
wrap_Instruction :: T_Instruction ->
                    Inh_Instruction ->
                    Syn_Instruction
wrap_Instruction sem (Inh_Instruction _lhsIcfg _lhsIcte _lhsImutexes _lhsIpcs _lhsIprenc _lhsIspark _lhsItn) =
    (let ( _lhsOself,_lhsOts) = sem _lhsIcfg _lhsIcte _lhsImutexes _lhsIpcs _lhsIprenc _lhsIspark _lhsItn
     in  (Syn_Instruction _lhsOself _lhsOts))
sem_Instruction_AShr :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_AShr pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 5131 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  AShr _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.AShr.id.sortexpr"
                   {-# LINE 5140 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5145 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.AShr.ty.mn"
                   {-# LINE 5150 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.AShr.ty.mts"
                   {-# LINE 5155 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 5160 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5165 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.AShr.op1.val"
                   {-# LINE 5170 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 5175 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5180 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.AShr.op2.val"
                   {-# LINE 5185 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
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
                  ({-# LINE 137 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 5256 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 138 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 5261 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 139 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 5266 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 140 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 5271 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 141 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 5276 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 142 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 5281 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 143 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 5286 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 144 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 5291 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 145 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ Prelude.map (\se -> sFn "=" _idIsexpr se) _vse
                   {-# LINE 5296 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 146 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "bin instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 5307 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vse =
                  ({-# LINE 154 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ sFn "bvadd" e1 e2 | e1 <- _op1Isexpr, e2 <- _op2Isexpr ]
                   {-# LINE 5312 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 5320 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 5325 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 5330 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 5335 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 5342 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 5347 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Add _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Add.ty.mn"
                   {-# LINE 5356 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Add.ty.mts"
                   {-# LINE 5361 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Alloca :: T_PC ->
                          T_Identifier ->
                          T_Type ->
                          T_Align ->
                          T_Instruction
sem_Instruction_Alloca pc_ id_ ty_ align_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 5408 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Alloca _pcIself _idIself _tyIself _alignIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.Alloca.id.sortexpr"
                   {-# LINE 5417 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5422 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Alloca.ty.mn"
                   {-# LINE 5427 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Alloca.ty.mts"
                   {-# LINE 5432 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
sem_Instruction_And :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_And pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
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
                  ({-# LINE 137 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 5501 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 138 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 5506 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 139 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 5511 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 140 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 5516 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 141 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 5521 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 142 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 5526 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 143 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 5531 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 144 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 5536 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 145 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ Prelude.map (\se -> sFn "=" _idIsexpr se) _vse
                   {-# LINE 5541 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 146 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "bin instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 5552 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vse =
                  ({-# LINE 160 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ sFn "and" e1 e2 | e1 <- _op1Isexpr, e2 <- _op2Isexpr ]
                   {-# LINE 5557 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 5565 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 5570 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 5575 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 5580 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 5587 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 5592 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  And _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.And.ty.mn"
                   {-# LINE 5601 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.And.ty.mts"
                   {-# LINE 5606 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _argsOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 5656 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  AtomicRMW _pcIself _idIself _argsIself _opIself _ordIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.AtomicRMW.id.sortexpr"
                   {-# LINE 5665 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5670 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOmts =
                  ({-# LINE 27 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.AtomicRMW.args.mts"
                   {-# LINE 5675 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOtn =
                  ({-# LINE 29 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5680 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOval =
                  ({-# LINE 28 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.AtomicRMW.args.val"
                   {-# LINE 5685 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _argsImts,_argsIself,_argsIsexpr,_argsIsexprs,_argsIvtype) =
                  args_ _argsOmts _argsOtn _argsOval
              ( _opIself) =
                  op_
              ( _ordIself) =
                  ord_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_BitCast :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           T_Type ->
                           T_Instruction
sem_Instruction_BitCast pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _vOmts :: TypeEnv
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
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
                  ({-# LINE 181 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 5743 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 182 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 5748 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 183 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 5753 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 184 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 5758 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 185 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 5763 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 186 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   if getISize _tyIself <= getISize _vIvtype
                   then wrap sOr $ Prelude.map (\e -> sFn "=" _idIsexpr $ ExtractExpr [ IdentExpr $ IdxIdent (SimpleSym "extract") [(getISize _tyIself)-1, 0] , e ]) _vIsexpr
                   else let n = getISize _tyIself - getISize _vIvtype
                        in  wrap sOr $ Prelude.map (\e -> sFn "=" _idIsexpr $ ZeroExtExpr e n) _vIsexpr
                   {-# LINE 5771 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 190 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                         in case npce of
                                             Nothing -> _lhsIspark k `sAnd` iexp
                                             Just e  -> let fnpce = if _npcev     == [] then error "Bitcast instruction" else sFn "=" e $ head _npcev
                                                        in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 5782 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 5790 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 5795 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 5800 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 5805 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 5812 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 5817 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  BitCast _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.BitCast.ty.mn"
                   {-# LINE 5826 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 5831 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Br :: T_PC ->
                      T_Value ->
                      T_Value ->
                      T_Value ->
                      T_Instruction
sem_Instruction_Br pc_ v_ t_ f_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _vOmts :: TypeEnv
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _tOmts :: TypeEnv
              _tOtn :: String
              _tOval :: (Map.Map Id (Type, [PC]))
              _fOmts :: TypeEnv
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
                  ({-# LINE 170 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 5897 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 171 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 5902 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 172 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 5907 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 173 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp   = wrap sAnd $ fpce:preds
                                        in case npce of
                                           Nothing -> _lhsIspark k `sAnd` iexp
                                           Just e  -> let fnpce = wrap sOr $ [ (ve `sAnd` sFn "=" e (_npcev     !! 0) ) `sOr` (FnAppExpr (SymIdent $ SimpleSym "not") [ve] `sAnd` sFn "=" e (_npcev     !! 1)) | ve <- _vIsexpr ]
                                                      in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 5918 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 5926 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 5931 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 5936 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 5941 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 5948 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 5953 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Br _pcIself _vIself _tIself _fIself
              _lhsOself =
                  _self
              _tOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _vImts
                   {-# LINE 5962 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5967 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Br.t.val"
                   {-# LINE 5972 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tImts
                   {-# LINE 5977 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5982 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Br.f.val"
                   {-# LINE 5987 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tIident,_tIisGlobal,_tImts,_tIpsexpr,_tIself,_tIsexpr,_tIsexprs,_tIsort,_tIvtype) =
                  t_ _tOmts _tOtn _tOval
              ( _fIident,_fIisGlobal,_fImts,_fIpsexpr,_fIself,_fIsexpr,_fIsexprs,_fIsort,_fIvtype) =
                  f_ _fOmts _fOtn _fOval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _mresOsortexpr :: (Maybe SSortExpr)
              _mresOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _calleeOsortexpr :: (Maybe SSortExpr)
              _calleeOtn :: String
              _argsOmts :: TypeEnv
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
                  ({-# LINE 232 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:preds
                                        in case npce of
                                             Nothing -> _lhsIspark k `sAnd` fpce
                                             Just e  -> let fnpce = if _npcev     == [] then error "Call instruction" else sFn "=" e $ head _npcev
                                                        in  _lhsIspark k `sAnd` fpce `sAnd` fnpce
                   {-# LINE 6053 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 6061 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 6066 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 6071 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 6076 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 6083 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 6088 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Call _pcIself _mresIself _tyIself _calleeIself _argsIself
              _lhsOself =
                  _self
              _mresOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.Call.mres.sortexpr"
                   {-# LINE 6097 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mresOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6102 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Call.ty.mn"
                   {-# LINE 6107 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Call.ty.mts"
                   {-# LINE 6112 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _calleeOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.Call.callee.sortexpr"
                   {-# LINE 6117 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _calleeOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6122 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOmts =
                  ({-# LINE 27 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6127 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOtn =
                  ({-# LINE 29 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6132 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOval =
                  ({-# LINE 28 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Call.args.val"
                   {-# LINE 6137 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                  args_ _argsOmts _argsOtn _argsOval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _mptrOmts :: TypeEnv
              _mptrOtn :: String
              _mptrOval :: (Map.Map Id (Type, [PC]))
              _cvalOmts :: TypeEnv
              _cvalOtn :: String
              _cvalOval :: (Map.Map Id (Type, [PC]))
              _nvalOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 6215 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Cmpxchg _pcIself _idIself _mptrIself _cvalIself _nvalIself _ordIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.Cmpxchg.id.sortexpr"
                   {-# LINE 6224 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6229 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mptrOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Cmpxchg.mptr.mts"
                   {-# LINE 6234 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mptrOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6239 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mptrOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Cmpxchg.mptr.val"
                   {-# LINE 6244 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cvalOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _mptrImts
                   {-# LINE 6249 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cvalOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6254 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cvalOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Cmpxchg.cval.val"
                   {-# LINE 6259 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nvalOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cvalImts
                   {-# LINE 6264 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nvalOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6269 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nvalOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Cmpxchg.nval.val"
                   {-# LINE 6274 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _mptrIident,_mptrIisGlobal,_mptrImts,_mptrIpsexpr,_mptrIself,_mptrIsexpr,_mptrIsexprs,_mptrIsort,_mptrIvtype) =
                  mptr_ _mptrOmts _mptrOtn _mptrOval
              ( _cvalIident,_cvalIisGlobal,_cvalImts,_cvalIpsexpr,_cvalIself,_cvalIsexpr,_cvalIsexprs,_cvalIsort,_cvalIvtype) =
                  cval_ _cvalOmts _cvalOtn _cvalOval
              ( _nvalIident,_nvalIisGlobal,_nvalImts,_nvalIpsexpr,_nvalIself,_nvalIsexpr,_nvalIsexprs,_nvalIsort,_nvalIvtype) =
                  nval_ _nvalOmts _nvalOtn _nvalOval
              ( _ordIself) =
                  ord_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_CreateThread :: T_PC ->
                                T_Values ->
                                T_Instruction
sem_Instruction_CreateThread pc_ args_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _argsOmts :: TypeEnv
              _argsOtn :: String
              _argsOval :: (Map.Map Id (Type, [PC]))
              _pcIself :: PC
              _argsImts :: TypeEnv
              _argsIself :: Values
              _argsIsexpr :: ([SExpr])
              _argsIsexprs :: SExpressions
              _argsIvtype :: ([Type])
              _lhsOts =
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 6314 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  CreateThread _pcIself _argsIself
              _lhsOself =
                  _self
              _argsOmts =
                  ({-# LINE 27 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.CreateThread.args.mts"
                   {-# LINE 6323 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOtn =
                  ({-# LINE 29 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6328 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _argsOval =
                  ({-# LINE 28 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.CreateThread.args.val"
                   {-# LINE 6333 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _argsImts,_argsIself,_argsIsexpr,_argsIsexprs,_argsIvtype) =
                  args_ _argsOmts _argsOtn _argsOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_ExtractValue :: T_PC ->
                                T_Identifier ->
                                T_Value ->
                                T_Ints ->
                                T_Instruction
sem_Instruction_ExtractValue pc_ id_ aggr_ idxs_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _aggrOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 6379 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ExtractValue _pcIself _idIself _aggrIself _idxsIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.ExtractValue.id.sortexpr"
                   {-# LINE 6388 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6393 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.ExtractValue.aggr.mts"
                   {-# LINE 6398 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6403 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.ExtractValue.aggr.val"
                   {-# LINE 6408 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _aggrIident,_aggrIisGlobal,_aggrImts,_aggrIpsexpr,_aggrIself,_aggrIsexpr,_aggrIsexprs,_aggrIsort,_aggrIvtype) =
                  aggr_ _aggrOmts _aggrOtn _aggrOval
              ( _idxsIself) =
                  idxs_
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 6477 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FAdd _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.FAdd.id.sortexpr"
                   {-# LINE 6486 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6491 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FAdd.ty.mn"
                   {-# LINE 6496 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FAdd.ty.mts"
                   {-# LINE 6501 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6506 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6511 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FAdd.op1.val"
                   {-# LINE 6516 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 6521 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6526 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FAdd.op2.val"
                   {-# LINE 6531 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 6604 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FCmp _pcIself _idIself _condIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.FCmp.id.sortexpr"
                   {-# LINE 6613 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6618 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FCmp.ty.mn"
                   {-# LINE 6623 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FCmp.ty.mts"
                   {-# LINE 6628 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6633 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6638 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FCmp.op1.val"
                   {-# LINE 6643 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 6648 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6653 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FCmp.op2.val"
                   {-# LINE 6658 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 6731 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.FDiv.id.sortexpr"
                   {-# LINE 6740 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6745 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FDiv.ty.mn"
                   {-# LINE 6750 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FDiv.ty.mts"
                   {-# LINE 6755 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6760 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6765 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FDiv.op1.val"
                   {-# LINE 6770 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 6775 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6780 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FDiv.op2.val"
                   {-# LINE 6785 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 6856 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FMul _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.FMul.id.sortexpr"
                   {-# LINE 6865 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6870 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FMul.ty.mn"
                   {-# LINE 6875 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FMul.ty.mts"
                   {-# LINE 6880 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6885 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6890 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FMul.op1.val"
                   {-# LINE 6895 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 6900 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6905 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FMul.op2.val"
                   {-# LINE 6910 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_FPExt :: T_PC ->
                         T_Identifier ->
                         T_Value ->
                         T_Type ->
                         T_Instruction
sem_Instruction_FPExt pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 6968 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FPExt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.FPExt.id.sortexpr"
                   {-# LINE 6977 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6982 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FPExt.v.mts"
                   {-# LINE 6987 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6992 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FPExt.v.val"
                   {-# LINE 6997 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FPExt.ty.mn"
                   {-# LINE 7002 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 7007 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 7063 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FPToSI _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.FPToSI.id.sortexpr"
                   {-# LINE 7072 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7077 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FPToSI.v.mts"
                   {-# LINE 7082 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7087 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FPToSI.v.val"
                   {-# LINE 7092 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FPToSI.ty.mn"
                   {-# LINE 7097 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 7102 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 7158 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FPToUI _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.FPToUI.id.sortexpr"
                   {-# LINE 7167 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7172 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FPToUI.v.mts"
                   {-# LINE 7177 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7182 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FPToUI.v.val"
                   {-# LINE 7187 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FPToUI.ty.mn"
                   {-# LINE 7192 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 7197 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 7253 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FPTrunc _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.FPTrunc.id.sortexpr"
                   {-# LINE 7262 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7267 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FPTrunc.v.mts"
                   {-# LINE 7272 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7277 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FPTrunc.v.val"
                   {-# LINE 7282 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FPTrunc.ty.mn"
                   {-# LINE 7287 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 7292 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 7361 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FRem _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.FRem.id.sortexpr"
                   {-# LINE 7370 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7375 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FRem.ty.mn"
                   {-# LINE 7380 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FRem.ty.mts"
                   {-# LINE 7385 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 7390 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7395 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FRem.op1.val"
                   {-# LINE 7400 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 7405 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7410 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FRem.op2.val"
                   {-# LINE 7415 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 7486 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  FSub _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.FSub.id.sortexpr"
                   {-# LINE 7495 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7500 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FSub.ty.mn"
                   {-# LINE 7505 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.FSub.ty.mts"
                   {-# LINE 7510 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 7515 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7520 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FSub.op1.val"
                   {-# LINE 7525 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 7530 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7535 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.FSub.op2.val"
                   {-# LINE 7540 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
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
                  ({-# LINE 213 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 7607 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 214 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 7612 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOmts =
                  ({-# LINE 215 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 7617 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOtn =
                  ({-# LINE 216 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 7622 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _structOval =
                  ({-# LINE 217 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 7627 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _size =
                  ({-# LINE 218 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   getIdxSize _structIvtype
                   {-# LINE 7632 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOmts =
                  ({-# LINE 219 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 7637 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOtn =
                  ({-# LINE 220 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 7642 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idxsOval =
                  ({-# LINE 221 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 7647 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _encidx =
                  ({-# LINE 222 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \i -> ExtractExpr [ IdentExpr $ IdxIdent (SimpleSym "extract") [_size     - 1, 0] , i ]
                   {-# LINE 7652 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 223 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ [ sFn "=" _idIsexpr (sFn "select" a (_encidx     i)) | a <- _structIsexpr, i <- tail _idxsIsexpr  ]
                   {-# LINE 7657 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 224 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "GetElementPtr instruction" else  sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 7668 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 7676 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 7681 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 7686 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 7691 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 7698 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 7703 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  GetElementPtr _pcIself _idIself _tyIself _structIself _idxsIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.GetElementPtr.ty.mn"
                   {-# LINE 7712 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.GetElementPtr.ty.mts"
                   {-# LINE 7717 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _structIident,_structIisGlobal,_structImts,_structIpsexpr,_structIself,_structIsexpr,_structIsexprs,_structIsort,_structIvtype) =
                  struct_ _structOmts _structOtn _structOval
              ( _idxsImts,_idxsIself,_idxsIsexpr,_idxsIsexprs,_idxsIvtype) =
                  idxs_ _idxsOmts _idxsOtn _idxsOval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
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
                  ({-# LINE 119 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 7791 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 120 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 7796 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 121 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 7801 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 122 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 7806 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 123 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 7811 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 124 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 7816 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 125 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 7821 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 126 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 7826 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vse =
                  ({-# LINE 127 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ if _condIself == IntNE then FnAppExpr (SymIdent $ SimpleSym "not") [ sFn "=" e1 e2 ] else sFn _condIpred e1 e2 | e1 <- _op1Isexpr, e2 <- _op2Isexpr ]
                   {-# LINE 7831 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 128 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ Prelude.map (\se -> sFn "=" _idIsexpr se) _vse
                   {-# LINE 7836 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 129 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "ICmp instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 7847 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 7855 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 7860 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 7865 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 7870 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 7877 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 7882 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ICmp _pcIself _idIself _condIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.ICmp.ty.mn"
                   {-# LINE 7891 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.ICmp.ty.mts"
                   {-# LINE 7896 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _aggrOmts :: TypeEnv
              _aggrOtn :: String
              _aggrOval :: (Map.Map Id (Type, [PC]))
              _ivalOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 7963 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  InsertValue _pcIself _idIself _aggrIself _ivalIself _idxsIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.InsertValue.id.sortexpr"
                   {-# LINE 7972 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7977 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.InsertValue.aggr.mts"
                   {-# LINE 7982 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7987 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _aggrOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.InsertValue.aggr.val"
                   {-# LINE 7992 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _aggrImts
                   {-# LINE 7997 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8002 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ivalOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.InsertValue.ival.val"
                   {-# LINE 8007 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _aggrIident,_aggrIisGlobal,_aggrImts,_aggrIpsexpr,_aggrIself,_aggrIsexpr,_aggrIsexprs,_aggrIsort,_aggrIvtype) =
                  aggr_ _aggrOmts _aggrOtn _aggrOval
              ( _ivalIident,_ivalIisGlobal,_ivalImts,_ivalIpsexpr,_ivalIself,_ivalIsexpr,_ivalIsexprs,_ivalIsort,_ivalIvtype) =
                  ival_ _ivalOmts _ivalOtn _ivalOval
              ( _idxsIself) =
                  idxs_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_IntToPtr :: T_PC ->
                            T_Identifier ->
                            T_Value ->
                            T_Type ->
                            T_Instruction
sem_Instruction_IntToPtr pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 8065 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  IntToPtr _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.IntToPtr.id.sortexpr"
                   {-# LINE 8074 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8079 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.IntToPtr.v.mts"
                   {-# LINE 8084 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8089 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.IntToPtr.v.val"
                   {-# LINE 8094 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.IntToPtr.ty.mn"
                   {-# LINE 8099 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 8104 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 8173 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  LShr _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.LShr.id.sortexpr"
                   {-# LINE 8182 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8187 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.LShr.ty.mn"
                   {-# LINE 8192 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.LShr.ty.mts"
                   {-# LINE 8197 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 8202 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8207 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.LShr.op1.val"
                   {-# LINE 8212 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 8217 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8222 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.LShr.op2.val"
                   {-# LINE 8227 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Load :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Align ->
                        T_Instruction
sem_Instruction_Load pc_ id_ v_ align_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _vOmts :: TypeEnv
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
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
                  ({-# LINE 81 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 8279 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 82 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8284 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 83 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 8289 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 84 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8294 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 85 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 8299 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 86 "src/Concurrent/Model/Encoder/Threads.ag" #-}
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
                   {-# LINE 8313 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 8321 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 8326 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 8331 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 8336 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 8343 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 8348 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Load _pcIself _idIself _vIself _alignIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _alignIself) =
                  align_
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 8421 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Mul _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.Mul.id.sortexpr"
                   {-# LINE 8430 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8435 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Mul.ty.mn"
                   {-# LINE 8440 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Mul.ty.mts"
                   {-# LINE 8445 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 8450 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8455 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Mul.op1.val"
                   {-# LINE 8460 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 8465 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8470 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Mul.op2.val"
                   {-# LINE 8475 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_MutexInit :: T_PC ->
                             T_Identifier ->
                             T_Value ->
                             T_Instruction
sem_Instruction_MutexInit pc_ rv_ mutex_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _rvOsortexpr :: (Maybe SSortExpr)
              _rvOtn :: String
              _mutexOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 8525 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  MutexInit _pcIself _rvIself _mutexIself
              _lhsOself =
                  _self
              _rvOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.MutexInit.rv.sortexpr"
                   {-# LINE 8534 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rvOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8539 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.MutexInit.mutex.mts"
                   {-# LINE 8544 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8549 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.MutexInit.mutex.val"
                   {-# LINE 8554 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _rvIdeclexpr,_rvIident,_rvIself,_rvIsexpr,_rvIssymbol) =
                  rv_ _rvOsortexpr _rvOtn
              ( _mutexIident,_mutexIisGlobal,_mutexImts,_mutexIpsexpr,_mutexIself,_mutexIsexpr,_mutexIsexprs,_mutexIsort,_mutexIvtype) =
                  mutex_ _mutexOmts _mutexOtn _mutexOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_MutexLock :: T_PC ->
                             T_Identifier ->
                             T_Value ->
                             T_Instruction
sem_Instruction_MutexLock pc_ rv_ mutex_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _rvOtn :: String
              _rvOsortexpr :: (Maybe SSortExpr)
              _mutexOmts :: TypeEnv
              _mutexOtn :: String
              _mutexOval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
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
                  ({-# LINE 245 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8600 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rvOsortexpr =
                  ({-# LINE 246 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 8605 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOmts =
                  ({-# LINE 247 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 8610 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOtn =
                  ({-# LINE 248 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8615 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOval =
                  ({-# LINE 249 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 8620 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 250 "src/Concurrent/Model/Encoder/Threads.ag" #-}
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
                   {-# LINE 8634 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 286 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then error "Should not happen"
                         else let cmut = _lhsImutexes !! k
                                  xx = IdentExpr $ SymIdent $ SimpleSym $ fromJust _mutexIident ++ show k
                              in Prelude.foldr (\(x,y) r -> (maybe [] (\yy -> if x == xx then [yy] else [sFn "=" yy x] ) y) ++ r) [] cmut
                   {-# LINE 8643 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 8648 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 8653 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 8658 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 8665 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 8670 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  MutexLock _pcIself _rvIself _mutexIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _rvIdeclexpr,_rvIident,_rvIself,_rvIsexpr,_rvIssymbol) =
                  rv_ _rvOsortexpr _rvOtn
              ( _mutexIident,_mutexIisGlobal,_mutexImts,_mutexIpsexpr,_mutexIself,_mutexIsexpr,_mutexIsexprs,_mutexIsort,_mutexIvtype) =
                  mutex_ _mutexOmts _mutexOtn _mutexOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_MutexUnlock :: T_PC ->
                               T_Identifier ->
                               T_Value ->
                               T_Instruction
sem_Instruction_MutexUnlock pc_ rv_ mutex_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _rvOtn :: String
              _rvOsortexpr :: (Maybe SSortExpr)
              _mutexOmts :: TypeEnv
              _mutexOtn :: String
              _mutexOval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
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
                  ({-# LINE 263 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8720 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rvOsortexpr =
                  ({-# LINE 264 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 8725 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOmts =
                  ({-# LINE 265 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 8730 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOtn =
                  ({-# LINE 266 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8735 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _mutexOval =
                  ({-# LINE 267 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 8740 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 268 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            mid  = fromJust _mutexIident
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            enc = IdentExpr $ SymIdent $ SimpleSym $ mid ++ show k
                                            iexp = wrap sAnd $ fpce:enc:preds
                                         in case npce of
                                             Nothing -> _lhsIspark k `sAnd` iexp
                                             Just e  -> let fnpce = if _npcev     == [] then error "mutexunlock instruction" else sFn "=" e $ head _npcev
                                                        in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 8753 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 292 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then error "Should not happen"
                         else let cmut = _lhsImutexes !! k
                                  xx = IdentExpr $ SymIdent $ SimpleSym $ fromJust _mutexIident ++ show k
                                  f = IdentExpr $ SymIdent $ SimpleSym "false"
                              in Prelude.foldr (\(x,y) r -> (maybe [] (\yy -> if x == xx then [sFn "=" yy f] else [sFn "=" yy x] ) y) ++ r) [] cmut
                   {-# LINE 8763 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 8768 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 8773 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 8778 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 8785 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 8790 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  MutexUnlock _pcIself _rvIself _mutexIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _rvIdeclexpr,_rvIident,_rvIself,_rvIsexpr,_rvIssymbol) =
                  rv_ _rvOsortexpr _rvOtn
              ( _mutexIident,_mutexIisGlobal,_mutexImts,_mutexIpsexpr,_mutexIself,_mutexIsexpr,_mutexIsexprs,_mutexIsort,_mutexIvtype) =
                  mutex_ _mutexOmts _mutexOtn _mutexOval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_NotifyEvent :: T_PC ->
                               Int ->
                               T_Instruction
sem_Instruction_NotifyEvent pc_ event_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _pcIself :: PC
              _lhsOts =
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 8820 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  NotifyEvent _pcIself event_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
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
                  ({-# LINE 137 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8887 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 138 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 8892 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 139 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 8897 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 140 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 8902 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 141 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8907 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 142 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 8912 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 143 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 8917 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 144 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 8922 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 145 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ Prelude.map (\se -> sFn "=" _idIsexpr se) _vse
                   {-# LINE 8927 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 146 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "bin instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 8938 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vse =
                  ({-# LINE 158 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ sFn "or" e1 e2 | e1 <- _op1Isexpr, e2 <- _op2Isexpr ]
                   {-# LINE 8943 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 8951 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 8956 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 8961 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 8966 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 8973 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 8978 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Or _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Or.ty.mn"
                   {-# LINE 8987 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Or.ty.mts"
                   {-# LINE 8992 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_PHI :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_PValues ->
                       T_Instruction
sem_Instruction_PHI pc_ id_ ty_ vals_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
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
              _valsIself :: PValues
              _lhsOts =
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 9039 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  PHI _pcIself _idIself _tyIself _valsIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.PHI.id.sortexpr"
                   {-# LINE 9048 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 9053 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.PHI.ty.mn"
                   {-# LINE 9058 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.PHI.ty.mts"
                   {-# LINE 9063 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _valsIself) =
                  vals_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_PtrToInt :: T_PC ->
                            T_Identifier ->
                            T_Value ->
                            T_Type ->
                            T_Instruction
sem_Instruction_PtrToInt pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 9119 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  PtrToInt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.PtrToInt.id.sortexpr"
                   {-# LINE 9128 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 9133 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.PtrToInt.v.mts"
                   {-# LINE 9138 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9143 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.PtrToInt.v.val"
                   {-# LINE 9148 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.PtrToInt.ty.mn"
                   {-# LINE 9153 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 9158 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Ret :: T_PC ->
                       T_RetInst ->
                       T_Instruction
sem_Instruction_Ret pc_ r_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _pcIself :: PC
              _rIself :: RetInst
              _ts =
                  ({-# LINE 240 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, _) k le -> let fpce = sFn "=" pce _pcev
                                         preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                         iexp = wrap sAnd $ fpce:preds
                                     in _lhsIspark k `sAnd` iexp `sAnd` (IdentExpr $ SymIdent $ SimpleSym "false")
                   {-# LINE 9190 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 9198 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 9203 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 9208 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 9213 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 9220 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 9225 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Ret _pcIself _rIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _rIself) =
                  r_
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 9294 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  SDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.SDiv.id.sortexpr"
                   {-# LINE 9303 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 9308 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.SDiv.ty.mn"
                   {-# LINE 9313 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.SDiv.ty.mts"
                   {-# LINE 9318 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 9323 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9328 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.SDiv.op1.val"
                   {-# LINE 9333 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 9338 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9343 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.SDiv.op2.val"
                   {-# LINE 9348 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_SExt :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Type ->
                        T_Instruction
sem_Instruction_SExt pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _idOtn :: String
              _idOsortexpr :: (Maybe SSortExpr)
              _vOmts :: TypeEnv
              _vOtn :: String
              _vOval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
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
                  ({-# LINE 198 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 9406 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 199 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 9411 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 200 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 9416 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 201 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 9421 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 202 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 9426 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 203 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   let n = getISize _tyIself - getISize _vIvtype
                   in  wrap sOr $ Prelude.map (\e -> sFn "=" _idIsexpr $ SignExtExpr e n) _vIsexpr
                   {-# LINE 9432 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 205 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                         in case npce of
                                             Nothing -> _lhsIspark k `sAnd` iexp
                                             Just e  -> let fnpce = if _npcev     == [] then error "SExt instruction" else sFn "=" e $ head _npcev
                                                        in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 9443 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 9451 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 9456 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 9461 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 9466 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 9473 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 9478 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  SExt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.SExt.ty.mn"
                   {-# LINE 9487 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 9492 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 9548 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  SIToFP _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.SIToFP.id.sortexpr"
                   {-# LINE 9557 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 9562 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.SIToFP.v.mts"
                   {-# LINE 9567 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9572 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.SIToFP.v.val"
                   {-# LINE 9577 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.SIToFP.ty.mn"
                   {-# LINE 9582 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 9587 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 9656 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  SRem _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.SRem.id.sortexpr"
                   {-# LINE 9665 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 9670 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.SRem.ty.mn"
                   {-# LINE 9675 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.SRem.ty.mts"
                   {-# LINE 9680 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 9685 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9690 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.SRem.op1.val"
                   {-# LINE 9695 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 9700 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9705 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.SRem.op2.val"
                   {-# LINE 9710 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _condOmts :: TypeEnv
              _condOtn :: String
              _condOval :: (Map.Map Id (Type, [PC]))
              _valtOmts :: TypeEnv
              _valtOtn :: String
              _valtOval :: (Map.Map Id (Type, [PC]))
              _valfOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 9786 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Select _pcIself _idIself _condIself _valtIself _valfIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.Select.id.sortexpr"
                   {-# LINE 9795 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 9800 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _condOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Select.cond.mts"
                   {-# LINE 9805 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _condOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9810 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _condOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Select.cond.val"
                   {-# LINE 9815 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valtOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _condImts
                   {-# LINE 9820 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valtOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9825 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valtOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Select.valt.val"
                   {-# LINE 9830 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valfOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _valtImts
                   {-# LINE 9835 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valfOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9840 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _valfOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Select.valf.val"
                   {-# LINE 9845 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _condIident,_condIisGlobal,_condImts,_condIpsexpr,_condIself,_condIsexpr,_condIsexprs,_condIsort,_condIvtype) =
                  cond_ _condOmts _condOtn _condOval
              ( _valtIident,_valtIisGlobal,_valtImts,_valtIpsexpr,_valtIself,_valtIsexpr,_valtIsexprs,_valtIsort,_valtIvtype) =
                  valt_ _valtOmts _valtOtn _valtOval
              ( _valfIident,_valfIisGlobal,_valfImts,_valfIpsexpr,_valfIself,_valfIsexpr,_valfIsexprs,_valfIsort,_valfIvtype) =
                  valf_ _valfOmts _valfOtn _valfOval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 9916 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Shl _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.Shl.id.sortexpr"
                   {-# LINE 9925 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 9930 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Shl.ty.mn"
                   {-# LINE 9935 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Shl.ty.mts"
                   {-# LINE 9940 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 9945 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9950 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Shl.op1.val"
                   {-# LINE 9955 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 9960 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9965 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Shl.op2.val"
                   {-# LINE 9970 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _v1Omts :: TypeEnv
              _v1Oval :: (Map.Map Id (Type, [PC]))
              _v2Omts :: TypeEnv
              _v2Oval :: (Map.Map Id (Type, [PC]))
              _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _v1Otn :: String
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
                  ({-# LINE 97 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 10035 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v1Omts =
                  ({-# LINE 98 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 10040 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v1Oval =
                  ({-# LINE 99 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _val
                   {-# LINE 10045 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v2Omts =
                  ({-# LINE 100 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 10050 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v2Oval =
                  ({-# LINE 101 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _val
                   {-# LINE 10055 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v1e =
                  ({-# LINE 102 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   if _v1Isexpr == [] then error "Store Instruction" else head _v1Isexpr
                   {-# LINE 10060 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 103 "src/Concurrent/Model/Encoder/Threads.ag" #-}
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
                   {-# LINE 10079 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 10087 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 10092 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 10097 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 10102 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 10109 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 10114 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Store _pcIself _tyIself _v1Iself _v2Iself _alignIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Store.ty.mn"
                   {-# LINE 10123 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Store.ty.mts"
                   {-# LINE 10128 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10133 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _v2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10138 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _v1Iident,_v1IisGlobal,_v1Imts,_v1Ipsexpr,_v1Iself,_v1Isexpr,_v1Isexprs,_v1Isort,_v1Ivtype) =
                  v1_ _v1Omts _v1Otn _v1Oval
              ( _v2Iident,_v2IisGlobal,_v2Imts,_v2Ipsexpr,_v2Iself,_v2Isexpr,_v2Isexprs,_v2Isort,_v2Ivtype) =
                  v2_ _v2Omts _v2Otn _v2Oval
              ( _alignIself) =
                  align_
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
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
                  ({-# LINE 137 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 10209 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOsortexpr =
                  ({-# LINE 138 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Nothing
                   {-# LINE 10214 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 139 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 10219 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 140 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 10224 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 141 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 10229 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 142 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   fStore _lhsIprenc
                   {-# LINE 10234 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 143 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   sortEnv _lhsIprenc
                   {-# LINE 10239 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 144 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 10244 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _enc =
                  ({-# LINE 145 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   wrap sOr $ Prelude.map (\se -> sFn "=" _idIsexpr se) _vse
                   {-# LINE 10249 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _ts =
                  ({-# LINE 146 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp = wrap sAnd $ fpce:(_enc    ):preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "bin instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 10260 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vse =
                  ({-# LINE 156 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ sFn "bvsub" e1 e2 | e1 <- _op1Isexpr, e2 <- _op2Isexpr ]
                   {-# LINE 10265 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 10273 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 10278 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 10283 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 10288 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 10295 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 10300 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Sub _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Sub.ty.mn"
                   {-# LINE 10309 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Sub.ty.mts"
                   {-# LINE 10314 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Switch :: T_PC ->
                          T_IntTyValIdL ->
                          T_Instruction
sem_Instruction_Switch pc_ elems_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _pcIself :: PC
              _elemsIself :: IntTyValIdL
              _lhsOts =
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10345 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Switch _pcIself _elemsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _elemsIself) =
                  elems_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Trunc :: T_PC ->
                         T_Identifier ->
                         T_Value ->
                         T_Type ->
                         T_Instruction
sem_Instruction_Trunc pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10401 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Trunc _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.Trunc.id.sortexpr"
                   {-# LINE 10410 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 10415 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Trunc.v.mts"
                   {-# LINE 10420 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10425 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Trunc.v.val"
                   {-# LINE 10430 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Trunc.ty.mn"
                   {-# LINE 10435 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 10440 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_UBr :: T_PC ->
                       T_Value ->
                       T_Instruction
sem_Instruction_UBr pc_ d_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _dOmts :: TypeEnv
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
                  ({-# LINE 162 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, npce) k le -> let fpce = sFn "=" pce _pcev
                                            preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                            iexp   = wrap sAnd $ fpce:preds
                                        in case npce of
                                            Nothing -> _lhsIspark k `sAnd` iexp
                                            Just e  -> let fnpce = if _npcev     == [] then error "Ubr instruction" else sFn "=" e $ head _npcev
                                                       in  _lhsIspark k `sAnd` iexp `sAnd` fnpce
                   {-# LINE 10486 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 10494 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 10499 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 10504 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 10509 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 10516 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 10521 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  UBr _pcIself _dIself
              _lhsOself =
                  _self
              _dOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.UBr.d.mts"
                   {-# LINE 10530 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _dOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10535 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _dOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.UBr.d.val"
                   {-# LINE 10540 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _dIident,_dIisGlobal,_dImts,_dIpsexpr,_dIself,_dIsexpr,_dIsexprs,_dIsort,_dIvtype) =
                  d_ _dOmts _dOtn _dOval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10605 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  UDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.UDiv.id.sortexpr"
                   {-# LINE 10614 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 10619 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.UDiv.ty.mn"
                   {-# LINE 10624 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.UDiv.ty.mts"
                   {-# LINE 10629 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 10634 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10639 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.UDiv.op1.val"
                   {-# LINE 10644 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 10649 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10654 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.UDiv.op2.val"
                   {-# LINE 10659 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_UIToFP :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_UIToFP pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10717 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  UIToFP _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.UIToFP.id.sortexpr"
                   {-# LINE 10726 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 10731 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.UIToFP.v.mts"
                   {-# LINE 10736 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10741 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.UIToFP.v.val"
                   {-# LINE 10746 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.UIToFP.ty.mn"
                   {-# LINE 10751 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 10756 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10825 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  URem _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.URem.id.sortexpr"
                   {-# LINE 10834 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 10839 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.URem.ty.mn"
                   {-# LINE 10844 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.URem.ty.mts"
                   {-# LINE 10849 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 10854 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10859 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.URem.op1.val"
                   {-# LINE 10864 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 10869 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 10874 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.URem.op2.val"
                   {-# LINE 10879 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_Unreachable :: T_PC ->
                               T_Instruction
sem_Instruction_Unreachable pc_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _pcIself :: PC
              _ts =
                  ({-# LINE 240 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \(pce, _) k le -> let fpce = sFn "=" pce _pcev
                                         preds  = encPreds le k _pcIself (fStore _lhsIprenc) Nothing
                                         iexp = wrap sAnd $ fpce:preds
                                     in _lhsIspark k `sAnd` iexp `sAnd` (IdentExpr $ SymIdent $ SimpleSym "false")
                   {-# LINE 10911 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _nmt =
                  ({-# LINE 281 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \k -> if _lhsImutexes == []
                         then []
                         else let cmut = _lhsImutexes !! k
                              in  Prelude.foldr (\(x,y) r -> (maybe [] (\x2 -> [sFn "=" x2 x]) y)++ r) [] cmut
                   {-# LINE 10919 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npc =
                  ({-# LINE 299 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   nextpc _pcIself _lhsIcfg
                   {-# LINE 10924 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _npcev =
                  ({-# LINE 300 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   Prelude.map (\p -> IdentExpr $ IdxIdent (bv p) [32]) _npc
                   {-# LINE 10929 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pcev =
                  ({-# LINE 301 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   IdentExpr $ IdxIdent (bv _pcIself) [32]
                   {-# LINE 10934 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _pts =
                  ({-# LINE 302 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   \m k le -> let p = fromMaybe (error "Searching for pcs") $ Map.lookup _lhsItn m
                                  m' = Map.elems $ Map.delete _lhsItn m
                              in wrap sAnd $ ((_ts     p k le):(_nmt     k)) ++ (encOFPC m')
                   {-# LINE 10941 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 305 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   [ _pts     ]
                   {-# LINE 10946 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Unreachable _pcIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_WaitEvent :: T_PC ->
                             Int ->
                             T_Instruction
sem_Instruction_WaitEvent pc_ event_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _pcIself :: PC
              _lhsOts =
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 10972 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  WaitEvent _pcIself event_
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _timeOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 11010 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  WaitTime _pcIself _timeIself
              _lhsOself =
                  _self
              _timeOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.WaitTime.time.mts"
                   {-# LINE 11019 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _timeOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 11024 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _timeOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.WaitTime.time.val"
                   {-# LINE 11029 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _timeIident,_timeIisGlobal,_timeImts,_timeIpsexpr,_timeIself,_timeIsexpr,_timeIsexprs,_timeIsort,_timeIvtype) =
                  time_ _timeOmts _timeOtn _timeOval
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
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: TypeEnv
              _op1Omts :: TypeEnv
              _op1Otn :: String
              _op1Oval :: (Map.Map Id (Type, [PC]))
              _op2Omts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 11094 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Xor _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.Xor.id.sortexpr"
                   {-# LINE 11103 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 11108 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Xor.ty.mn"
                   {-# LINE 11113 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.Xor.ty.mts"
                   {-# LINE 11118 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 11123 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 11128 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Xor.op1.val"
                   {-# LINE 11133 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 11138 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 11143 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.Xor.op2.val"
                   {-# LINE 11148 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Iident,_op1IisGlobal,_op1Imts,_op1Ipsexpr,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Isort,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Iident,_op2IisGlobal,_op2Imts,_op2Ipsexpr,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Isort,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOself,_lhsOts)))
sem_Instruction_ZExt :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Type ->
                        T_Instruction
sem_Instruction_ZExt pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instruction
              _idOsortexpr :: (Maybe SSortExpr)
              _idOtn :: String
              _vOmts :: TypeEnv
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
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 11206 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ZExt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Instruction.ZExt.id.sortexpr"
                   {-# LINE 11215 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 11220 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.ZExt.v.mts"
                   {-# LINE 11225 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 11230 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   error "missing rule: Instruction.ZExt.v.val"
                   {-# LINE 11235 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   error "missing rule: Instruction.ZExt.ty.mn"
                   {-# LINE 11240 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _vImts
                   {-# LINE 11245 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIdeclexpr,_idIident,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOsortexpr _idOtn
              ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOself,_lhsOts)))
-- Instructions ------------------------------------------------
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = CF ->
                      (Map.Map String PC) ->
                      ([[(SExpr, Maybe SExpr)]]) ->
                      (Map.Map String [PC]) ->
                      PreEncoder ->
                      (Int -> SExpr) ->
                      Id ->
                      ( Instructions,([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr]))
data Inh_Instructions = Inh_Instructions {cfg_Inh_Instructions :: CF,cte_Inh_Instructions :: (Map.Map String PC),mutexes_Inh_Instructions :: ([[(SExpr, Maybe SExpr)]]),pcs_Inh_Instructions :: (Map.Map String [PC]),prenc_Inh_Instructions :: PreEncoder,spark_Inh_Instructions :: (Int -> SExpr),tn_Inh_Instructions :: Id}
data Syn_Instructions = Syn_Instructions {self_Syn_Instructions :: Instructions,ts_Syn_Instructions :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])}
wrap_Instructions :: T_Instructions ->
                     Inh_Instructions ->
                     Syn_Instructions
wrap_Instructions sem (Inh_Instructions _lhsIcfg _lhsIcte _lhsImutexes _lhsIpcs _lhsIprenc _lhsIspark _lhsItn) =
    (let ( _lhsOself,_lhsOts) = sem _lhsIcfg _lhsIcte _lhsImutexes _lhsIpcs _lhsIprenc _lhsIspark _lhsItn
     in  (Syn_Instructions _lhsOself _lhsOts))
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instructions
              _hdOcfg :: CF
              _hdOcte :: (Map.Map String PC)
              _hdOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _hdOpcs :: (Map.Map String [PC])
              _hdOprenc :: PreEncoder
              _hdOspark :: (Int -> SExpr)
              _hdOtn :: Id
              _tlOcfg :: CF
              _tlOcte :: (Map.Map String PC)
              _tlOmutexes :: ([[(SExpr, Maybe SExpr)]])
              _tlOpcs :: (Map.Map String [PC])
              _tlOprenc :: PreEncoder
              _tlOspark :: (Int -> SExpr)
              _tlOtn :: Id
              _hdIself :: Instruction
              _hdIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _tlIself :: Instructions
              _tlIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOts =
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _hdIts ++ _tlIts
                   {-# LINE 11313 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOcfg =
                  ({-# LINE 62 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 11322 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOcte =
                  ({-# LINE 63 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 11327 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOmutexes =
                  ({-# LINE 68 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 11332 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOpcs =
                  ({-# LINE 64 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIpcs
                   {-# LINE 11337 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOprenc =
                  ({-# LINE 66 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 11342 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOspark =
                  ({-# LINE 67 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIspark
                   {-# LINE 11347 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 65 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 11352 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOcfg =
                  ({-# LINE 62 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcfg
                   {-# LINE 11357 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOcte =
                  ({-# LINE 63 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIcte
                   {-# LINE 11362 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmutexes =
                  ({-# LINE 68 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsImutexes
                   {-# LINE 11367 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOpcs =
                  ({-# LINE 64 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIpcs
                   {-# LINE 11372 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOprenc =
                  ({-# LINE 66 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIprenc
                   {-# LINE 11377 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOspark =
                  ({-# LINE 67 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsIspark
                   {-# LINE 11382 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 65 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   _lhsItn
                   {-# LINE 11387 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIself,_hdIts) =
                  hd_ _hdOcfg _hdOcte _hdOmutexes _hdOpcs _hdOprenc _hdOspark _hdOtn
              ( _tlIself,_tlIts) =
                  tl_ _tlOcfg _tlOcte _tlOmutexes _tlOpcs _tlOprenc _tlOspark _tlOtn
          in  ( _lhsOself,_lhsOts)))
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    (\ _lhsIcfg
       _lhsIcte
       _lhsImutexes
       _lhsIpcs
       _lhsIprenc
       _lhsIspark
       _lhsItn ->
         (let _lhsOts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
              _lhsOself :: Instructions
              _lhsOts =
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Threads.ag" #-}
                   []
                   {-# LINE 11408 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
sem_IntPredicate (IntSGE) =
    (sem_IntPredicate_IntSGE)
sem_IntPredicate (IntSGT) =
    (sem_IntPredicate_IntSGT)
sem_IntPredicate (IntSLE) =
    (sem_IntPredicate_IntSLE)
sem_IntPredicate (IntSLT) =
    (sem_IntPredicate_IntSLT)
sem_IntPredicate (IntUGE) =
    (sem_IntPredicate_IntUGE)
sem_IntPredicate (IntUGT) =
    (sem_IntPredicate_IntUGT)
sem_IntPredicate (IntULE) =
    (sem_IntPredicate_IntULE)
sem_IntPredicate (IntULT) =
    (sem_IntPredicate_IntULT)
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
             ({-# LINE 311 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              "="
              {-# LINE 11456 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
             ({-# LINE 312 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              error "IntNE"
              {-# LINE 11470 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntNE
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntSGE :: T_IntPredicate
sem_IntPredicate_IntSGE =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 318 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvsge"
              {-# LINE 11484 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntSGE
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntSGT :: T_IntPredicate
sem_IntPredicate_IntSGT =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 317 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvsgt"
              {-# LINE 11498 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntSGT
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntSLE :: T_IntPredicate
sem_IntPredicate_IntSLE =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 320 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvsle"
              {-# LINE 11512 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntSLE
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntSLT :: T_IntPredicate
sem_IntPredicate_IntSLT =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 319 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvslt"
              {-# LINE 11526 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntSLT
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntUGE :: T_IntPredicate
sem_IntPredicate_IntUGE =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 314 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvuge"
              {-# LINE 11540 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntUGE
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntUGT :: T_IntPredicate
sem_IntPredicate_IntUGT =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 313 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvugt"
              {-# LINE 11554 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntUGT
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntULE :: T_IntPredicate
sem_IntPredicate_IntULE =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 316 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvule"
              {-# LINE 11568 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntULE
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
sem_IntPredicate_IntULT :: T_IntPredicate
sem_IntPredicate_IntULT =
    (let _lhsOpred :: String
         _lhsOself :: IntPredicate
         _lhsOpred =
             ({-# LINE 315 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              "bvult"
              {-# LINE 11582 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _self =
             IntULT
         _lhsOself =
             _self
     in  ( _lhsOpred,_lhsOself))
-- IntTyValId --------------------------------------------------
-- cata
sem_IntTyValId :: IntTyValId ->
                  T_IntTyValId
sem_IntTyValId ( x1,x2,x3) =
    (sem_IntTyValId_Tuple (sem_Type x1) (sem_Value x2) (sem_Identifier x3))
-- semantic domain
type T_IntTyValId = ( IntTyValId)
data Inh_IntTyValId = Inh_IntTyValId {}
data Syn_IntTyValId = Syn_IntTyValId {self_Syn_IntTyValId :: IntTyValId}
wrap_IntTyValId :: T_IntTyValId ->
                   Inh_IntTyValId ->
                   Syn_IntTyValId
wrap_IntTyValId sem (Inh_IntTyValId) =
    (let ( _lhsOself) = sem
     in  (Syn_IntTyValId _lhsOself))
sem_IntTyValId_Tuple :: T_Type ->
                        T_Value ->
                        T_Identifier ->
                        T_IntTyValId
sem_IntTyValId_Tuple x1_ x2_ x3_ =
    (let _lhsOself :: IntTyValId
         _x1Omn :: (Maybe SSort)
         _x1Omts :: TypeEnv
         _x2Omts :: TypeEnv
         _x2Otn :: String
         _x2Oval :: (Map.Map Id (Type, [PC]))
         _x3Osortexpr :: (Maybe SSortExpr)
         _x3Otn :: String
         _x1Imts :: TypeEnv
         _x1Iself :: Type
         _x1Isexprs :: SExpressions
         _x1Isort :: SSortExpr
         _x1Isortn :: SSort
         _x2Iident :: (Maybe String)
         _x2IisGlobal :: Bool
         _x2Imts :: TypeEnv
         _x2Ipsexpr :: (Int -> [SExpr])
         _x2Iself :: Value
         _x2Isexpr :: ([SExpr])
         _x2Isexprs :: SExpressions
         _x2Isort :: SSortExpr
         _x2Ivtype :: Type
         _x3Ideclexpr :: ([SExpression])
         _x3Iident :: String
         _x3Iself :: Identifier
         _x3Isexpr :: SExpr
         _x3Issymbol :: SSymbol
         _self =
             (_x1Iself,_x2Iself,_x3Iself)
         _lhsOself =
             _self
         _x1Omn =
             ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
              error "missing rule: IntTyValId.Tuple.x1.mn"
              {-# LINE 11644 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x1Omts =
             ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
              error "missing rule: IntTyValId.Tuple.x1.mts"
              {-# LINE 11649 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x2Omts =
             ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
              _x1Imts
              {-# LINE 11654 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x2Otn =
             ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: IntTyValId.Tuple.x2.tn"
              {-# LINE 11659 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x2Oval =
             ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: IntTyValId.Tuple.x2.val"
              {-# LINE 11664 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x3Osortexpr =
             ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
              error "missing rule: IntTyValId.Tuple.x3.sortexpr"
              {-# LINE 11669 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x3Otn =
             ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
              error "missing rule: IntTyValId.Tuple.x3.tn"
              {-# LINE 11674 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         ( _x1Imts,_x1Iself,_x1Isexprs,_x1Isort,_x1Isortn) =
             x1_ _x1Omn _x1Omts
         ( _x2Iident,_x2IisGlobal,_x2Imts,_x2Ipsexpr,_x2Iself,_x2Isexpr,_x2Isexprs,_x2Isort,_x2Ivtype) =
             x2_ _x2Omts _x2Otn _x2Oval
         ( _x3Ideclexpr,_x3Iident,_x3Iself,_x3Isexpr,_x3Issymbol) =
             x3_ _x3Osortexpr _x3Otn
     in  ( _lhsOself))
-- IntTyValIdL -------------------------------------------------
-- cata
sem_IntTyValIdL :: IntTyValIdL ->
                   T_IntTyValIdL
sem_IntTyValIdL list =
    (Prelude.foldr sem_IntTyValIdL_Cons sem_IntTyValIdL_Nil (Prelude.map sem_IntTyValId list))
-- semantic domain
type T_IntTyValIdL = ( IntTyValIdL)
data Inh_IntTyValIdL = Inh_IntTyValIdL {}
data Syn_IntTyValIdL = Syn_IntTyValIdL {self_Syn_IntTyValIdL :: IntTyValIdL}
wrap_IntTyValIdL :: T_IntTyValIdL ->
                    Inh_IntTyValIdL ->
                    Syn_IntTyValIdL
wrap_IntTyValIdL sem (Inh_IntTyValIdL) =
    (let ( _lhsOself) = sem
     in  (Syn_IntTyValIdL _lhsOself))
sem_IntTyValIdL_Cons :: T_IntTyValId ->
                        T_IntTyValIdL ->
                        T_IntTyValIdL
sem_IntTyValIdL_Cons hd_ tl_ =
    (let _lhsOself :: IntTyValIdL
         _hdIself :: IntTyValId
         _tlIself :: IntTyValIdL
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_IntTyValIdL_Nil :: T_IntTyValIdL
sem_IntTyValIdL_Nil =
    (let _lhsOself :: IntTyValIdL
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
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
-- Label -------------------------------------------------------
-- cata
sem_Label :: Label ->
             T_Label
sem_Label ( x1) =
    (sem_Label_Tuple x1)
-- semantic domain
type T_Label = ( Label)
data Inh_Label = Inh_Label {}
data Syn_Label = Syn_Label {self_Syn_Label :: Label}
wrap_Label :: T_Label ->
              Inh_Label ->
              Syn_Label
wrap_Label sem (Inh_Label) =
    (let ( _lhsOself) = sem
     in  (Syn_Label _lhsOself))
sem_Label_Tuple :: String ->
                   T_Label
sem_Label_Tuple x1_ =
    (let _lhsOself :: Label
         _self =
             (x1_)
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Linkage -----------------------------------------------------
-- cata
sem_Linkage :: Linkage ->
               T_Linkage
sem_Linkage (AppendingLinkage) =
    (sem_Linkage_AppendingLinkage)
sem_Linkage (AvailableExternallyLinkage) =
    (sem_Linkage_AvailableExternallyLinkage)
sem_Linkage (CommonLinkage) =
    (sem_Linkage_CommonLinkage)
sem_Linkage (DLLExportLinkage) =
    (sem_Linkage_DLLExportLinkage)
sem_Linkage (DLLImportLinkage) =
    (sem_Linkage_DLLImportLinkage)
sem_Linkage (ExternalLinkage) =
    (sem_Linkage_ExternalLinkage)
sem_Linkage (ExternalWeakLinkage) =
    (sem_Linkage_ExternalWeakLinkage)
sem_Linkage (GhostLinkage) =
    (sem_Linkage_GhostLinkage)
sem_Linkage (InternalLinkage) =
    (sem_Linkage_InternalLinkage)
sem_Linkage (LinkOnceAnyLinkage) =
    (sem_Linkage_LinkOnceAnyLinkage)
sem_Linkage (LinkOnceODRLinkage) =
    (sem_Linkage_LinkOnceODRLinkage)
sem_Linkage (LinkerPrivateLinkage) =
    (sem_Linkage_LinkerPrivateLinkage)
sem_Linkage (LinkerPrivateWeakDefAutoLinkage) =
    (sem_Linkage_LinkerPrivateWeakDefAutoLinkage)
sem_Linkage (LinkerPrivateWeakLinkage) =
    (sem_Linkage_LinkerPrivateWeakLinkage)
sem_Linkage (PrivateLinkage) =
    (sem_Linkage_PrivateLinkage)
sem_Linkage (WeakAnyLinkage) =
    (sem_Linkage_WeakAnyLinkage)
sem_Linkage (WeakODRLinkage) =
    (sem_Linkage_WeakODRLinkage)
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
sem_Linkage_AppendingLinkage :: T_Linkage
sem_Linkage_AppendingLinkage =
    (let _lhsOself :: Linkage
         _self =
             AppendingLinkage
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
sem_Linkage_CommonLinkage :: T_Linkage
sem_Linkage_CommonLinkage =
    (let _lhsOself :: Linkage
         _self =
             CommonLinkage
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
sem_Linkage_DLLImportLinkage :: T_Linkage
sem_Linkage_DLLImportLinkage =
    (let _lhsOself :: Linkage
         _self =
             DLLImportLinkage
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Linkage_ExternalLinkage :: T_Linkage
sem_Linkage_ExternalLinkage =
    (let _lhsOself :: Linkage
         _self =
             ExternalLinkage
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
sem_Linkage_InternalLinkage :: T_Linkage
sem_Linkage_InternalLinkage =
    (let _lhsOself :: Linkage
         _self =
             InternalLinkage
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
sem_Linkage_LinkerPrivateLinkage :: T_Linkage
sem_Linkage_LinkerPrivateLinkage =
    (let _lhsOself :: Linkage
         _self =
             LinkerPrivateLinkage
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
sem_Linkage_LinkerPrivateWeakLinkage :: T_Linkage
sem_Linkage_LinkerPrivateWeakLinkage =
    (let _lhsOself :: Linkage
         _self =
             LinkerPrivateWeakLinkage
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
type T_MConstant = TypeEnv ->
                   String ->
                   (Map.Map Id (Type, [PC])) ->
                   ( TypeEnv,MConstant,([SExpr]),SExpressions)
data Inh_MConstant = Inh_MConstant {mts_Inh_MConstant :: TypeEnv,tn_Inh_MConstant :: String,val_Inh_MConstant :: (Map.Map Id (Type, [PC]))}
data Syn_MConstant = Syn_MConstant {mts_Syn_MConstant :: TypeEnv,self_Syn_MConstant :: MConstant,sexpr_Syn_MConstant :: ([SExpr]),sexprs_Syn_MConstant :: SExpressions}
wrap_MConstant :: T_MConstant ->
                  Inh_MConstant ->
                  Syn_MConstant
wrap_MConstant sem (Inh_MConstant _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_MConstant _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs))
sem_MConstant_Just :: T_Constant ->
                      T_MConstant
sem_MConstant_Just just_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOself :: MConstant
              _lhsOmts :: TypeEnv
              _justOmts :: TypeEnv
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
                  ({-# LINE 23 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _justIsexpr
                   {-# LINE 12129 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 24 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _justIsexprs
                   {-# LINE 12134 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Just _justIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _justImts
                   {-# LINE 12143 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 12148 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12153 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _justOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12158 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _justIident,_justIisGlobal,_justImts,_justIpsexpr,_justIself,_justIsexpr,_justIsexprs,_justIsort,_justIvtype) =
                  just_ _justOmts _justOtn _justOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs)))
sem_MConstant_Nothing :: T_MConstant
sem_MConstant_Nothing =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOself :: MConstant
              _lhsOmts :: TypeEnv
              _lhsOsexpr =
                  ({-# LINE 20 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 12175 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 12180 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Nothing
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 12189 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
type T_MIdentifier = ( MIdentifier)
data Inh_MIdentifier = Inh_MIdentifier {}
data Syn_MIdentifier = Syn_MIdentifier {self_Syn_MIdentifier :: MIdentifier}
wrap_MIdentifier :: T_MIdentifier ->
                    Inh_MIdentifier ->
                    Syn_MIdentifier
wrap_MIdentifier sem (Inh_MIdentifier) =
    (let ( _lhsOself) = sem
     in  (Syn_MIdentifier _lhsOself))
sem_MIdentifier_Just :: T_Identifier ->
                        T_MIdentifier
sem_MIdentifier_Just just_ =
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
             ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
              error "missing rule: MIdentifier.Just.just.sortexpr"
              {-# LINE 12342 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _justOtn =
             ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
              error "missing rule: MIdentifier.Just.just.tn"
              {-# LINE 12347 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         ( _justIdeclexpr,_justIident,_justIself,_justIsexpr,_justIssymbol) =
             just_ _justOsortexpr _justOtn
     in  ( _lhsOself))
sem_MIdentifier_Nothing :: T_MIdentifier
sem_MIdentifier_Nothing =
    (let _lhsOself :: MIdentifier
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MLabel ------------------------------------------------------
-- cata
sem_MLabel :: MLabel ->
              T_MLabel
sem_MLabel (Prelude.Just x) =
    (sem_MLabel_Just (sem_Label x))
sem_MLabel Prelude.Nothing =
    sem_MLabel_Nothing
-- semantic domain
type T_MLabel = ( MLabel)
data Inh_MLabel = Inh_MLabel {}
data Syn_MLabel = Syn_MLabel {self_Syn_MLabel :: MLabel}
wrap_MLabel :: T_MLabel ->
               Inh_MLabel ->
               Syn_MLabel
wrap_MLabel sem (Inh_MLabel) =
    (let ( _lhsOself) = sem
     in  (Syn_MLabel _lhsOself))
sem_MLabel_Just :: T_Label ->
                   T_MLabel
sem_MLabel_Just just_ =
    (let _lhsOself :: MLabel
         _justIself :: Label
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
     in  ( _lhsOself))
sem_MLabel_Nothing :: T_MLabel
sem_MLabel_Nothing =
    (let _lhsOself :: MLabel
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
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
type T_MValue = ( MValue)
data Inh_MValue = Inh_MValue {}
data Syn_MValue = Syn_MValue {self_Syn_MValue :: MValue}
wrap_MValue :: T_MValue ->
               Inh_MValue ->
               Syn_MValue
wrap_MValue sem (Inh_MValue) =
    (let ( _lhsOself) = sem
     in  (Syn_MValue _lhsOself))
sem_MValue_Just :: T_Value ->
                   T_MValue
sem_MValue_Just just_ =
    (let _lhsOself :: MValue
         _justOmts :: TypeEnv
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
         _justOmts =
             ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: MValue.Just.just.mts"
              {-# LINE 12588 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _justOtn =
             ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: MValue.Just.just.tn"
              {-# LINE 12593 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _justOval =
             ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: MValue.Just.just.val"
              {-# LINE 12598 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         ( _justIident,_justIisGlobal,_justImts,_justIpsexpr,_justIself,_justIsexpr,_justIsexprs,_justIsort,_justIvtype) =
             just_ _justOmts _justOtn _justOval
     in  ( _lhsOself))
sem_MValue_Nothing :: T_MValue
sem_MValue_Nothing =
    (let _lhsOself :: MValue
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
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
    (let _lhsOself :: Module
         _gvarsOgs :: GlobalState
         _funsOcfg :: (Map.Map String CF)
         _funsOcte :: (Map.Map String PC)
         _funsOmutexes :: ([[(SExpr, Maybe SExpr)]])
         _funsOprenc :: PreEncoder
         _layoutIself :: DataLayout
         _targetIself :: TargetData
         _gvarsIgs :: GlobalState
         _gvarsIself :: Globals
         _gvarsIsexpr :: ([SExpr])
         _gvarsIsexprs :: SExpressions
         _funsIself :: Functions
         _funsIts :: ([Map.Map String (SExpr, Maybe SExpr) -> Int -> [(SExpr, Maybe SExpr)] -> SExpr])
         _nmdtysIself :: NamedTypes
         _self =
             Module id_ _layoutIself _targetIself _gvarsIself _funsIself _nmdtysIself
         _lhsOself =
             _self
         _gvarsOgs =
             ({-# LINE 20 "src/Concurrent/Model/Encoder/Global.ag" #-}
              error "missing rule: Module.Module.gvars.gs"
              {-# LINE 12736 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _funsOcfg =
             ({-# LINE 46 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              error "missing rule: Module.Module.funs.cfg"
              {-# LINE 12741 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _funsOcte =
             ({-# LINE 47 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              error "missing rule: Module.Module.funs.cte"
              {-# LINE 12746 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _funsOmutexes =
             ({-# LINE 48 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              error "missing rule: Module.Module.funs.mutexes"
              {-# LINE 12751 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _funsOprenc =
             ({-# LINE 45 "src/Concurrent/Model/Encoder/Threads.ag" #-}
              error "missing rule: Module.Module.funs.prenc"
              {-# LINE 12756 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         ( _layoutIself) =
             layout_
         ( _targetIself) =
             target_
         ( _gvarsIgs,_gvarsIself,_gvarsIsexpr,_gvarsIsexprs) =
             gvars_ _gvarsOgs
         ( _funsIself,_funsIts) =
             funs_ _funsOcfg _funsOcte _funsOmutexes _funsOprenc
         ( _nmdtysIself) =
             nmdtys_
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
type T_NamedTypes = ( NamedTypes)
data Inh_NamedTypes = Inh_NamedTypes {}
data Syn_NamedTypes = Syn_NamedTypes {self_Syn_NamedTypes :: NamedTypes}
wrap_NamedTypes :: T_NamedTypes ->
                   Inh_NamedTypes ->
                   Syn_NamedTypes
wrap_NamedTypes sem (Inh_NamedTypes) =
    (let ( _lhsOself) = sem
     in  (Syn_NamedTypes _lhsOself))
sem_NamedTypes_Entry :: Id ->
                        T_Type ->
                        T_NamedTypes ->
                        T_NamedTypes
sem_NamedTypes_Entry key_ val_ tl_ =
    (let _lhsOself :: NamedTypes
         _valOmn :: (Maybe SSort)
         _valOmts :: TypeEnv
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
             ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
              error "missing rule: NamedTypes.Entry.val.mn"
              {-# LINE 12871 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _valOmts =
             ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
              error "missing rule: NamedTypes.Entry.val.mts"
              {-# LINE 12876 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         ( _valImts,_valIself,_valIsexprs,_valIsort,_valIsortn) =
             val_ _valOmn _valOmts
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_NamedTypes_Nil :: T_NamedTypes
sem_NamedTypes_Nil =
    (let _lhsOself :: NamedTypes
         _self =
             Data.Map.empty
         _lhsOself =
             _self
     in  ( _lhsOself))
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
type T_PTyInt = ( PTyInt)
data Inh_PTyInt = Inh_PTyInt {}
data Syn_PTyInt = Syn_PTyInt {self_Syn_PTyInt :: PTyInt}
wrap_PTyInt :: T_PTyInt ->
               Inh_PTyInt ->
               Syn_PTyInt
wrap_PTyInt sem (Inh_PTyInt) =
    (let ( _lhsOself) = sem
     in  (Syn_PTyInt _lhsOself))
sem_PTyInt_Tuple :: T_Type ->
                    Int ->
                    T_PTyInt
sem_PTyInt_Tuple x1_ x2_ =
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
             ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
              error "missing rule: PTyInt.Tuple.x1.mn"
              {-# LINE 12951 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x1Omts =
             ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
              error "missing rule: PTyInt.Tuple.x1.mts"
              {-# LINE 12956 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         ( _x1Imts,_x1Iself,_x1Isexprs,_x1Isort,_x1Isortn) =
             x1_ _x1Omn _x1Omts
     in  ( _lhsOself))
-- PTyIntL -----------------------------------------------------
-- cata
sem_PTyIntL :: PTyIntL ->
               T_PTyIntL
sem_PTyIntL list =
    (Prelude.foldr sem_PTyIntL_Cons sem_PTyIntL_Nil (Prelude.map sem_PTyInt list))
-- semantic domain
type T_PTyIntL = ( PTyIntL)
data Inh_PTyIntL = Inh_PTyIntL {}
data Syn_PTyIntL = Syn_PTyIntL {self_Syn_PTyIntL :: PTyIntL}
wrap_PTyIntL :: T_PTyIntL ->
                Inh_PTyIntL ->
                Syn_PTyIntL
wrap_PTyIntL sem (Inh_PTyIntL) =
    (let ( _lhsOself) = sem
     in  (Syn_PTyIntL _lhsOself))
sem_PTyIntL_Cons :: T_PTyInt ->
                    T_PTyIntL ->
                    T_PTyIntL
sem_PTyIntL_Cons hd_ tl_ =
    (let _lhsOself :: PTyIntL
         _hdIself :: PTyInt
         _tlIself :: PTyIntL
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_PTyIntL_Nil :: T_PTyIntL
sem_PTyIntL_Nil =
    (let _lhsOself :: PTyIntL
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- PValue ------------------------------------------------------
-- cata
sem_PValue :: PValue ->
              T_PValue
sem_PValue ( x1,x2) =
    (sem_PValue_Tuple (sem_Value x1) (sem_Value x2))
-- semantic domain
type T_PValue = ( PValue)
data Inh_PValue = Inh_PValue {}
data Syn_PValue = Syn_PValue {self_Syn_PValue :: PValue}
wrap_PValue :: T_PValue ->
               Inh_PValue ->
               Syn_PValue
wrap_PValue sem (Inh_PValue) =
    (let ( _lhsOself) = sem
     in  (Syn_PValue _lhsOself))
sem_PValue_Tuple :: T_Value ->
                    T_Value ->
                    T_PValue
sem_PValue_Tuple x1_ x2_ =
    (let _lhsOself :: PValue
         _x1Omts :: TypeEnv
         _x1Otn :: String
         _x1Oval :: (Map.Map Id (Type, [PC]))
         _x2Omts :: TypeEnv
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
         _x1Omts =
             ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: PValue.Tuple.x1.mts"
              {-# LINE 13053 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x1Otn =
             ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: PValue.Tuple.x1.tn"
              {-# LINE 13058 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x1Oval =
             ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: PValue.Tuple.x1.val"
              {-# LINE 13063 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x2Omts =
             ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
              _x1Imts
              {-# LINE 13068 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x2Otn =
             ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: PValue.Tuple.x2.tn"
              {-# LINE 13073 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x2Oval =
             ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: PValue.Tuple.x2.val"
              {-# LINE 13078 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         ( _x1Iident,_x1IisGlobal,_x1Imts,_x1Ipsexpr,_x1Iself,_x1Isexpr,_x1Isexprs,_x1Isort,_x1Ivtype) =
             x1_ _x1Omts _x1Otn _x1Oval
         ( _x2Iident,_x2IisGlobal,_x2Imts,_x2Ipsexpr,_x2Iself,_x2Isexpr,_x2Isexprs,_x2Isort,_x2Ivtype) =
             x2_ _x2Omts _x2Otn _x2Oval
     in  ( _lhsOself))
-- PValueIdx ---------------------------------------------------
-- cata
sem_PValueIdx :: PValueIdx ->
                 T_PValueIdx
sem_PValueIdx ( x1,x2) =
    (sem_PValueIdx_Tuple (sem_Value x1) x2)
-- semantic domain
type T_PValueIdx = ( PValueIdx)
data Inh_PValueIdx = Inh_PValueIdx {}
data Syn_PValueIdx = Syn_PValueIdx {self_Syn_PValueIdx :: PValueIdx}
wrap_PValueIdx :: T_PValueIdx ->
                  Inh_PValueIdx ->
                  Syn_PValueIdx
wrap_PValueIdx sem (Inh_PValueIdx) =
    (let ( _lhsOself) = sem
     in  (Syn_PValueIdx _lhsOself))
sem_PValueIdx_Tuple :: T_Value ->
                       Int ->
                       T_PValueIdx
sem_PValueIdx_Tuple x1_ x2_ =
    (let _lhsOself :: PValueIdx
         _x1Omts :: TypeEnv
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
         _x1Omts =
             ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: PValueIdx.Tuple.x1.mts"
              {-# LINE 13125 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x1Otn =
             ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: PValueIdx.Tuple.x1.tn"
              {-# LINE 13130 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _x1Oval =
             ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: PValueIdx.Tuple.x1.val"
              {-# LINE 13135 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         ( _x1Iident,_x1IisGlobal,_x1Imts,_x1Ipsexpr,_x1Iself,_x1Isexpr,_x1Isexprs,_x1Isort,_x1Ivtype) =
             x1_ _x1Omts _x1Otn _x1Oval
     in  ( _lhsOself))
-- PValues -----------------------------------------------------
-- cata
sem_PValues :: PValues ->
               T_PValues
sem_PValues list =
    (Prelude.foldr sem_PValues_Cons sem_PValues_Nil (Prelude.map sem_PValue list))
-- semantic domain
type T_PValues = ( PValues)
data Inh_PValues = Inh_PValues {}
data Syn_PValues = Syn_PValues {self_Syn_PValues :: PValues}
wrap_PValues :: T_PValues ->
                Inh_PValues ->
                Syn_PValues
wrap_PValues sem (Inh_PValues) =
    (let ( _lhsOself) = sem
     in  (Syn_PValues _lhsOself))
sem_PValues_Cons :: T_PValue ->
                    T_PValues ->
                    T_PValues
sem_PValues_Cons hd_ tl_ =
    (let _lhsOself :: PValues
         _hdIself :: PValue
         _tlIself :: PValues
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_PValues_Nil :: T_PValues
sem_PValues_Nil =
    (let _lhsOself :: PValues
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Parameter ---------------------------------------------------
-- cata
sem_Parameter :: Parameter ->
                 T_Parameter
sem_Parameter (Parameter _var _ty) =
    (sem_Parameter_Parameter (sem_Identifier _var) (sem_Type _ty))
-- semantic domain
type T_Parameter = TypeEnv ->
                   String ->
                   ( String,TypeEnv,Parameter,SExpr,SExpressions)
data Inh_Parameter = Inh_Parameter {mts_Inh_Parameter :: TypeEnv,tn_Inh_Parameter :: String}
data Syn_Parameter = Syn_Parameter {ident_Syn_Parameter :: String,mts_Syn_Parameter :: TypeEnv,self_Syn_Parameter :: Parameter,sexpr_Syn_Parameter :: SExpr,sexprs_Syn_Parameter :: SExpressions}
wrap_Parameter :: T_Parameter ->
                  Inh_Parameter ->
                  Syn_Parameter
wrap_Parameter sem (Inh_Parameter _lhsImts _lhsItn) =
    (let ( _lhsOident,_lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs) = sem _lhsImts _lhsItn
     in  (Syn_Parameter _lhsOident _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs))
sem_Parameter_Parameter :: T_Identifier ->
                           T_Type ->
                           T_Parameter
sem_Parameter_Parameter var_ ty_ =
    (\ _lhsImts
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
                  ({-# LINE 243 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   IdentExpr $ SymIdent _sym
                   {-# LINE 13226 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sym =
                  ({-# LINE 244 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   SimpleSym $ _lhsItn ++ (getIdName _varIself)
                   {-# LINE 13231 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 245 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn ++ (getIdName _varIself)
                   {-# LINE 13236 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 246 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 13241 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 247 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 13246 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 248 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   [ declfun _sym     _tyIsort ] ++ _tyIsexprs
                   {-# LINE 13251 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 249 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 13256 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Parameter _varIself _tyIself
              _lhsOself =
                  _self
              _varOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Parameter.Parameter.var.sortexpr"
                   {-# LINE 13265 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _varOtn =
                  ({-# LINE 12 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 13270 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
type T_Parameters = ( Parameters)
data Inh_Parameters = Inh_Parameters {}
data Syn_Parameters = Syn_Parameters {self_Syn_Parameters :: Parameters}
wrap_Parameters :: T_Parameters ->
                   Inh_Parameters ->
                   Syn_Parameters
wrap_Parameters sem (Inh_Parameters) =
    (let ( _lhsOself) = sem
     in  (Syn_Parameters _lhsOself))
sem_Parameters_Cons :: T_Parameter ->
                       T_Parameters ->
                       T_Parameters
sem_Parameters_Cons hd_ tl_ =
    (let _lhsOself :: Parameters
         _hdOmts :: TypeEnv
         _hdOtn :: String
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
             ({-# LINE 235 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: Parameters.Cons.hd.mts"
              {-# LINE 13313 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _hdOtn =
             ({-# LINE 238 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: Parameters.Cons.hd.tn"
              {-# LINE 13318 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         ( _hdIident,_hdImts,_hdIself,_hdIsexpr,_hdIsexprs) =
             hd_ _hdOmts _hdOtn
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Parameters_Nil :: T_Parameters
sem_Parameters_Nil =
    (let _lhsOself :: Parameters
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- RealPredicate -----------------------------------------------
-- cata
sem_RealPredicate :: RealPredicate ->
                     T_RealPredicate
sem_RealPredicate (LLVMRealOEQ) =
    (sem_RealPredicate_LLVMRealOEQ)
sem_RealPredicate (LLVMRealOGE) =
    (sem_RealPredicate_LLVMRealOGE)
sem_RealPredicate (LLVMRealOGT) =
    (sem_RealPredicate_LLVMRealOGT)
sem_RealPredicate (LLVMRealOLE) =
    (sem_RealPredicate_LLVMRealOLE)
sem_RealPredicate (LLVMRealOLT) =
    (sem_RealPredicate_LLVMRealOLT)
sem_RealPredicate (LLVMRealONE) =
    (sem_RealPredicate_LLVMRealONE)
sem_RealPredicate (LLVMRealORD) =
    (sem_RealPredicate_LLVMRealORD)
sem_RealPredicate (LLVMRealPredicateFalse) =
    (sem_RealPredicate_LLVMRealPredicateFalse)
sem_RealPredicate (LLVMRealPredicateTrue) =
    (sem_RealPredicate_LLVMRealPredicateTrue)
sem_RealPredicate (LLVMRealUEQ) =
    (sem_RealPredicate_LLVMRealUEQ)
sem_RealPredicate (LLVMRealUGE) =
    (sem_RealPredicate_LLVMRealUGE)
sem_RealPredicate (LLVMRealUGT) =
    (sem_RealPredicate_LLVMRealUGT)
sem_RealPredicate (LLVMRealULE) =
    (sem_RealPredicate_LLVMRealULE)
sem_RealPredicate (LLVMRealULT) =
    (sem_RealPredicate_LLVMRealULT)
sem_RealPredicate (LLVMRealUNE) =
    (sem_RealPredicate_LLVMRealUNE)
sem_RealPredicate (LLVMRealUNO) =
    (sem_RealPredicate_LLVMRealUNO)
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
sem_RealPredicate_LLVMRealOEQ :: T_RealPredicate
sem_RealPredicate_LLVMRealOEQ =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealOEQ
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
sem_RealPredicate_LLVMRealOGT :: T_RealPredicate
sem_RealPredicate_LLVMRealOGT =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealOGT
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
sem_RealPredicate_LLVMRealOLT :: T_RealPredicate
sem_RealPredicate_LLVMRealOLT =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealOLT
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
sem_RealPredicate_LLVMRealPredicateFalse :: T_RealPredicate
sem_RealPredicate_LLVMRealPredicateFalse =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealPredicateFalse
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
sem_RealPredicate_LLVMRealUEQ :: T_RealPredicate
sem_RealPredicate_LLVMRealUEQ =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealUEQ
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
sem_RealPredicate_LLVMRealUGT :: T_RealPredicate
sem_RealPredicate_LLVMRealUGT =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealUGT
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
sem_RealPredicate_LLVMRealULT :: T_RealPredicate
sem_RealPredicate_LLVMRealULT =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealULT
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
sem_RealPredicate_LLVMRealUNO :: T_RealPredicate
sem_RealPredicate_LLVMRealUNO =
    (let _lhsOself :: RealPredicate
         _self =
             LLVMRealUNO
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
type T_RetInst = ( RetInst)
data Inh_RetInst = Inh_RetInst {}
data Syn_RetInst = Syn_RetInst {self_Syn_RetInst :: RetInst}
wrap_RetInst :: T_RetInst ->
                Inh_RetInst ->
                Syn_RetInst
wrap_RetInst sem (Inh_RetInst) =
    (let ( _lhsOself) = sem
     in  (Syn_RetInst _lhsOself))
sem_RetInst_ValueRet :: T_Value ->
                        T_RetInst
sem_RetInst_ValueRet v_ =
    (let _lhsOself :: RetInst
         _vOmts :: TypeEnv
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
         _vOmts =
             ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: RetInst.ValueRet.v.mts"
              {-# LINE 13548 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _vOtn =
             ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: RetInst.ValueRet.v.tn"
              {-# LINE 13553 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _vOval =
             ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
              error "missing rule: RetInst.ValueRet.v.val"
              {-# LINE 13558 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         ( _vIident,_vIisGlobal,_vImts,_vIpsexpr,_vIself,_vIsexpr,_vIsexprs,_vIsort,_vIvtype) =
             v_ _vOmts _vOtn _vOval
     in  ( _lhsOself))
sem_RetInst_VoidRet :: T_RetInst
sem_RetInst_VoidRet =
    (let _lhsOself :: RetInst
         _self =
             VoidRet
         _lhsOself =
             _self
     in  ( _lhsOself))
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
sem_SimpleConstant (ConstantFP _fp) =
    (sem_SimpleConstant_ConstantFP (sem_ConstantFP _fp))
sem_SimpleConstant (ConstantInt _iv _ty) =
    (sem_SimpleConstant_ConstantInt _iv (sem_Type _ty))
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
                  ({-# LINE 114 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _fpIsexpr
                   {-# LINE 13643 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 115 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _fpIsexprs
                   {-# LINE 13648 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 116 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _fpIvtype
                   {-# LINE 13653 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 117 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _fpIsort
                   {-# LINE 13658 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantFP _fpIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _fpImts
                   {-# LINE 13667 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fpOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 13672 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fpOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 13677 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _fpOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 13682 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _fpImts,_fpIself,_fpIsexpr,_fpIsexprs,_fpIsort,_fpIvtype) =
                  fp_ _fpOmts _fpOtn _fpOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
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
                  ({-# LINE 120 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 13710 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 121 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 13715 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 122 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   [ IdentExpr $ IdxIdent (bv iv_) [getISize _tyIself] ]
                   {-# LINE 13720 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 123 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 13725 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 124 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 13730 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 125 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 13735 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantInt iv_ _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 13744 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
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
                  ({-# LINE 127 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 13771 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 128 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 13776 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 129 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 13781 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 130 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 13786 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 131 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 13791 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 132 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 13796 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  ConstantPointerNull _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 13805 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- Target ------------------------------------------------------
-- cata
sem_Target :: Target ->
              T_Target
sem_Target (Linux) =
    (sem_Target_Linux)
sem_Target (MacOs) =
    (sem_Target_MacOs)
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
sem_Target_Linux :: T_Target
sem_Target_Linux =
    (let _lhsOself :: Target
         _self =
             Linux
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Target_MacOs :: T_Target
sem_Target_MacOs =
    (let _lhsOself :: Target
         _self =
             MacOs
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
sem_TyFloatPoint (TyDouble) =
    (sem_TyFloatPoint_TyDouble)
sem_TyFloatPoint (TyFP128) =
    (sem_TyFloatPoint_TyFP128)
sem_TyFloatPoint (TyFloat) =
    (sem_TyFloatPoint_TyFloat)
sem_TyFloatPoint (TyHalf) =
    (sem_TyFloatPoint_TyHalf)
sem_TyFloatPoint (TyPPCFP128) =
    (sem_TyFloatPoint_TyPPCFP128)
sem_TyFloatPoint (Tyx86FP80) =
    (sem_TyFloatPoint_Tyx86FP80)
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
sem_TyFloatPoint_TyFloat :: T_TyFloatPoint
sem_TyFloatPoint_TyFloat =
    (let _lhsOself :: TyFloatPoint
         _self =
             TyFloat
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_TyFloatPoint_TyHalf :: T_TyFloatPoint
sem_TyFloatPoint_TyHalf =
    (let _lhsOself :: TyFloatPoint
         _self =
             TyHalf
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
sem_TyFloatPoint_Tyx86FP80 :: T_TyFloatPoint
sem_TyFloatPoint_Tyx86FP80 =
    (let _lhsOself :: TyFloatPoint
         _self =
             Tyx86FP80
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Type --------------------------------------------------------
-- cata
sem_Type :: Type ->
            T_Type
sem_Type (TyArray _numEl _ty) =
    (sem_Type_TyArray _numEl (sem_Type _ty))
sem_Type (TyFloatPoint _p) =
    (sem_Type_TyFloatPoint (sem_TyFloatPoint _p))
sem_Type (TyFunction _party _retty) =
    (sem_Type_TyFunction (sem_Types _party) (sem_Type _retty))
sem_Type (TyInt _p) =
    (sem_Type_TyInt _p)
sem_Type (TyLabel) =
    (sem_Type_TyLabel)
sem_Type (TyMetadata) =
    (sem_Type_TyMetadata)
sem_Type (TyOpaque) =
    (sem_Type_TyOpaque)
sem_Type (TyPointer _ty) =
    (sem_Type_TyPointer (sem_Type _ty))
sem_Type (TyStruct _name _numEl _tys) =
    (sem_Type_TyStruct _name _numEl (sem_Types _tys))
sem_Type (TyUndefined) =
    (sem_Type_TyUndefined)
sem_Type (TyVector _numEl _ty) =
    (sem_Type_TyVector _numEl (sem_Type _ty))
sem_Type (TyVoid) =
    (sem_Type_TyVoid)
sem_Type (Tyx86MMX) =
    (sem_Type_Tyx86MMX)
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
                  ({-# LINE 62 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   getBSize numEl_
                   {-# LINE 14037 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sortn =
                  ({-# LINE 63 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   fromMaybe ("Array" ++ show numEl_ ++ _tyIsortn) _lhsImn
                   {-# LINE 14042 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 64 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sortn
                   {-# LINE 14047 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 65 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 14052 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 66 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   Map.insert _self (_sort    , _sortn    ) _tyImts
                   {-# LINE 14057 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sort =
                  ({-# LINE 67 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   SymSort _sortn
                   {-# LINE 14062 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 68 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sort
                   {-# LINE 14067 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sortd =
                  ({-# LINE 69 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   ArraySort (BitVector _bsize    ) _tyIsort
                   {-# LINE 14072 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 70 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _tyIsexprs ++ [ SE $ DefSort (SimpleSym _sortn    ) [] $ toSExpr _sortd     ]
                   {-# LINE 14077 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyArray numEl_ _tyIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 14086 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
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
                  ({-# LINE 58 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14105 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 59 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14110 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 60 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14115 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 14120 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyFloatPoint _pIself
              _lhsOself =
                  _self
              ( _pIself) =
                  p_
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyFunction :: T_Types ->
                       T_Type ->
                       T_Type
sem_Type_TyFunction party_ retty_ =
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
                  ({-# LINE 72 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14151 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 73 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14156 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 74 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14161 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _rettyIsexprs
                   {-# LINE 14166 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyFunction _partyIself _rettyIself
              _lhsOself =
                  _self
              _rettyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 14175 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _rettyOmts =
                  ({-# LINE 19 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 14180 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _partyIself) =
                  party_
              ( _rettyImts,_rettyIself,_rettyIsexprs,_rettyIsort,_rettyIsortn) =
                  retty_ _rettyOmn _rettyOmts
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
                  ({-# LINE 45 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   if p_ == 1
                   then fromMaybe "Bool"           _lhsImn
                   else fromMaybe ("I" ++ show p_) _lhsImn
                   {-# LINE 14202 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sortn
                   {-# LINE 14207 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sort =
                  ({-# LINE 49 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   SymSort _sortn
                   {-# LINE 14212 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 50 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sort
                   {-# LINE 14217 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 51 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   Map.insert _self (_sort    , _sortn    ) _lhsImts
                   {-# LINE 14222 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 52 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   case Map.lookup _self _lhsImts of
                        Nothing -> if p_ == 1
                                   then []
                                   else [ defsorti p_ ]
                        Just _  -> []
                   {-# LINE 14231 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyInt p_
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
                  ({-# LINE 33 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14250 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 34 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14255 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 35 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14260 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 14265 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                  ({-# LINE 37 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14284 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 38 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14289 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 39 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14294 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 14299 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                  ({-# LINE 41 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14318 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 42 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14323 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 43 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14328 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 14333 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyOpaque
              _lhsOself =
                  _self
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
                  ({-# LINE 94 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   fromMaybe _tyIsortn _lhsImn
                   {-# LINE 14360 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 95 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sortn
                   {-# LINE 14365 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sort =
                  ({-# LINE 96 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   SymSort _sortn
                   {-# LINE 14370 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 97 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sort
                   {-# LINE 14375 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 98 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 14380 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 99 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   Map.insert _self (_sort    , _sortn    ) _tyImts
                   {-# LINE 14385 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sortd =
                  ({-# LINE 100 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _tyIsort
                   {-# LINE 14390 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 101 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _tyIsexprs
                   {-# LINE 14395 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyPointer _tyIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 14404 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
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
                  ({-# LINE 76 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   fromMaybe name_ _lhsImn
                   {-# LINE 14425 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 77 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sortn
                   {-# LINE 14430 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tysi =
                  ({-# LINE 78 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   case _sortn     of
                        "union.pthread_mutex_t"          -> (_lhsImts, [], SymSort "Bool")
                        "struct.__pthread_mutex_s"       -> (_lhsImts, [], SymSort "Bool")
                        "struct.__pthread_internal_list" -> (_lhsImts, [], SymSort "Bool")
                        n -> encTypes _tysIself _lhsImts
                   {-# LINE 14439 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sort =
                  ({-# LINE 83 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   case _sortn     of
                        "" -> trdu _tysi
                        n  -> SymSort _sortn
                   {-# LINE 14446 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 86 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sort
                   {-# LINE 14451 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 87 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   case _sortn     of
                        "" -> fstu _tysi
                        n  -> Map.insert _self (_sort    , n) $ fstu _tysi
                   {-# LINE 14458 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 90 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   case _sortn     of
                        "" -> sndu _tysi
                        n  -> sndu _tysi     ++ [ SE $ DefSort (SimpleSym _sortn    ) [] $ toSExpr $ trdu _tysi     ]
                   {-# LINE 14465 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyStruct name_ numEl_ _tysIself
              _lhsOself =
                  _self
              ( _tysIself) =
                  tys_
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
                  ({-# LINE 103 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14486 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 104 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14491 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 105 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14496 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 14501 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyUndefined
              _lhsOself =
                  _self
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
                  ({-# LINE 62 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   getBSize numEl_
                   {-# LINE 14529 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sortn =
                  ({-# LINE 63 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   fromMaybe ("Array" ++ show numEl_ ++ _tyIsortn) _lhsImn
                   {-# LINE 14534 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 64 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sortn
                   {-# LINE 14539 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 65 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 14544 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 66 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   Map.insert _self (_sort    , _sortn    ) _tyImts
                   {-# LINE 14549 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sort =
                  ({-# LINE 67 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   SymSort _sortn
                   {-# LINE 14554 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 68 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _sort
                   {-# LINE 14559 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _sortd =
                  ({-# LINE 69 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   ArraySort (BitVector _bsize    ) _tyIsort
                   {-# LINE 14564 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 70 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _tyIsexprs ++ [ SE $ DefSort (SimpleSym _sortn    ) [] $ toSExpr _sortd     ]
                   {-# LINE 14569 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  TyVector numEl_ _tyIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 21 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 14578 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
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
                  ({-# LINE 25 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14595 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 26 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14600 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 27 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14605 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 14610 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
                  ({-# LINE 29 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14629 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 30 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14634 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 31 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   errormessage
                   {-# LINE 14639 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 18 "src/Concurrent/Model/Encoder/Types.ag" #-}
                   []
                   {-# LINE 14644 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Tyx86MMX
              _lhsOself =
                  _self
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
             ({-# LINE 12 "src/Concurrent/Model/Encoder/Types.ag" #-}
              Nothing
              {-# LINE 14683 "src/Concurrent/Model/Encoder/Threads.hs" #-}
              )
         _hdOmts =
             ({-# LINE 13 "src/Concurrent/Model/Encoder/Types.ag" #-}
              Map.empty
              {-# LINE 14688 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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
-- Value -------------------------------------------------------
-- cata
sem_Value :: Value ->
             T_Value
sem_Value (Constant _c) =
    (sem_Value_Constant (sem_Constant _c))
sem_Value (Id _v _ty) =
    (sem_Value_Id (sem_Identifier _v) (sem_Type _ty))
-- semantic domain
type T_Value = TypeEnv ->
               String ->
               (Map.Map Id (Type, [PC])) ->
               ( (Maybe String),Bool,TypeEnv,(Int -> [SExpr]),Value,([SExpr]),SExpressions,SSortExpr,Type)
data Inh_Value = Inh_Value {mts_Inh_Value :: TypeEnv,tn_Inh_Value :: String,val_Inh_Value :: (Map.Map Id (Type, [PC]))}
data Syn_Value = Syn_Value {ident_Syn_Value :: (Maybe String),isGlobal_Syn_Value :: Bool,mts_Syn_Value :: TypeEnv,psexpr_Syn_Value :: (Int -> [SExpr]),self_Syn_Value :: Value,sexpr_Syn_Value :: ([SExpr]),sexprs_Syn_Value :: SExpressions,sort_Syn_Value :: SSortExpr,vtype_Syn_Value :: Type}
wrap_Value :: T_Value ->
              Inh_Value ->
              Syn_Value
wrap_Value sem (Inh_Value _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_Value _lhsOident _lhsOisGlobal _lhsOmts _lhsOpsexpr _lhsOself _lhsOsexpr _lhsOsexprs _lhsOsort _lhsOvtype))
sem_Value_Constant :: T_Constant ->
                      T_Value
sem_Value_Constant c_ =
    (\ _lhsImts
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
              _cOmts :: TypeEnv
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
                  ({-# LINE 76 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIsexpr
                   {-# LINE 14758 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 77 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIpsexpr
                   {-# LINE 14763 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 78 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIsexprs
                   {-# LINE 14768 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 79 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIvtype
                   {-# LINE 14773 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOisGlobal =
                  ({-# LINE 80 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIisGlobal
                   {-# LINE 14778 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 81 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIident
                   {-# LINE 14783 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 82 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cIsort
                   {-# LINE 14788 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Constant _cIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _cImts
                   {-# LINE 14797 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cOmts =
                  ({-# LINE 44 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 14802 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 14807 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _cOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 14812 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _cIident,_cIisGlobal,_cImts,_cIpsexpr,_cIself,_cIsexpr,_cIsexprs,_cIsort,_cIvtype) =
                  c_ _cOmts _cOtn _cOval
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
sem_Value_Id :: T_Identifier ->
                T_Type ->
                T_Value
sem_Value_Id v_ ty_ =
    (\ _lhsImts
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
                  ({-# LINE 59 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 14850 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 60 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Nothing
                   {-# LINE 14855 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 61 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 14860 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 62 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   case Map.lookup _vIident _lhsIval of
                        Nothing -> [ _vIsexpr ]
                        Just (t,lp)  -> Prelude.map (\p -> IdentExpr $ SymIdent $ SimpleSym $ _lhsItn ++ _vIident ++ show p) [0.. length lp]
                   {-# LINE 14867 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOpsexpr =
                  ({-# LINE 65 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   \_ -> case Map.lookup _vIident _lhsIval of
                              Nothing -> [ ]
                              Just (t,lp)  -> Prelude.map (\p -> IdentExpr $ SymIdent $ SimpleSym $ "p" ++ _lhsItn ++ _vIident ++ show p) [0.. length lp]
                   {-# LINE 14874 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 68 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsexprs
                   {-# LINE 14879 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 69 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 14884 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 70 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 14889 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOisGlobal =
                  ({-# LINE 71 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   False
                   {-# LINE 14894 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 72 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   Just _vIident
                   {-# LINE 14899 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 73 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tyIsort
                   {-# LINE 14904 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  Id _vIself _tyIself
              _lhsOself =
                  _self
              _vOsortexpr =
                  ({-# LINE 17 "src/Concurrent/Model/Encoder/Identifier.ag" #-}
                   error "missing rule: Value.Id.v.sortexpr"
                   {-# LINE 14913 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _vIdeclexpr,_vIident,_vIself,_vIsexpr,_vIssymbol) =
                  v_ _vOsortexpr _vOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOident,_lhsOisGlobal,_lhsOmts,_lhsOpsexpr,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOsort,_lhsOvtype)))
-- ValueIdxs ---------------------------------------------------
-- cata
sem_ValueIdxs :: ValueIdxs ->
                 T_ValueIdxs
sem_ValueIdxs list =
    (Prelude.foldr sem_ValueIdxs_Cons sem_ValueIdxs_Nil (Prelude.map sem_PValueIdx list))
-- semantic domain
type T_ValueIdxs = ( ValueIdxs)
data Inh_ValueIdxs = Inh_ValueIdxs {}
data Syn_ValueIdxs = Syn_ValueIdxs {self_Syn_ValueIdxs :: ValueIdxs}
wrap_ValueIdxs :: T_ValueIdxs ->
                  Inh_ValueIdxs ->
                  Syn_ValueIdxs
wrap_ValueIdxs sem (Inh_ValueIdxs) =
    (let ( _lhsOself) = sem
     in  (Syn_ValueIdxs _lhsOself))
sem_ValueIdxs_Cons :: T_PValueIdx ->
                      T_ValueIdxs ->
                      T_ValueIdxs
sem_ValueIdxs_Cons hd_ tl_ =
    (let _lhsOself :: ValueIdxs
         _hdIself :: PValueIdx
         _tlIself :: ValueIdxs
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_ValueIdxs_Nil :: T_ValueIdxs
sem_ValueIdxs_Nil =
    (let _lhsOself :: ValueIdxs
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Values ------------------------------------------------------
-- cata
sem_Values :: Values ->
              T_Values
sem_Values list =
    (Prelude.foldr sem_Values_Cons sem_Values_Nil (Prelude.map sem_Value list))
-- semantic domain
type T_Values = TypeEnv ->
                String ->
                (Map.Map Id (Type, [PC])) ->
                ( TypeEnv,Values,([SExpr]),SExpressions,([Type]))
data Inh_Values = Inh_Values {mts_Inh_Values :: TypeEnv,tn_Inh_Values :: String,val_Inh_Values :: (Map.Map Id (Type, [PC]))}
data Syn_Values = Syn_Values {mts_Syn_Values :: TypeEnv,self_Syn_Values :: Values,sexpr_Syn_Values :: ([SExpr]),sexprs_Syn_Values :: SExpressions,vtype_Syn_Values :: ([Type])}
wrap_Values :: T_Values ->
               Inh_Values ->
               Syn_Values
wrap_Values sem (Inh_Values _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_Values _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOvtype))
sem_Values_Cons :: T_Value ->
                   T_Values ->
                   T_Values
sem_Values_Cons hd_ tl_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _hdOmts :: TypeEnv
              _tlOmts :: TypeEnv
              _lhsOmts :: TypeEnv
              _lhsOvtype :: ([Type])
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOself :: Values
              _hdOtn :: String
              _hdOval :: (Map.Map Id (Type, [PC]))
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
                  ({-# LINE 38 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 15014 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 39 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _hdImts
                   {-# LINE 15019 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 40 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _tlImts
                   {-# LINE 15024 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 41 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _hdIvtype:(_tlIvtype)
                   {-# LINE 15029 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 31 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _hdIsexpr ++ _tlIsexpr
                   {-# LINE 15034 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 30 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _hdIsexprs ++ _tlIsexprs
                   {-# LINE 15039 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOtn =
                  ({-# LINE 47 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 15048 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _hdOval =
                  ({-# LINE 48 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 15053 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 29 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 15058 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 28 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 15063 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              ( _hdIident,_hdIisGlobal,_hdImts,_hdIpsexpr,_hdIself,_hdIsexpr,_hdIsexprs,_hdIsort,_hdIvtype) =
                  hd_ _hdOmts _hdOtn _hdOval
              ( _tlImts,_tlIself,_tlIsexpr,_tlIsexprs,_tlIvtype) =
                  tl_ _tlOmts _tlOtn _tlOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Values_Nil :: T_Values
sem_Values_Nil =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOmts :: TypeEnv
              _lhsOsexpr :: ([SExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: ([Type])
              _lhsOself :: Values
              _lhsOmts =
                  ({-# LINE 36 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 15083 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 31 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 15088 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 30 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 15093 "src/Concurrent/Model/Encoder/Threads.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 32 "src/Concurrent/Model/Encoder/Value.ag" #-}
                   []
                   {-# LINE 15098 "src/Concurrent/Model/Encoder/Threads.hs" #-}
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