

{-#LANGUAGE RecordWildCards #-}
-- UUAGC 0.9.40.3 (src/Concurrent/Model/ESEncoder/Model.ag)
module Concurrent.Model.ESEncoder.Model where

{-# LINE 24 "src/Concurrent/Model/ESEncoder/Model.ag" #-}

import Language.SMTLib2.Base
import Language.SMTLib2.Builder

import Language.LLVMIR

import Concurrent.Model
import Concurrent.Model.Analysis.ControlFlow (ControlFlow(..))
import Concurrent.Model.Analysis.DataFlow    (DataFlow(..))

import Data.Char
import Data.Maybe
import Data.List (find,nub)

import Numeric

import Debug.Trace (trace)

{-# LINE 27 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 11 "src/Language/LLVMIR/Grammar/Base.ag" #-}

import Prelude              hiding (sequence)
import Data.Char            (chr)
import qualified Data.Map as Data.Map
import qualified Data.Map as Map
import Data.Map
{-# LINE 36 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 11 "src/Concurrent/Model/ESEncoder/Global.ag" #-}

import Control.Applicative ((<$>))
import Control.Monad       (mplus)
{-# LINE 42 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
{-# LINE 5 "src/Concurrent/Model/ESEncoder/Model.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.ESEncoder.Model
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 49 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Grammar/Base.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Base
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 57 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Grammar/Type.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Type
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 65 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 1 "src/Concurrent/Model/ESEncoder/Types.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.ESEncoder.Types
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 73 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 95 "src/Concurrent/Model/ESEncoder/Types.ag" #-}

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

getISize :: Type -> Int
getISize (TyInt p) = p
getISize (TyPointer t) = getISize t
getISize _ = 0

-- TODO - Define a new sort for each element of the struct
encTypes :: Types -> Map.Map Type SSort -> (Map.Map Type SSort, SExpressions, SSortExpr) 
encTypes []     _   = error "empty struct"
encTypes [x]    mts = encType x Nothing mts
encTypes (x:xs) mts = let (mts', sexprs, sort) = encType x Nothing mts
                      in  encTypes' (sexprs,sort) xs mts' 
  where encTypes' (sexprs,ssort) []     mts = (mts, sexprs, ssort)
        encTypes' (sexprs,ssort) (x:xs) mts = let (mts', sexprs', ssort') = encType x Nothing mts
                                              in  encTypes' (sexprs ++ sexprs', PairSort ssort ssort') xs mts'

encType :: Type -> Maybe SSort -> Map.Map Type SSort -> (Map.Map Type SSort, SExpressions, SSortExpr)
encType ty s mts = let tw = wrap_Type (sem_Type ty) $ Inh_Type { mn_Inh_Type = s, mts_Inh_Type = mts }
                   in case Map.lookup ty mts of
                           Nothing -> trace (show (mts_Syn_Type tw, sexprs_Syn_Type tw, sort_Syn_Type tw)) $ (mts_Syn_Type tw, sexprs_Syn_Type tw, sort_Syn_Type tw) 
                           Just tsn  -> case s of
                                        Nothing -> (mts, [], SymSort tsn)
                                        Just sn -> if sn == tsn
                                                   then (mts, [], SymSort sn)
                                                   else (mts, [ defsort sn tsn ], SymSort sn)

{-# LINE 117 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 1 "src/Concurrent/Model/ESEncoder/Global.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.ESEncoder.Global
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 125 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 44 "src/Concurrent/Model/ESEncoder/Global.ag" #-}



sAnd :: ISExpr -> ISExpr -> ISExpr
sAnd a@(ISExpr _) b@(ISExpr _) = sFn "and" a b
sAnd ISEmpty      y            = y
sAnd x            ISEmpty      = x
sAnd _            _            = error "sAnd"

sOr :: ISExpr -> ISExpr -> ISExpr
sOr a@(ISExpr _) b@(ISExpr _) = sFn "or" a b
sOr ISEmpty      y            = y
sOr x            ISEmpty      = x
sOr _            _            = error "sOr"

sFn :: String -> ISExpr -> ISExpr -> ISExpr
sFn f s1 s2 = ISExpr $ FnAppExpr (SymIdent $ SimpleSym f) [fromISExpr s1, fromISExpr s2]

-- | Encode Global Variables
encGlobalVars :: Globals -> GlobalState -> (GlobalState, SExpressions)
encGlobalVars gvars gs = let gw = wrap_Globals (sem_Globals gvars) $ Inh_Globals { gs_Inh_Globals = gs }
                             me  = case sexpr_Syn_Globals gw of
                                        ISEmpty  -> []
                                        ISExpr e -> [assert e]
                                        _        -> error "encGlobalVars" 
                         in (gs_Syn_Globals gw, sexprs_Syn_Globals gw ++ me)

{-# LINE 155 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 1 "src/Concurrent/Model/ESEncoder/Value.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.ESEncoder.Value
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 163 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 167 "src/Concurrent/Model/ESEncoder/Value.ag" #-}

init' :: [a] -> [a]
init' [] = []
init' [x] = [x]
init' [x,y] = [x]
init' (x:y:ys) = x:(init' (y:ys))

verrormessage = error "value instance not supported"

bv :: Int -> SSymbol
bv n = SimpleSym $ "bv" ++ show n

changeN :: ISExpr -> Int -> ISExpr
changeN (ISExpr (IdentExpr (IdxIdent s _))) n = ISExpr $ IdentExpr $ IdxIdent s [n]
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


encValue :: Value -> Map.Map Type SSort -> Valuation -> String -> (Map.Map Type SSort, SExpressions, ISExpr) 
encValue v mts val tn = let vw = wrap_Value (sem_Value v) $ Inh_Value {mts_Inh_Value = mts, tn_Inh_Value = tn, val_Inh_Value = val}
                        in (mts_Syn_Value vw, sexprs_Syn_Value vw, sexpr_Syn_Value vw)

encParameter :: Parameter -> Map.Map Type SSort -> String -> (Map.Map Type SSort, SExpressions, ISExpr)
encParameter p mts tn = let pw = wrap_Parameter (sem_Parameter p) $ Inh_Parameter {mts_Inh_Parameter = mts, tn_Inh_Parameter = tn}
                        in (mts_Syn_Parameter pw, sexprs_Syn_Parameter pw, sexpr_Syn_Parameter pw)


getValueType :: Value -> Type 
getValueType v = vtype_Syn_Value $ wrap_Value (sem_Value v) $ Inh_Value {mts_Inh_Value = undefined, tn_Inh_Value = undefined, val_Inh_Value = undefined}

getFnValueName :: Value -> Id
getFnValueName (Constant (GlobalValue (FunctionValue (Global n) _))) = n
getFnValueName _ = error "getFnValueName failed"
{-# LINE 209 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 1 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.ESEncoder.Identifier
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 217 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 29 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}


freshId :: Id -> Id
freshId x = x ++ "0"

{-# LINE 225 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 1 "src/Concurrent/Model/ESEncoder/Function.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.ESEncoder.Function
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 233 "src/Concurrent/Model/ESEncoder/Model.hs" #-}

{-# LINE 153 "src/Concurrent/Model/ESEncoder/Function.ag" #-}

nextpc :: PC -> [(PC,PC)] -> [PC]
nextpc i = snd . unzip . Prelude.filter (\(p,r) -> p == i && r /= -1)

updateSPC :: GlobalState -> String -> PC -> GlobalState
updateSPC gs@GlobalState{..} n pc = let nti = Map.adjust (\ts@ThreadState{..} -> ts { tipc = pc }) n ti  
                                    in gs { currentpc = pc, ti = nti }
                                         

-- | Returns the deep value of a Value
value :: Valuation -> Value -> (Maybe Id, Value)
value vals v = case getGValueId v of
                    Nothing -> (Nothing, v)
                    Just i  -> (ivalueId vals i, ivalue vals i)

ivalue :: Valuation -> Id -> Value
ivalue vals i = case Map.lookup i vals of
                     Nothing        -> error $ "Global var " ++ show i ++ " not found in the env"
                     Just (Right v) -> v
                     Just (Left  j) -> ivalue vals j

ivalueId :: Valuation -> Id -> Maybe Id
ivalueId vals i = case Map.lookup i vals of
                       Nothing        -> Nothing -- error $ "Global var " ++ show i ++ " not found in the env"
                       Just (Right v) -> Just i
                       Just (Left  j) -> case ivalueId vals j of
                                              Nothing -> Just j
                                              Just h  -> Just h

upValuation :: Valuation -> Value -> Value -> Valuation
upValuation vals v1 v2 = case getValueId v1 of
                              Nothing -> vals -- error "no name"
                              Just i  -> Map.insert i (Right v2) vals

-- | Get return type 
getFnRetTy :: Declarations -> Id -> Type
getFnRetTy dcls i = case Map.lookup i dcls of
                         Nothing    -> error "Function unknow"
                         Just (t,_) -> t

getFnParams :: Declarations -> Id -> Parameters
getFnParams dcls i = case Map.lookup i dcls of
                          Nothing -> error "getFnParams error"
                          Just (_,p) -> p

-- | Get Transitions
encTransitions :: Functions -> ControlFlow -> DataFlow -> Declarations -> Map.Map String Transitions
encTransitions fs ccfg cdfg fdcl = cts_Syn_Functions $ wrap_Functions (sem_Functions fs) $ Inh_Functions {ccfg_Inh_Functions = ccfg, cdfg_Inh_Functions = cdfg, fdcl_Inh_Functions = fdcl}

{-# LINE 285 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
         _argOmts :: (Map.Map Type SSort)
         _argOtn :: String
         _argOval :: Valuation
         _argImts :: (Map.Map Type SSort)
         _argIself :: Value
         _argIsexpr :: ISExpr
         _argIsexprs :: SExpressions
         _argIvtype :: Type
         _self =
             Argument _argIself
         _lhsOself =
             _self
         _argOmts =
             ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: Argument.Argument.arg.mts"
              {-# LINE 414 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _argOtn =
             ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: Argument.Argument.arg.tn"
              {-# LINE 419 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _argOval =
             ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: Argument.Argument.arg.val"
              {-# LINE 424 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         ( _argImts,_argIself,_argIsexpr,_argIsexprs,_argIvtype) =
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
type T_BasicBlock = ([(PC,PC)]) ->
                    (Map.Map Identifier Type) ->
                    Declarations ->
                    Id ->
                    ( ([Id]),BasicBlock,Transitions)
data Inh_BasicBlock = Inh_BasicBlock {cfg_Inh_BasicBlock :: ([(PC,PC)]),dfg_Inh_BasicBlock :: (Map.Map Identifier Type),fdcl_Inh_BasicBlock :: Declarations,tn_Inh_BasicBlock :: Id}
data Syn_BasicBlock = Syn_BasicBlock {locals_Syn_BasicBlock :: ([Id]),self_Syn_BasicBlock :: BasicBlock,ts_Syn_BasicBlock :: Transitions}
wrap_BasicBlock :: T_BasicBlock ->
                   Inh_BasicBlock ->
                   Syn_BasicBlock
wrap_BasicBlock sem (Inh_BasicBlock _lhsIcfg _lhsIdfg _lhsIfdcl _lhsItn) =
    (let ( _lhsOlocals,_lhsOself,_lhsOts) = sem _lhsIcfg _lhsIdfg _lhsIfdcl _lhsItn
     in  (Syn_BasicBlock _lhsOlocals _lhsOself _lhsOts))
sem_BasicBlock_BasicBlock :: T_Label ->
                             T_Instructions ->
                             T_BasicBlock
sem_BasicBlock_BasicBlock label_ instrs_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOts :: Transitions
              _lhsOlocals :: ([Id])
              _lhsOself :: BasicBlock
              _instrsOcfg :: ([(PC,PC)])
              _instrsOdfg :: (Map.Map Identifier Type)
              _instrsOfdcl :: Declarations
              _instrsOtn :: Id
              _labelIself :: Label
              _instrsIlocals :: ([Id])
              _instrsIself :: Instructions
              _instrsIts :: Transitions
              _lhsOts =
                  ({-# LINE 43 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _instrsIts
                   {-# LINE 849 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _instrsIlocals
                   {-# LINE 854 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  BasicBlock _labelIself _instrsIself
              _lhsOself =
                  _self
              _instrsOcfg =
                  ({-# LINE 33 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIcfg
                   {-# LINE 863 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _instrsOdfg =
                  ({-# LINE 35 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIdfg
                   {-# LINE 868 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _instrsOfdcl =
                  ({-# LINE 34 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIfdcl
                   {-# LINE 873 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _instrsOtn =
                  ({-# LINE 36 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsItn
                   {-# LINE 878 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _labelIself) =
                  label_
              ( _instrsIlocals,_instrsIself,_instrsIts) =
                  instrs_ _instrsOcfg _instrsOdfg _instrsOfdcl _instrsOtn
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
-- BasicBlocks -------------------------------------------------
-- cata
sem_BasicBlocks :: BasicBlocks ->
                   T_BasicBlocks
sem_BasicBlocks list =
    (Prelude.foldr sem_BasicBlocks_Cons sem_BasicBlocks_Nil (Prelude.map sem_BasicBlock list))
-- semantic domain
type T_BasicBlocks = ([(PC,PC)]) ->
                     (Map.Map Identifier Type) ->
                     Declarations ->
                     Id ->
                     ( ([Id]),BasicBlocks,Transitions)
data Inh_BasicBlocks = Inh_BasicBlocks {cfg_Inh_BasicBlocks :: ([(PC,PC)]),dfg_Inh_BasicBlocks :: (Map.Map Identifier Type),fdcl_Inh_BasicBlocks :: Declarations,tn_Inh_BasicBlocks :: Id}
data Syn_BasicBlocks = Syn_BasicBlocks {locals_Syn_BasicBlocks :: ([Id]),self_Syn_BasicBlocks :: BasicBlocks,ts_Syn_BasicBlocks :: Transitions}
wrap_BasicBlocks :: T_BasicBlocks ->
                    Inh_BasicBlocks ->
                    Syn_BasicBlocks
wrap_BasicBlocks sem (Inh_BasicBlocks _lhsIcfg _lhsIdfg _lhsIfdcl _lhsItn) =
    (let ( _lhsOlocals,_lhsOself,_lhsOts) = sem _lhsIcfg _lhsIdfg _lhsIfdcl _lhsItn
     in  (Syn_BasicBlocks _lhsOlocals _lhsOself _lhsOts))
sem_BasicBlocks_Cons :: T_BasicBlock ->
                        T_BasicBlocks ->
                        T_BasicBlocks
sem_BasicBlocks_Cons hd_ tl_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: BasicBlocks
              _hdOcfg :: ([(PC,PC)])
              _hdOdfg :: (Map.Map Identifier Type)
              _hdOfdcl :: Declarations
              _hdOtn :: Id
              _tlOcfg :: ([(PC,PC)])
              _tlOdfg :: (Map.Map Identifier Type)
              _tlOfdcl :: Declarations
              _tlOtn :: Id
              _hdIlocals :: ([Id])
              _hdIself :: BasicBlock
              _hdIts :: Transitions
              _tlIlocals :: ([Id])
              _tlIself :: BasicBlocks
              _tlIts :: Transitions
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _hdIlocals ++ _tlIlocals
                   {-# LINE 933 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _hdIts ++ _tlIts
                   {-# LINE 938 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOcfg =
                  ({-# LINE 33 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIcfg
                   {-# LINE 947 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _hdOdfg =
                  ({-# LINE 35 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIdfg
                   {-# LINE 952 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _hdOfdcl =
                  ({-# LINE 34 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIfdcl
                   {-# LINE 957 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 36 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsItn
                   {-# LINE 962 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOcfg =
                  ({-# LINE 33 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIcfg
                   {-# LINE 967 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOdfg =
                  ({-# LINE 35 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIdfg
                   {-# LINE 972 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOfdcl =
                  ({-# LINE 34 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIfdcl
                   {-# LINE 977 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 36 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsItn
                   {-# LINE 982 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _hdIlocals,_hdIself,_hdIts) =
                  hd_ _hdOcfg _hdOdfg _hdOfdcl _hdOtn
              ( _tlIlocals,_tlIself,_tlIts) =
                  tl_ _tlOcfg _tlOdfg _tlOfdcl _tlOtn
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_BasicBlocks_Nil :: T_BasicBlocks
sem_BasicBlocks_Nil =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: BasicBlocks
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 1001 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 1006 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
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
type T_CompareConstantExpr = ( CompareConstantExpr)
data Inh_CompareConstantExpr = Inh_CompareConstantExpr {}
data Syn_CompareConstantExpr = Syn_CompareConstantExpr {self_Syn_CompareConstantExpr :: CompareConstantExpr}
wrap_CompareConstantExpr :: T_CompareConstantExpr ->
                            Inh_CompareConstantExpr ->
                            Syn_CompareConstantExpr
wrap_CompareConstantExpr sem (Inh_CompareConstantExpr) =
    (let ( _lhsOself) = sem
     in  (Syn_CompareConstantExpr _lhsOself))
sem_CompareConstantExpr_FCmpExpr :: T_RealPredicate ->
                                    T_Type ->
                                    T_Value ->
                                    T_Value ->
                                    T_CompareConstantExpr
sem_CompareConstantExpr_FCmpExpr cond_ ty_ op1_ op2_ =
    (let _lhsOself :: CompareConstantExpr
         _tyOmn :: (Maybe SSort)
         _tyOmts :: (Map.Map Type SSort)
         _op1Omts :: (Map.Map Type SSort)
         _op1Otn :: String
         _op1Oval :: Valuation
         _op2Omts :: (Map.Map Type SSort)
         _op2Otn :: String
         _op2Oval :: Valuation
         _condIself :: RealPredicate
         _tyImts :: (Map.Map Type SSort)
         _tyIself :: Type
         _tyIsexprs :: SExpressions
         _tyIsort :: SSortExpr
         _tyIsortn :: SSort
         _op1Imts :: (Map.Map Type SSort)
         _op1Iself :: Value
         _op1Isexpr :: ISExpr
         _op1Isexprs :: SExpressions
         _op1Ivtype :: Type
         _op2Imts :: (Map.Map Type SSort)
         _op2Iself :: Value
         _op2Isexpr :: ISExpr
         _op2Isexprs :: SExpressions
         _op2Ivtype :: Type
         _self =
             FCmpExpr _condIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         _tyOmn =
             ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
              error "missing rule: CompareConstantExpr.FCmpExpr.ty.mn"
              {-# LINE 1258 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _tyOmts =
             ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
              error "missing rule: CompareConstantExpr.FCmpExpr.ty.mts"
              {-# LINE 1263 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _op1Omts =
             ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              _tyImts
              {-# LINE 1268 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _op1Otn =
             ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: CompareConstantExpr.FCmpExpr.op1.tn"
              {-# LINE 1273 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _op1Oval =
             ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: CompareConstantExpr.FCmpExpr.op1.val"
              {-# LINE 1278 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _op2Omts =
             ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              _op1Imts
              {-# LINE 1283 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _op2Otn =
             ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: CompareConstantExpr.FCmpExpr.op2.tn"
              {-# LINE 1288 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _op2Oval =
             ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: CompareConstantExpr.FCmpExpr.op2.val"
              {-# LINE 1293 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         ( _condIself) =
             cond_
         ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
             ty_ _tyOmn _tyOmts
         ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
             op1_ _op1Omts _op1Otn _op1Oval
         ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
             op2_ _op2Omts _op2Otn _op2Oval
     in  ( _lhsOself))
sem_CompareConstantExpr_ICmpExpr :: T_IntPredicate ->
                                    T_Type ->
                                    T_Value ->
                                    T_Value ->
                                    T_CompareConstantExpr
sem_CompareConstantExpr_ICmpExpr cond_ ty_ op1_ op2_ =
    (let _lhsOself :: CompareConstantExpr
         _tyOmn :: (Maybe SSort)
         _tyOmts :: (Map.Map Type SSort)
         _op1Omts :: (Map.Map Type SSort)
         _op1Otn :: String
         _op1Oval :: Valuation
         _op2Omts :: (Map.Map Type SSort)
         _op2Otn :: String
         _op2Oval :: Valuation
         _condIself :: IntPredicate
         _tyImts :: (Map.Map Type SSort)
         _tyIself :: Type
         _tyIsexprs :: SExpressions
         _tyIsort :: SSortExpr
         _tyIsortn :: SSort
         _op1Imts :: (Map.Map Type SSort)
         _op1Iself :: Value
         _op1Isexpr :: ISExpr
         _op1Isexprs :: SExpressions
         _op1Ivtype :: Type
         _op2Imts :: (Map.Map Type SSort)
         _op2Iself :: Value
         _op2Isexpr :: ISExpr
         _op2Isexprs :: SExpressions
         _op2Ivtype :: Type
         _self =
             ICmpExpr _condIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         _tyOmn =
             ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
              error "missing rule: CompareConstantExpr.ICmpExpr.ty.mn"
              {-# LINE 1342 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _tyOmts =
             ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
              error "missing rule: CompareConstantExpr.ICmpExpr.ty.mts"
              {-# LINE 1347 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _op1Omts =
             ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              _tyImts
              {-# LINE 1352 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _op1Otn =
             ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: CompareConstantExpr.ICmpExpr.op1.tn"
              {-# LINE 1357 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _op1Oval =
             ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: CompareConstantExpr.ICmpExpr.op1.val"
              {-# LINE 1362 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _op2Omts =
             ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              _op1Imts
              {-# LINE 1367 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _op2Otn =
             ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: CompareConstantExpr.ICmpExpr.op2.tn"
              {-# LINE 1372 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _op2Oval =
             ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: CompareConstantExpr.ICmpExpr.op2.val"
              {-# LINE 1377 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         ( _condIself) =
             cond_
         ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
             ty_ _tyOmn _tyOmts
         ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
             op1_ _op1Omts _op1Otn _op1Oval
         ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
             op2_ _op2Omts _op2Otn _op2Oval
     in  ( _lhsOself))
-- Constant ----------------------------------------------------
-- cata
sem_Constant :: Constant ->
                T_Constant
sem_Constant (BlockAddr) =
    (sem_Constant_BlockAddr)
sem_Constant (ConstantAggregateZero _ty) =
    (sem_Constant_ConstantAggregateZero (sem_Type _ty))
sem_Constant (ConstantArray _ty _vals) =
    (sem_Constant_ConstantArray (sem_Type _ty) (sem_Values _vals))
sem_Constant (ConstantDataSequential _cds) =
    (sem_Constant_ConstantDataSequential (sem_ConstantDataSequential _cds))
sem_Constant (ConstantExpr _expr) =
    (sem_Constant_ConstantExpr (sem_ConstantExpr _expr))
sem_Constant (ConstantFP _fp) =
    (sem_Constant_ConstantFP (sem_ConstantFP _fp))
sem_Constant (ConstantInt _iv _ty) =
    (sem_Constant_ConstantInt _iv (sem_Type _ty))
sem_Constant (ConstantPointerNull _ty) =
    (sem_Constant_ConstantPointerNull (sem_Type _ty))
sem_Constant (ConstantStruct _ty _vals) =
    (sem_Constant_ConstantStruct (sem_Type _ty) (sem_Values _vals))
sem_Constant (ConstantVector) =
    (sem_Constant_ConstantVector)
sem_Constant (GlobalValue _gv) =
    (sem_Constant_GlobalValue (sem_GlobalValue _gv))
sem_Constant (UndefValue) =
    (sem_Constant_UndefValue)
-- semantic domain
type T_Constant = (Map.Map Type SSort) ->
                  String ->
                  Valuation ->
                  ( (Map.Map Type SSort),Constant,ISExpr,SExpressions,Type)
data Inh_Constant = Inh_Constant {mts_Inh_Constant :: (Map.Map Type SSort),tn_Inh_Constant :: String,val_Inh_Constant :: Valuation}
data Syn_Constant = Syn_Constant {mts_Syn_Constant :: (Map.Map Type SSort),self_Syn_Constant :: Constant,sexpr_Syn_Constant :: ISExpr,sexprs_Syn_Constant :: SExpressions,vtype_Syn_Constant :: Type}
wrap_Constant :: T_Constant ->
                 Inh_Constant ->
                 Syn_Constant
wrap_Constant sem (Inh_Constant _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_Constant _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOvtype))
sem_Constant_BlockAddr :: T_Constant
sem_Constant_BlockAddr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr =
                  ({-# LINE 71 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1442 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 72 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 1447 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 73 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1452 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  BlockAddr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1461 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Constant_ConstantAggregateZero :: T_Type ->
                                      T_Constant
sem_Constant_ConstantAggregateZero ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _tyOmn :: (Maybe SSort)
              _lhsOself :: Constant
              _lhsOmts :: (Map.Map Type SSort)
              _tyOmts :: (Map.Map Type SSort)
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOsexpr =
                  ({-# LINE 75 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   ISEmpty
                   {-# LINE 1485 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 76 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 1490 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 77 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1495 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 78 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   Nothing
                   {-# LINE 1500 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantAggregateZero _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1509 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 1514 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Constant_ConstantArray :: T_Type ->
                              T_Values ->
                              T_Constant
sem_Constant_ConstantArray ty_ vals_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _valsOmts :: (Map.Map Type SSort)
              _valsOtn :: String
              _valsOval :: Valuation
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _valsImts :: (Map.Map Type SSort)
              _valsIself :: Values
              _valsIsexpr :: ([ISExpr])
              _valsIsexprs :: SExpressions
              _valsIvtype :: ([Type])
              _lhsOsexpr =
                  ({-# LINE 71 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1549 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 72 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 1554 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 73 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1559 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantArray _tyIself _valsIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _valsImts
                   {-# LINE 1568 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Constant.ConstantArray.ty.mn"
                   {-# LINE 1573 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 1578 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valsOmts =
                  ({-# LINE 27 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1583 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valsOtn =
                  ({-# LINE 29 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1588 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valsOval =
                  ({-# LINE 28 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1593 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _valsImts,_valsIself,_valsIsexpr,_valsIsexprs,_valsIvtype) =
                  vals_ _valsOmts _valsOtn _valsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Constant_ConstantDataSequential :: T_ConstantDataSequential ->
                                       T_Constant
sem_Constant_ConstantDataSequential cds_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: (Map.Map Type SSort)
              _cdsOmts :: (Map.Map Type SSort)
              _cdsOtn :: String
              _cdsOval :: Valuation
              _cdsImts :: (Map.Map Type SSort)
              _cdsIself :: ConstantDataSequential
              _cdsIsexpr :: ISExpr
              _cdsIsexprs :: SExpressions
              _cdsIvtype :: Type
              _lhsOsexpr =
                  ({-# LINE 80 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _cdsIsexpr
                   {-# LINE 1622 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 81 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _cdsIsexprs
                   {-# LINE 1627 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 82 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _cdsIvtype
                   {-# LINE 1632 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantDataSequential _cdsIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _cdsImts
                   {-# LINE 1641 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _cdsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1646 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _cdsOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1651 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _cdsOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1656 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _cdsImts,_cdsIself,_cdsIsexpr,_cdsIsexprs,_cdsIvtype) =
                  cds_ _cdsOmts _cdsOtn _cdsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Constant_ConstantExpr :: T_ConstantExpr ->
                             T_Constant
sem_Constant_ConstantExpr expr_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: (Map.Map Type SSort)
              _exprOmts :: (Map.Map Type SSort)
              _exprOtn :: String
              _exprOval :: Valuation
              _exprImts :: (Map.Map Type SSort)
              _exprIself :: ConstantExpr
              _exprIsexpr :: ISExpr
              _exprIsexprs :: SExpressions
              _exprIvtype :: Type
              _lhsOsexpr =
                  ({-# LINE 84 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _exprIsexpr
                   {-# LINE 1683 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 85 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _exprIsexprs
                   {-# LINE 1688 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 86 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _exprIvtype
                   {-# LINE 1693 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantExpr _exprIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _exprImts
                   {-# LINE 1702 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _exprOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1707 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _exprOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1712 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _exprOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1717 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _exprImts,_exprIself,_exprIsexpr,_exprIsexprs,_exprIvtype) =
                  expr_ _exprOmts _exprOtn _exprOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Constant_ConstantFP :: T_ConstantFP ->
                           T_Constant
sem_Constant_ConstantFP fp_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: (Map.Map Type SSort)
              _fpOmts :: (Map.Map Type SSort)
              _fpOtn :: String
              _fpOval :: Valuation
              _fpImts :: (Map.Map Type SSort)
              _fpIself :: ConstantFP
              _fpIsexpr :: ISExpr
              _fpIsexprs :: SExpressions
              _fpIvtype :: Type
              _lhsOsexpr =
                  ({-# LINE 88 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _fpIsexpr
                   {-# LINE 1744 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 89 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _fpIsexprs
                   {-# LINE 1749 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 90 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _fpIvtype
                   {-# LINE 1754 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantFP _fpIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _fpImts
                   {-# LINE 1763 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _fpOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 1768 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _fpOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1773 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _fpOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1778 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _fpImts,_fpIself,_fpIsexpr,_fpIsexprs,_fpIvtype) =
                  fp_ _fpOmts _fpOtn _fpOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Constant_ConstantInt :: Int ->
                            T_Type ->
                            T_Constant
sem_Constant_ConstantInt iv_ ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOsexpr =
                  ({-# LINE 92 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   ISExpr $ IdentExpr $ IdxIdent (bv iv_) [getISize _tyIself]
                   {-# LINE 1805 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 93 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 1810 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 94 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 1815 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantInt iv_ _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1824 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Constant.ConstantInt.ty.mn"
                   {-# LINE 1829 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 1834 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Constant_ConstantPointerNull :: T_Type ->
                                    T_Constant
sem_Constant_ConstantPointerNull ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOsexpr =
                  ({-# LINE 71 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1860 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 72 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 1865 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 73 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1870 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantPointerNull _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1879 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Constant.ConstantPointerNull.ty.mn"
                   {-# LINE 1884 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 1889 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Constant_ConstantStruct :: T_Type ->
                               T_Values ->
                               T_Constant
sem_Constant_ConstantStruct ty_ vals_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _valsOmts :: (Map.Map Type SSort)
              _valsOtn :: String
              _valsOval :: Valuation
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _valsImts :: (Map.Map Type SSort)
              _valsIself :: Values
              _valsIsexpr :: ([ISExpr])
              _valsIsexprs :: SExpressions
              _valsIvtype :: ([Type])
              _lhsOsexpr =
                  ({-# LINE 71 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1924 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 72 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 1929 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 73 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1934 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantStruct _tyIself _valsIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _valsImts
                   {-# LINE 1943 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Constant.ConstantStruct.ty.mn"
                   {-# LINE 1948 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 1953 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valsOmts =
                  ({-# LINE 27 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 1958 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valsOtn =
                  ({-# LINE 29 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 1963 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valsOval =
                  ({-# LINE 28 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 1968 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _valsImts,_valsIself,_valsIsexpr,_valsIsexprs,_valsIvtype) =
                  vals_ _valsOmts _valsOtn _valsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Constant_ConstantVector :: T_Constant
sem_Constant_ConstantVector =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr =
                  ({-# LINE 71 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1988 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 72 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 1993 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 73 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 1998 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantVector
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2007 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Constant_GlobalValue :: T_GlobalValue ->
                            T_Constant
sem_Constant_GlobalValue gv_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOself :: Constant
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOvtype :: Type
              _gvOmts :: (Map.Map Type SSort)
              _gvOtn :: String
              _gvOval :: Valuation
              _gvImts :: (Map.Map Type SSort)
              _gvIself :: GlobalValue
              _gvIsexpr :: ISExpr
              _gvIsexprs :: SExpressions
              _gvIvtype :: Type
              _lhsOsexpr =
                  ({-# LINE 96 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _gvIsexpr
                   {-# LINE 2032 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 97 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _gvIsexprs
                   {-# LINE 2037 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  GlobalValue _gvIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _gvImts
                   {-# LINE 2046 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 50 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _gvIvtype
                   {-# LINE 2051 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _gvOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2056 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _gvOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2061 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _gvOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2066 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _gvImts,_gvIself,_gvIsexpr,_gvIsexprs,_gvIvtype) =
                  gv_ _gvOmts _gvOtn _gvOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Constant_UndefValue :: T_Constant
sem_Constant_UndefValue =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Constant
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr =
                  ({-# LINE 71 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2084 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 72 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 2089 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 73 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2094 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  UndefValue
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2103 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
-- ConstantDataSequential --------------------------------------
-- cata
sem_ConstantDataSequential :: ConstantDataSequential ->
                              T_ConstantDataSequential
sem_ConstantDataSequential (ConstantDataArray _ty _val) =
    (sem_ConstantDataSequential_ConstantDataArray (sem_Type _ty) _val)
sem_ConstantDataSequential (ConstantDataVector _ty _val) =
    (sem_ConstantDataSequential_ConstantDataVector (sem_Type _ty) _val)
-- semantic domain
type T_ConstantDataSequential = (Map.Map Type SSort) ->
                                String ->
                                Valuation ->
                                ( (Map.Map Type SSort),ConstantDataSequential,ISExpr,SExpressions,Type)
data Inh_ConstantDataSequential = Inh_ConstantDataSequential {mts_Inh_ConstantDataSequential :: (Map.Map Type SSort),tn_Inh_ConstantDataSequential :: String,val_Inh_ConstantDataSequential :: Valuation}
data Syn_ConstantDataSequential = Syn_ConstantDataSequential {mts_Syn_ConstantDataSequential :: (Map.Map Type SSort),self_Syn_ConstantDataSequential :: ConstantDataSequential,sexpr_Syn_ConstantDataSequential :: ISExpr,sexprs_Syn_ConstantDataSequential :: SExpressions,vtype_Syn_ConstantDataSequential :: Type}
wrap_ConstantDataSequential :: T_ConstantDataSequential ->
                               Inh_ConstantDataSequential ->
                               Syn_ConstantDataSequential
wrap_ConstantDataSequential sem (Inh_ConstantDataSequential _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_ConstantDataSequential _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOvtype))
sem_ConstantDataSequential_ConstantDataArray :: T_Type ->
                                                String ->
                                                T_ConstantDataSequential
sem_ConstantDataSequential_ConstantDataArray ty_ val_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantDataSequential
              _lhsOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOsexpr =
                  ({-# LINE 117 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   ISEmpty
                   {-# LINE 2149 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 118 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 2154 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 119 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2159 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantDataArray _tyIself val_
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 2168 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: ConstantDataSequential.ConstantDataArray.ty.mn"
                   {-# LINE 2173 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 2178 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_ConstantDataSequential_ConstantDataVector :: T_Type ->
                                                 String ->
                                                 T_ConstantDataSequential
sem_ConstantDataSequential_ConstantDataVector ty_ val_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantDataSequential
              _lhsOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOsexpr =
                  ({-# LINE 121 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "data vector"
                   {-# LINE 2205 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 122 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 2210 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 123 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2215 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantDataVector _tyIself val_
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 2224 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: ConstantDataSequential.ConstantDataVector.ty.mn"
                   {-# LINE 2229 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 2234 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
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
type T_ConstantExpr = (Map.Map Type SSort) ->
                      String ->
                      Valuation ->
                      ( (Map.Map Type SSort),ConstantExpr,ISExpr,SExpressions,Type)
data Inh_ConstantExpr = Inh_ConstantExpr {mts_Inh_ConstantExpr :: (Map.Map Type SSort),tn_Inh_ConstantExpr :: String,val_Inh_ConstantExpr :: Valuation}
data Syn_ConstantExpr = Syn_ConstantExpr {mts_Syn_ConstantExpr :: (Map.Map Type SSort),self_Syn_ConstantExpr :: ConstantExpr,sexpr_Syn_ConstantExpr :: ISExpr,sexprs_Syn_ConstantExpr :: SExpressions,vtype_Syn_ConstantExpr :: Type}
wrap_ConstantExpr :: T_ConstantExpr ->
                     Inh_ConstantExpr ->
                     Syn_ConstantExpr
wrap_ConstantExpr sem (Inh_ConstantExpr _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_ConstantExpr _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOvtype))
sem_ConstantExpr_BinaryConstantExpr :: T_ConstantExpr
sem_ConstantExpr_BinaryConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr =
                  ({-# LINE 140 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2289 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 141 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2294 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 142 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2299 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  BinaryConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2308 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_ConstantExpr_CompareConstantExpr :: T_CompareConstantExpr ->
                                        T_ConstantExpr
sem_ConstantExpr_CompareConstantExpr cmpExpr_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: (Map.Map Type SSort)
              _cmpExprIself :: CompareConstantExpr
              _lhsOsexpr =
                  ({-# LINE 140 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2326 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 141 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2331 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 142 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2336 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  CompareConstantExpr _cmpExprIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2345 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _cmpExprIself) =
                  cmpExpr_
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_ConstantExpr_ExtractElementConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ExtractElementConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr =
                  ({-# LINE 140 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2363 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 141 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2368 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 142 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2373 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ExtractElementConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2382 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_ConstantExpr_ExtractValueConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ExtractValueConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr =
                  ({-# LINE 140 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2398 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 141 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2403 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 142 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2408 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ExtractValueConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2417 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_ConstantExpr_GetElementPtrConstantExpr :: T_Value ->
                                              T_Values ->
                                              T_ConstantExpr
sem_ConstantExpr_GetElementPtrConstantExpr struct_ idxs_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _structOmts :: (Map.Map Type SSort)
              _idxsOmts :: (Map.Map Type SSort)
              _lhsOmts :: (Map.Map Type SSort)
              _structOtn :: String
              _idxsOtn :: String
              _lhsOvtype :: Type
              _lhsOsexprs :: SExpressions
              _lhsOsexpr :: ISExpr
              _lhsOself :: ConstantExpr
              _structOval :: Valuation
              _idxsOval :: Valuation
              _structImts :: (Map.Map Type SSort)
              _structIself :: Value
              _structIsexpr :: ISExpr
              _structIsexprs :: SExpressions
              _structIvtype :: Type
              _idxsImts :: (Map.Map Type SSort)
              _idxsIself :: Values
              _idxsIsexpr :: ([ISExpr])
              _idxsIsexprs :: SExpressions
              _idxsIvtype :: ([Type])
              _structOmts =
                  ({-# LINE 128 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2451 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _idxsOmts =
                  ({-# LINE 129 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _structImts
                   {-# LINE 2456 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 130 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _idxsImts
                   {-# LINE 2461 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _structOtn =
                  ({-# LINE 131 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2466 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _idxsOtn =
                  ({-# LINE 132 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2471 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 133 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _structIvtype
                   {-# LINE 2476 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _idxn =
                  ({-# LINE 134 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   let x = getIdxN _structIvtype
                   in trace (show x ++ " " ++ show _structIvtype) $ x
                   {-# LINE 2482 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 136 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _structIsexprs ++ _idxsIsexprs
                   {-# LINE 2487 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 137 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   foldr (\(n, s1) s2 -> sFn "select" s2 $ changeN s1 n) _structIsexpr $ zip _idxn     $ init' _idxsIsexpr
                   {-# LINE 2492 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  GetElementPtrConstantExpr _structIself _idxsIself
              _lhsOself =
                  _self
              _structOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2501 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _idxsOval =
                  ({-# LINE 28 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2506 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _structImts,_structIself,_structIsexpr,_structIsexprs,_structIvtype) =
                  struct_ _structOmts _structOtn _structOval
              ( _idxsImts,_idxsIself,_idxsIsexpr,_idxsIsexprs,_idxsIvtype) =
                  idxs_ _idxsOmts _idxsOtn _idxsOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_ConstantExpr_InsertElementConstantExpr :: T_ConstantExpr
sem_ConstantExpr_InsertElementConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr =
                  ({-# LINE 140 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2526 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 141 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2531 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 142 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2536 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  InsertElementConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2545 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_ConstantExpr_InsertValueConstantExpr :: T_ConstantExpr
sem_ConstantExpr_InsertValueConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr =
                  ({-# LINE 140 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2561 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 141 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2566 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 142 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2571 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  InsertValueConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2580 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_ConstantExpr_SelectConstantExpr :: T_ConstantExpr
sem_ConstantExpr_SelectConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr =
                  ({-# LINE 140 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2596 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 141 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2601 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 142 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2606 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  SelectConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2615 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_ConstantExpr_ShuffleVectorConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ShuffleVectorConstantExpr =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr =
                  ({-# LINE 140 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2631 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 141 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2636 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 142 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2641 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ShuffleVectorConstantExpr
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2650 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_ConstantExpr_UnaryConstantExpr :: String ->
                                      Int ->
                                      T_Value ->
                                      T_Type ->
                                      T_ConstantExpr
sem_ConstantExpr_UnaryConstantExpr name_ op_ val_ ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantExpr
              _lhsOmts :: (Map.Map Type SSort)
              _valOmts :: (Map.Map Type SSort)
              _valOtn :: String
              _valOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _valImts :: (Map.Map Type SSort)
              _valIself :: Value
              _valIsexpr :: ISExpr
              _valIsexprs :: SExpressions
              _valIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOsexpr =
                  ({-# LINE 140 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2685 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 141 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2690 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 142 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2695 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  UnaryConstantExpr name_ op_ _valIself _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 2704 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 2709 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 2714 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 2719 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: ConstantExpr.UnaryConstantExpr.ty.mn"
                   {-# LINE 2724 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _valImts
                   {-# LINE 2729 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _valImts,_valIself,_valIsexpr,_valIsexprs,_valIvtype) =
                  val_ _valOmts _valOtn _valOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
-- ConstantFP --------------------------------------------------
-- cata
sem_ConstantFP :: ConstantFP ->
                  T_ConstantFP
sem_ConstantFP (ConstantFPDouble _dbv _ty) =
    (sem_ConstantFP_ConstantFPDouble _dbv (sem_Type _ty))
sem_ConstantFP (ConstantFPFloat _fpv _ty) =
    (sem_ConstantFP_ConstantFPFloat _fpv (sem_Type _ty))
-- semantic domain
type T_ConstantFP = (Map.Map Type SSort) ->
                    String ->
                    Valuation ->
                    ( (Map.Map Type SSort),ConstantFP,ISExpr,SExpressions,Type)
data Inh_ConstantFP = Inh_ConstantFP {mts_Inh_ConstantFP :: (Map.Map Type SSort),tn_Inh_ConstantFP :: String,val_Inh_ConstantFP :: Valuation}
data Syn_ConstantFP = Syn_ConstantFP {mts_Syn_ConstantFP :: (Map.Map Type SSort),self_Syn_ConstantFP :: ConstantFP,sexpr_Syn_ConstantFP :: ISExpr,sexprs_Syn_ConstantFP :: SExpressions,vtype_Syn_ConstantFP :: Type}
wrap_ConstantFP :: T_ConstantFP ->
                   Inh_ConstantFP ->
                   Syn_ConstantFP
wrap_ConstantFP sem (Inh_ConstantFP _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_ConstantFP _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOvtype))
sem_ConstantFP_ConstantFPDouble :: Double ->
                                   T_Type ->
                                   T_ConstantFP
sem_ConstantFP_ConstantFPDouble dbv_ ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantFP
              _lhsOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOsexpr =
                  ({-# LINE 103 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2779 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 104 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2784 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 105 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2789 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantFPDouble dbv_ _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 2798 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: ConstantFP.ConstantFPDouble.ty.mn"
                   {-# LINE 2803 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 2808 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_ConstantFP_ConstantFPFloat :: Float ->
                                  T_Type ->
                                  T_ConstantFP
sem_ConstantFP_ConstantFPFloat fpv_ ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: ConstantFP
              _lhsOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOsexpr =
                  ({-# LINE 103 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2835 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 104 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2840 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 105 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   verrormessage
                   {-# LINE 2845 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ConstantFPFloat fpv_ _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 2854 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: ConstantFP.ConstantFPFloat.ty.mn"
                   {-# LINE 2859 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 2864 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
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
    (sem_Function_FunctionDecl (sem_Id _name) (sem_Linkage _linkage) (sem_Type _retty) (sem_Parameters _params))
sem_Function (FunctionDef _name _linkage _retty _params _body) =
    (sem_Function_FunctionDef (sem_Id _name) (sem_Linkage _linkage) (sem_Type _retty) (sem_Parameters _params) (sem_BasicBlocks _body))
-- semantic domain
type T_Function = ([(PC,PC)]) ->
                  (Map.Map Identifier Type) ->
                  Declarations ->
                  Id ->
                  ( ([Id]),Function,Transitions)
data Inh_Function = Inh_Function {cfg_Inh_Function :: ([(PC,PC)]),dfg_Inh_Function :: (Map.Map Identifier Type),fdcl_Inh_Function :: Declarations,tn_Inh_Function :: Id}
data Syn_Function = Syn_Function {locals_Syn_Function :: ([Id]),self_Syn_Function :: Function,ts_Syn_Function :: Transitions}
wrap_Function :: T_Function ->
                 Inh_Function ->
                 Syn_Function
wrap_Function sem (Inh_Function _lhsIcfg _lhsIdfg _lhsIfdcl _lhsItn) =
    (let ( _lhsOlocals,_lhsOself,_lhsOts) = sem _lhsIcfg _lhsIdfg _lhsIfdcl _lhsItn
     in  (Syn_Function _lhsOlocals _lhsOself _lhsOts))
sem_Function_FunctionDecl :: T_Id ->
                             T_Linkage ->
                             T_Type ->
                             T_Parameters ->
                             T_Function
sem_Function_FunctionDecl name_ linkage_ retty_ params_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOts :: Transitions
              _lhsOlocals :: ([Id])
              _lhsOself :: Function
              _rettyOmn :: (Maybe SSort)
              _rettyOmts :: (Map.Map Type SSort)
              _nameIself :: Id
              _linkageIself :: Linkage
              _rettyImts :: (Map.Map Type SSort)
              _rettyIself :: Type
              _rettyIsexprs :: SExpressions
              _rettyIsort :: SSortExpr
              _rettyIsortn :: SSort
              _paramsIself :: Parameters
              _lhsOts =
                  ({-# LINE 39 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 3251 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 3256 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FunctionDecl _nameIself _linkageIself _rettyIself _paramsIself
              _lhsOself =
                  _self
              _rettyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Function.FunctionDecl.retty.mn"
                   {-# LINE 3265 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _rettyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Function.FunctionDecl.retty.mts"
                   {-# LINE 3270 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _nameIself) =
                  name_
              ( _linkageIself) =
                  linkage_
              ( _rettyImts,_rettyIself,_rettyIsexprs,_rettyIsort,_rettyIsortn) =
                  retty_ _rettyOmn _rettyOmts
              ( _paramsIself) =
                  params_
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Function_FunctionDef :: T_Id ->
                            T_Linkage ->
                            T_Type ->
                            T_Parameters ->
                            T_BasicBlocks ->
                            T_Function
sem_Function_FunctionDef name_ linkage_ retty_ params_ body_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOts :: Transitions
              _lhsOlocals :: ([Id])
              _lhsOself :: Function
              _rettyOmn :: (Maybe SSort)
              _rettyOmts :: (Map.Map Type SSort)
              _bodyOcfg :: ([(PC,PC)])
              _bodyOdfg :: (Map.Map Identifier Type)
              _bodyOfdcl :: Declarations
              _bodyOtn :: Id
              _nameIself :: Id
              _linkageIself :: Linkage
              _rettyImts :: (Map.Map Type SSort)
              _rettyIself :: Type
              _rettyIsexprs :: SExpressions
              _rettyIsort :: SSortExpr
              _rettyIsortn :: SSort
              _paramsIself :: Parameters
              _bodyIlocals :: ([Id])
              _bodyIself :: BasicBlocks
              _bodyIts :: Transitions
              _lhsOts =
                  ({-# LINE 40 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _bodyIts
                   {-# LINE 3315 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _bodyIlocals
                   {-# LINE 3320 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FunctionDef _nameIself _linkageIself _rettyIself _paramsIself _bodyIself
              _lhsOself =
                  _self
              _rettyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Function.FunctionDef.retty.mn"
                   {-# LINE 3329 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _rettyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Function.FunctionDef.retty.mts"
                   {-# LINE 3334 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _bodyOcfg =
                  ({-# LINE 33 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIcfg
                   {-# LINE 3339 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _bodyOdfg =
                  ({-# LINE 35 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIdfg
                   {-# LINE 3344 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _bodyOfdcl =
                  ({-# LINE 34 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIfdcl
                   {-# LINE 3349 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _bodyOtn =
                  ({-# LINE 36 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsItn
                   {-# LINE 3354 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _nameIself) =
                  name_
              ( _linkageIself) =
                  linkage_
              ( _rettyImts,_rettyIself,_rettyIsexprs,_rettyIsort,_rettyIsortn) =
                  retty_ _rettyOmn _rettyOmts
              ( _paramsIself) =
                  params_
              ( _bodyIlocals,_bodyIself,_bodyIts) =
                  body_ _bodyOcfg _bodyOdfg _bodyOfdcl _bodyOtn
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
-- Functions ---------------------------------------------------
-- cata
sem_Functions :: Functions ->
                 T_Functions
sem_Functions m =
    (Data.Map.foldrWithKey sem_Functions_Entry sem_Functions_Nil (Data.Map.map sem_Function m))
-- semantic domain
type T_Functions = ControlFlow ->
                   (Map.Map String (Map.Map Identifier Type)) ->
                   Declarations ->
                   ( (Map.Map String Transitions),Functions)
data Inh_Functions = Inh_Functions {ccfg_Inh_Functions :: ControlFlow,cdfg_Inh_Functions :: (Map.Map String (Map.Map Identifier Type)),fdcl_Inh_Functions :: Declarations}
data Syn_Functions = Syn_Functions {cts_Syn_Functions :: (Map.Map String Transitions),self_Syn_Functions :: Functions}
wrap_Functions :: T_Functions ->
                  Inh_Functions ->
                  Syn_Functions
wrap_Functions sem (Inh_Functions _lhsIccfg _lhsIcdfg _lhsIfdcl) =
    (let ( _lhsOcts,_lhsOself) = sem _lhsIccfg _lhsIcdfg _lhsIfdcl
     in  (Syn_Functions _lhsOcts _lhsOself))
sem_Functions_Entry :: String ->
                       T_Function ->
                       T_Functions ->
                       T_Functions
sem_Functions_Entry key_ val_ tl_ =
    (\ _lhsIccfg
       _lhsIcdfg
       _lhsIfdcl ->
         (let _lhsOcts :: (Map.Map String Transitions)
              _valOcfg :: ([(PC,PC)])
              _valOdfg :: (Map.Map Identifier Type)
              _valOtn :: Id
              _lhsOself :: Functions
              _valOfdcl :: Declarations
              _tlOccfg :: ControlFlow
              _tlOcdfg :: (Map.Map String (Map.Map Identifier Type))
              _tlOfdcl :: Declarations
              _valIlocals :: ([Id])
              _valIself :: Function
              _valIts :: Transitions
              _tlIcts :: (Map.Map String Transitions)
              _tlIself :: Functions
              _lhsOcts =
                  ({-# LINE 25 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   Map.insert key_ _valIts _tlIcts
                   {-# LINE 3411 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valOcfg =
                  ({-# LINE 26 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   fromMaybe (error "no cfg") $ Map.lookup key_ $ cfg _lhsIccfg
                   {-# LINE 3416 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valOdfg =
                  ({-# LINE 27 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   fromMaybe (error "no dfg") $ Map.lookup key_ _lhsIcdfg
                   {-# LINE 3421 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valOtn =
                  ({-# LINE 28 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   key_
                   {-# LINE 3426 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Data.Map.insert key_ _valIself _tlIself
              _lhsOself =
                  _self
              _valOfdcl =
                  ({-# LINE 34 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIfdcl
                   {-# LINE 3435 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOccfg =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIccfg
                   {-# LINE 3440 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOcdfg =
                  ({-# LINE 18 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIcdfg
                   {-# LINE 3445 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOfdcl =
                  ({-# LINE 17 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIfdcl
                   {-# LINE 3450 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _valIlocals,_valIself,_valIts) =
                  val_ _valOcfg _valOdfg _valOfdcl _valOtn
              ( _tlIcts,_tlIself) =
                  tl_ _tlOccfg _tlOcdfg _tlOfdcl
          in  ( _lhsOcts,_lhsOself)))
sem_Functions_Nil :: T_Functions
sem_Functions_Nil =
    (\ _lhsIccfg
       _lhsIcdfg
       _lhsIfdcl ->
         (let _lhsOcts :: (Map.Map String Transitions)
              _lhsOself :: Functions
              _lhsOcts =
                  ({-# LINE 23 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   Map.empty
                   {-# LINE 3467 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Data.Map.empty
              _lhsOself =
                  _self
          in  ( _lhsOcts,_lhsOself)))
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
    (sem_Global_GlobalVar (sem_Id _name) (sem_Linkage _linkage) _isConst _isUaddr (sem_Type _ty) (sem_MValue _ival) (sem_Align _align))
-- semantic domain
type T_Global = GlobalState ->
                ( GlobalState,Global,ISExpr,SExpressions)
data Inh_Global = Inh_Global {gs_Inh_Global :: GlobalState}
data Syn_Global = Syn_Global {gs_Syn_Global :: GlobalState,self_Syn_Global :: Global,sexpr_Syn_Global :: ISExpr,sexprs_Syn_Global :: SExpressions}
wrap_Global :: T_Global ->
               Inh_Global ->
               Syn_Global
wrap_Global sem (Inh_Global _lhsIgs) =
    (let ( _lhsOgs,_lhsOself,_lhsOsexpr,_lhsOsexprs) = sem _lhsIgs
     in  (Syn_Global _lhsOgs _lhsOself _lhsOsexpr _lhsOsexprs))
sem_Global_GlobalVar :: T_Id ->
                        T_Linkage ->
                        Bool ->
                        Bool ->
                        T_Type ->
                        T_MValue ->
                        T_Align ->
                        T_Global
sem_Global_GlobalVar name_ linkage_ isConst_ isUaddr_ ty_ ival_ align_ =
    (\ _lhsIgs ->
         (let _tyOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _ivalOmts :: (Map.Map Type SSort)
              _ivalOtn :: String
              _lhsOsexprs :: SExpressions
              _lhsOsexpr :: ISExpr
              _lhsOgs :: GlobalState
              _lhsOself :: Global
              _ivalOval :: Valuation
              _nameIself :: Id
              _linkageIself :: Linkage
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _ivalImts :: (Map.Map Type SSort)
              _ivalIself :: MValue
              _ivalIsexpr :: ISExpr
              _ivalIsexprs :: SExpressions
              _alignIself :: Align
              _tyOmts =
                  ({-# LINE 31 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   defsorts _lhsIgs
                   {-# LINE 3550 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   Nothing
                   {-# LINE 3555 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _ivalOmts =
                  ({-# LINE 33 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   _tyImts
                   {-# LINE 3560 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _ivalOtn =
                  ({-# LINE 34 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   ""
                   {-# LINE 3565 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 35 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   _tyIsexprs ++ _ivalIsexprs ++ [ declfun _sym     _tyIsort ]
                   {-# LINE 3570 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 36 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   case _ivalIsexpr of
                        ISExpr s -> sFn "=" (ISExpr $ IdentExpr $ SymIdent $ _sym    ) _ivalIsexpr
                        _       -> _ivalIsexpr
                   {-# LINE 3577 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _sym =
                  ({-# LINE 39 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   SimpleSym _nameIself
                   {-# LINE 3582 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOgs =
                  ({-# LINE 40 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   let ogs@GlobalState{..} = _lhsIgs
                       gvals' = maybe gvals (\v -> Map.insert _nameIself (Right v) gvals) _ivalIself
                   in ogs { defsorts = _ivalImts, gvals = gvals' }
                   {-# LINE 3589 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  GlobalVar _nameIself _linkageIself isConst_ isUaddr_ _tyIself _ivalIself _alignIself
              _lhsOself =
                  _self
              _ivalOval =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Global.GlobalVar.ival.val"
                   {-# LINE 3598 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _nameIself) =
                  name_
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
type T_GlobalValue = (Map.Map Type SSort) ->
                     String ->
                     Valuation ->
                     ( (Map.Map Type SSort),GlobalValue,ISExpr,SExpressions,Type)
data Inh_GlobalValue = Inh_GlobalValue {mts_Inh_GlobalValue :: (Map.Map Type SSort),tn_Inh_GlobalValue :: String,val_Inh_GlobalValue :: Valuation}
data Syn_GlobalValue = Syn_GlobalValue {mts_Syn_GlobalValue :: (Map.Map Type SSort),self_Syn_GlobalValue :: GlobalValue,sexpr_Syn_GlobalValue :: ISExpr,sexprs_Syn_GlobalValue :: SExpressions,vtype_Syn_GlobalValue :: Type}
wrap_GlobalValue :: T_GlobalValue ->
                    Inh_GlobalValue ->
                    Syn_GlobalValue
wrap_GlobalValue sem (Inh_GlobalValue _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_GlobalValue _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOvtype))
sem_GlobalValue_FunctionValue :: T_Identifier ->
                                 T_Type ->
                                 T_GlobalValue
sem_GlobalValue_FunctionValue n_ ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _nOtn :: String
              _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: GlobalValue
              _lhsOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _nIident :: String
              _nIlocals :: ([Id])
              _nIself :: Identifier
              _nIsexpr :: ISExpr
              _nIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _nOtn =
                  ({-# LINE 109 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 3662 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 110 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   ISExpr $ IdentExpr $ SymIdent _nIssymbol
                   {-# LINE 3667 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 111 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 3672 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 112 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 3677 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FunctionValue _nIself _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 3686 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: GlobalValue.FunctionValue.ty.mn"
                   {-# LINE 3691 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 3696 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _nIident,_nIlocals,_nIself,_nIsexpr,_nIssymbol) =
                  n_ _nOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_GlobalValue_GlobalAlias :: T_Identifier ->
                               T_Type ->
                               T_GlobalValue
sem_GlobalValue_GlobalAlias n_ ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _nOtn :: String
              _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: GlobalValue
              _lhsOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _nIident :: String
              _nIlocals :: ([Id])
              _nIself :: Identifier
              _nIsexpr :: ISExpr
              _nIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _nOtn =
                  ({-# LINE 109 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 3731 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 110 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   ISExpr $ IdentExpr $ SymIdent _nIssymbol
                   {-# LINE 3736 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 111 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 3741 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 112 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 3746 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  GlobalAlias _nIself _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 3755 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: GlobalValue.GlobalAlias.ty.mn"
                   {-# LINE 3760 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 3765 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _nIident,_nIlocals,_nIself,_nIsexpr,_nIssymbol) =
                  n_ _nOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_GlobalValue_GlobalVariable :: T_Identifier ->
                                  T_Type ->
                                  T_GlobalValue
sem_GlobalValue_GlobalVariable n_ ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _nOtn :: String
              _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: GlobalValue
              _lhsOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _nIident :: String
              _nIlocals :: ([Id])
              _nIself :: Identifier
              _nIsexpr :: ISExpr
              _nIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _nOtn =
                  ({-# LINE 109 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 3800 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 110 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   ISExpr $ IdentExpr $ SymIdent _nIssymbol
                   {-# LINE 3805 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 111 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 3810 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 112 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 3815 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  GlobalVariable _nIself _tyIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 3824 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: GlobalValue.GlobalVariable.ty.mn"
                   {-# LINE 3829 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 3834 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _nIident,_nIlocals,_nIself,_nIsexpr,_nIssymbol) =
                  n_ _nOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
-- Globals -----------------------------------------------------
-- cata
sem_Globals :: Globals ->
               T_Globals
sem_Globals list =
    (Prelude.foldr sem_Globals_Cons sem_Globals_Nil (Prelude.map sem_Global list))
-- semantic domain
type T_Globals = GlobalState ->
                 ( GlobalState,Globals,ISExpr,SExpressions)
data Inh_Globals = Inh_Globals {gs_Inh_Globals :: GlobalState}
data Syn_Globals = Syn_Globals {gs_Syn_Globals :: GlobalState,self_Syn_Globals :: Globals,sexpr_Syn_Globals :: ISExpr,sexprs_Syn_Globals :: SExpressions}
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
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOself :: Globals
              _lhsOgs :: GlobalState
              _hdOgs :: GlobalState
              _tlOgs :: GlobalState
              _hdIgs :: GlobalState
              _hdIself :: Global
              _hdIsexpr :: ISExpr
              _hdIsexprs :: SExpressions
              _tlIgs :: GlobalState
              _tlIself :: Globals
              _tlIsexpr :: ISExpr
              _tlIsexprs :: SExpressions
              _lhsOsexpr =
                  ({-# LINE 26 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   sAnd _hdIsexpr _tlIsexpr
                   {-# LINE 3880 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 27 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   nub $  _hdIsexprs ++ _tlIsexprs
                   {-# LINE 3885 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOgs =
                  ({-# LINE 20 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   _tlIgs
                   {-# LINE 3894 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _hdOgs =
                  ({-# LINE 20 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   _lhsIgs
                   {-# LINE 3899 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOgs =
                  ({-# LINE 20 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   _hdIgs
                   {-# LINE 3904 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _hdIgs,_hdIself,_hdIsexpr,_hdIsexprs) =
                  hd_ _hdOgs
              ( _tlIgs,_tlIself,_tlIsexpr,_tlIsexprs) =
                  tl_ _tlOgs
          in  ( _lhsOgs,_lhsOself,_lhsOsexpr,_lhsOsexprs)))
sem_Globals_Nil :: T_Globals
sem_Globals_Nil =
    (\ _lhsIgs ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOself :: Globals
              _lhsOgs :: GlobalState
              _lhsOsexpr =
                  ({-# LINE 24 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   ISEmpty
                   {-# LINE 3921 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 19 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   []
                   {-# LINE 3926 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOgs =
                  ({-# LINE 20 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
                   _lhsIgs
                   {-# LINE 3935 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
type T_Identifier = String ->
                    ( String,([Id]),Identifier,ISExpr,SSymbol)
data Inh_Identifier = Inh_Identifier {tn_Inh_Identifier :: String}
data Syn_Identifier = Syn_Identifier {ident_Syn_Identifier :: String,locals_Syn_Identifier :: ([Id]),self_Syn_Identifier :: Identifier,sexpr_Syn_Identifier :: ISExpr,ssymbol_Syn_Identifier :: SSymbol}
wrap_Identifier :: T_Identifier ->
                   Inh_Identifier ->
                   Syn_Identifier
wrap_Identifier sem (Inh_Identifier _lhsItn) =
    (let ( _lhsOident,_lhsOlocals,_lhsOself,_lhsOsexpr,_lhsOssymbol) = sem _lhsItn
     in  (Syn_Identifier _lhsOident _lhsOlocals _lhsOself _lhsOsexpr _lhsOssymbol))
sem_Identifier_Global :: T_Id ->
                         T_Identifier
sem_Identifier_Global name_ =
    (\ _lhsItn ->
         (let _lhsOssymbol :: SSymbol
              _lhsOident :: String
              _lhsOsexpr :: ISExpr
              _lhsOlocals :: ([Id])
              _lhsOself :: Identifier
              _nameIself :: Id
              _lhsOssymbol =
                  ({-# LINE 19 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _sym
                   {-# LINE 3995 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 20 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _nameIself
                   {-# LINE 4000 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 21 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   ISExpr $ IdentExpr $ SymIdent _sym
                   {-# LINE 4005 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _sym =
                  ({-# LINE 22 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   SimpleSym _nameIself
                   {-# LINE 4010 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 4015 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Global _nameIself
              _lhsOself =
                  _self
              ( _nameIself) =
                  name_
          in  ( _lhsOident,_lhsOlocals,_lhsOself,_lhsOsexpr,_lhsOssymbol)))
sem_Identifier_Local :: T_Id ->
                        T_Identifier
sem_Identifier_Local name_ =
    (\ _lhsItn ->
         (let _lhsOssymbol :: SSymbol
              _lhsOident :: String
              _lhsOsexpr :: ISExpr
              _lhsOlocals :: ([Id])
              _lhsOself :: Identifier
              _nameIself :: Id
              _lhsOssymbol =
                  ({-# LINE 24 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _sym
                   {-# LINE 4037 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOident =
                  ({-# LINE 25 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _nameIself
                   {-# LINE 4042 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 26 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   ISExpr $ IdentExpr $ SymIdent _sym
                   {-# LINE 4047 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _sym =
                  ({-# LINE 27 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   SimpleSym $ _lhsItn ++ _nameIself
                   {-# LINE 4052 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 4057 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Local _nameIself
              _lhsOself =
                  _self
              ( _nameIself) =
                  name_
          in  ( _lhsOident,_lhsOlocals,_lhsOself,_lhsOsexpr,_lhsOssymbol)))
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
         _hdOtn :: String
         _hdIident :: String
         _hdIlocals :: ([Id])
         _hdIself :: Identifier
         _hdIsexpr :: ISExpr
         _hdIssymbol :: SSymbol
         _tlIself :: Identifiers
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         _hdOtn =
             ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
              error "missing rule: Identifiers.Cons.hd.tn"
              {-# LINE 4101 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         ( _hdIident,_hdIlocals,_hdIself,_hdIsexpr,_hdIssymbol) =
             hd_ _hdOtn
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
    (sem_Instruction_Call (sem_PC _pc) (sem_Identifier _mres) (sem_Type _ty) (sem_Id _callee) (sem_Values _args))
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
sem_Instruction (IntToPtr _pc _id _v _ty) =
    (sem_Instruction_IntToPtr (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
sem_Instruction (LShr _pc _id _ty _op1 _op2) =
    (sem_Instruction_LShr (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Load _pc _id _v _align) =
    (sem_Instruction_Load (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Align _align))
sem_Instruction (Mul _pc _id _ty _op1 _op2) =
    (sem_Instruction_Mul (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
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
sem_Instruction (Xor _pc _id _ty _op1 _op2) =
    (sem_Instruction_Xor (sem_PC _pc) (sem_Identifier _id) (sem_Type _ty) (sem_Value _op1) (sem_Value _op2))
sem_Instruction (ZExt _pc _id _v _ty) =
    (sem_Instruction_ZExt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Type _ty))
-- semantic domain
type T_Instruction = ([(PC,PC)]) ->
                     (Map.Map Identifier Type) ->
                     Declarations ->
                     Id ->
                     ( ([Id]),Instruction,Transitions)
data Inh_Instruction = Inh_Instruction {cfg_Inh_Instruction :: ([(PC,PC)]),dfg_Inh_Instruction :: (Map.Map Identifier Type),fdcl_Inh_Instruction :: Declarations,tn_Inh_Instruction :: Id}
data Syn_Instruction = Syn_Instruction {locals_Syn_Instruction :: ([Id]),self_Syn_Instruction :: Instruction,ts_Syn_Instruction :: Transitions}
wrap_Instruction :: T_Instruction ->
                    Inh_Instruction ->
                    Syn_Instruction
wrap_Instruction sem (Inh_Instruction _lhsIcfg _lhsIdfg _lhsIfdcl _lhsItn) =
    (let ( _lhsOlocals,_lhsOself,_lhsOts) = sem _lhsIcfg _lhsIdfg _lhsIfdcl _lhsItn
     in  (Syn_Instruction _lhsOlocals _lhsOself _lhsOts))
sem_Instruction_AShr :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_AShr pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 4275 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 4280 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  AShr _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 4289 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.AShr.ty.mn"
                   {-# LINE 4294 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.AShr.ty.mts"
                   {-# LINE 4299 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 4304 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4309 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.AShr.op1.val"
                   {-# LINE 4314 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 4319 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4324 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.AShr.op2.val"
                   {-# LINE 4329 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Add :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Add pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 4389 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 4394 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Add _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 4403 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Add.ty.mn"
                   {-# LINE 4408 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Add.ty.mts"
                   {-# LINE 4413 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 4418 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4423 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Add.op1.val"
                   {-# LINE 4428 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 4433 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4438 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Add.op2.val"
                   {-# LINE 4443 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Alloca :: T_PC ->
                          T_Identifier ->
                          T_Type ->
                          T_Align ->
                          T_Instruction
sem_Instruction_Alloca pc_ id_ ty_ align_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOts :: Transitions
              _idOtn :: String
              _lhsOlocals :: ([Id])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _alignIself :: Align
              _lhsOts =
                  ({-# LINE 58 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   [(_pcIself, True, _upst    , _npc    )]
                   {-# LINE 4487 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 59 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsItn
                   {-# LINE 4492 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _upst =
                  ({-# LINE 60 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   \gs -> let tw = wrap_Type (sem_Type _tyIself) $ Inh_Type { mn_Inh_Type = Nothing, mts_Inh_Type = defsorts gs }
                              pgs = updateSPC gs _lhsItn _npc
                              gs' = pgs { defsorts = mts_Syn_Type tw }
                              tysexprs = sexprs_Syn_Type tw
                              tysort   = sort_Syn_Type   tw
                          in (gs', tysexprs ++ [ declfun _idIssymbol tysort ], ISEmpty)
                   {-# LINE 4502 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _npc =
                  ({-# LINE 66 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   case nextpc _pcIself _lhsIcfg of
                        [x] -> x
                        _   -> error "Alloca npc"
                   {-# LINE 4509 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 4514 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Alloca _pcIself _idIself _tyIself _alignIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Alloca.ty.mn"
                   {-# LINE 4523 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Alloca.ty.mts"
                   {-# LINE 4528 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _alignIself) =
                  align_
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_And :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_And pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 4586 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 4591 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  And _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 4600 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.And.ty.mn"
                   {-# LINE 4605 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.And.ty.mts"
                   {-# LINE 4610 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 4615 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4620 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.And.op1.val"
                   {-# LINE 4625 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 4630 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4635 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.And.op2.val"
                   {-# LINE 4640 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_AtomicRMW :: T_PC ->
                             T_Identifier ->
                             T_Values ->
                             T_BinOp ->
                             T_AtomicOrdering ->
                             T_Instruction
sem_Instruction_AtomicRMW pc_ id_ args_ op_ ord_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _argsOmts :: (Map.Map Type SSort)
              _argsOtn :: String
              _argsOval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _argsImts :: (Map.Map Type SSort)
              _argsIself :: Values
              _argsIsexpr :: ([ISExpr])
              _argsIsexprs :: SExpressions
              _argsIvtype :: ([Type])
              _opIself :: BinOp
              _ordIself :: AtomicOrdering
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 4687 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 4692 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  AtomicRMW _pcIself _idIself _argsIself _opIself _ordIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 4701 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _argsOmts =
                  ({-# LINE 27 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.AtomicRMW.args.mts"
                   {-# LINE 4706 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _argsOtn =
                  ({-# LINE 29 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4711 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _argsOval =
                  ({-# LINE 28 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.AtomicRMW.args.val"
                   {-# LINE 4716 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _argsImts,_argsIself,_argsIsexpr,_argsIsexprs,_argsIvtype) =
                  args_ _argsOmts _argsOtn _argsOval
              ( _opIself) =
                  op_
              ( _ordIself) =
                  ord_
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_BitCast :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           T_Type ->
                           T_Instruction
sem_Instruction_BitCast pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOts :: Transitions
              _idOtn :: String
              _lhsOlocals :: ([Id])
              _lhsOself :: Instruction
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOts =
                  ({-# LINE 124 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   [(_pcIself, True, _upst    , _npc    )]
                   {-# LINE 4767 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 125 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsItn
                   {-# LINE 4772 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _upst =
                  ({-# LINE 126 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   \gs@GlobalState{..} -> let (mid, op) = value gvals _vIself
                                              (mts1, sexprs1, sexpr1) = encValue op defsorts gvals _lhsItn
                                              tw  = wrap_Type (sem_Type _tyIself) $ Inh_Type { mn_Inh_Type = Nothing, mts_Inh_Type = mts1 }
                                              pgs = updateSPC gs _lhsItn _npc
                                              gs' = pgs { defsorts = mts_Syn_Type tw }
                                              tysort = sort_Syn_Type tw
                                              tysexprs = (declfun _idIssymbol tysort):(sexprs_Syn_Type tw)
                                              sexprv2 = ISExpr $ ExtractExpr [ IdentExpr $ IdxIdent (SimpleSym "extract") [(getISize _tyIself)-1, 0] , fromISExpr $ sexpr1 ]
                                          in (gs', sexprs1 ++ tysexprs, sFn "=" _idIsexpr sexprv2)
                   {-# LINE 4785 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _npc =
                  ({-# LINE 135 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   case nextpc _pcIself _lhsIcfg of
                        [x] -> x
                        _   -> error "bitcast npc"
                   {-# LINE 4792 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 4797 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  BitCast _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.BitCast.v.mts"
                   {-# LINE 4806 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4811 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.BitCast.v.val"
                   {-# LINE 4816 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.BitCast.ty.mn"
                   {-# LINE 4821 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _vImts
                   {-# LINE 4826 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Br :: T_PC ->
                      T_Value ->
                      T_Value ->
                      T_Value ->
                      T_Instruction
sem_Instruction_Br pc_ v_ t_ f_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOts :: Transitions
              _lhsOlocals :: ([Id])
              _lhsOself :: Instruction
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tOmts :: (Map.Map Type SSort)
              _tOtn :: String
              _tOval :: Valuation
              _fOmts :: (Map.Map Type SSort)
              _fOtn :: String
              _fOval :: Valuation
              _pcIself :: PC
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tImts :: (Map.Map Type SSort)
              _tIself :: Value
              _tIsexpr :: ISExpr
              _tIsexprs :: SExpressions
              _tIvtype :: Type
              _fImts :: (Map.Map Type SSort)
              _fIself :: Value
              _fIsexpr :: ISExpr
              _fIsexprs :: SExpressions
              _fIvtype :: Type
              _lhsOts =
                  ({-# LINE 53 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   [(_pcIself, True, _upst    , _npc    )]
                   {-# LINE 4878 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _upst =
                  ({-# LINE 54 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   \gs -> (gs, [], ISEmpty)
                   {-# LINE 4883 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _npc =
                  ({-# LINE 55 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   case nextpc _pcIself _lhsIcfg of
                        [x] -> x
                        _   -> error "UBr npc"
                   {-# LINE 4890 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 4895 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Br _pcIself _vIself _tIself _fIself
              _lhsOself =
                  _self
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Br.v.mts"
                   {-# LINE 4904 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4909 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Br.v.val"
                   {-# LINE 4914 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _vImts
                   {-# LINE 4919 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4924 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Br.t.val"
                   {-# LINE 4929 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _fOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tImts
                   {-# LINE 4934 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _fOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 4939 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _fOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Br.f.val"
                   {-# LINE 4944 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tImts,_tIself,_tIsexpr,_tIsexprs,_tIvtype) =
                  t_ _tOmts _tOtn _tOval
              ( _fImts,_fIself,_fIsexpr,_fIsexprs,_fIvtype) =
                  f_ _fOmts _fOtn _fOval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Call :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Id ->
                        T_Values ->
                        T_Instruction
sem_Instruction_Call pc_ mres_ ty_ callee_ args_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOts :: Transitions
              _mresOtn :: String
              _lhsOlocals :: ([Id])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _argsOmts :: (Map.Map Type SSort)
              _argsOtn :: String
              _argsOval :: Valuation
              _pcIself :: PC
              _mresIident :: String
              _mresIlocals :: ([Id])
              _mresIself :: Identifier
              _mresIsexpr :: ISExpr
              _mresIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _calleeIself :: Id
              _argsImts :: (Map.Map Type SSort)
              _argsIself :: Values
              _argsIsexpr :: ([ISExpr])
              _argsIsexprs :: SExpressions
              _argsIvtype :: ([Type])
              _lhsOts =
                  ({-# LINE 109 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   [(_pcIself, True, _upst    , _npc    )]
                   {-# LINE 4995 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _mresOtn =
                  ({-# LINE 110 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsItn
                   {-# LINE 5000 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _upst =
                  ({-# LINE 111 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   \gs -> let rty = getFnRetTy _lhsIfdcl _calleeIself
                              tw  = wrap_Type (sem_Type rty) $ Inh_Type { mn_Inh_Type = Nothing, mts_Inh_Type = defsorts gs }
                              pgs = updateSPC gs _lhsItn _npc
                              gs' = pgs { defsorts = mts_Syn_Type tw }
                              tysexprs = sexprs_Syn_Type tw
                              tysort   = sort_Syn_Type   tw
                              csexprs  = if _mresIident == ""
                                         then []
                                         else [ declfun _mresIssymbol tysort ]
                          in (gs', tysexprs ++ csexprs, ISEmpty)
                   {-# LINE 5014 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _npc =
                  ({-# LINE 121 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   case nextpc _pcIself _lhsIcfg of
                        [x] -> x
                        _   -> error "call npc"
                   {-# LINE 5021 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _mresIlocals
                   {-# LINE 5026 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Call _pcIself _mresIself _tyIself _calleeIself _argsIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Call.ty.mn"
                   {-# LINE 5035 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Call.ty.mts"
                   {-# LINE 5040 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _argsOmts =
                  ({-# LINE 27 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 5045 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _argsOtn =
                  ({-# LINE 29 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5050 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _argsOval =
                  ({-# LINE 28 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Call.args.val"
                   {-# LINE 5055 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _mresIident,_mresIlocals,_mresIself,_mresIsexpr,_mresIssymbol) =
                  mres_ _mresOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _calleeIself) =
                  callee_
              ( _argsImts,_argsIself,_argsIsexpr,_argsIsexprs,_argsIvtype) =
                  args_ _argsOmts _argsOtn _argsOval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_CreateThread :: T_PC ->
                                T_Values ->
                                T_Instruction
sem_Instruction_CreateThread pc_ args_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOts :: Transitions
              _lhsOlocals :: ([Id])
              _lhsOself :: Instruction
              _argsOmts :: (Map.Map Type SSort)
              _argsOtn :: String
              _argsOval :: Valuation
              _pcIself :: PC
              _argsImts :: (Map.Map Type SSort)
              _argsIself :: Values
              _argsIsexpr :: ([ISExpr])
              _argsIsexprs :: SExpressions
              _argsIvtype :: ([Type])
              _lhsOts =
                  ({-# LINE 138 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   [(_pcIself, True, _upst    , _npc    )]
                   {-# LINE 5091 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _thn =
                  ({-# LINE 139 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   getFnValueName $ _argsIself !! 2
                   {-# LINE 5096 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _par =
                  ({-# LINE 140 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   getFnParams _lhsIfdcl _thn
                   {-# LINE 5101 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _upst =
                  ({-# LINE 141 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   \gs@GlobalState{..} -> let arg = _argsIself !! 3
                                              (mid, op) = value gvals arg
                                              (mts, sexprs, sexpr)    = encValue op defsorts gvals _lhsItn
                                              (mts', sexprs', sexpr') = encParameter (_par     !! 0) mts _thn
                                              pgs = updateSPC gs _lhsItn _npc
                                              gs' = pgs { defsorts = mts' }
                                          in (gs', sexprs' ++ sexprs, sFn "=" sexpr' sexpr)
                   {-# LINE 5112 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _npc =
                  ({-# LINE 148 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   case nextpc _pcIself _lhsIcfg of
                        [x] -> x
                        _   -> error "create thread npc"
                   {-# LINE 5119 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 5124 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  CreateThread _pcIself _argsIself
              _lhsOself =
                  _self
              _argsOmts =
                  ({-# LINE 27 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.CreateThread.args.mts"
                   {-# LINE 5133 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _argsOtn =
                  ({-# LINE 29 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5138 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _argsOval =
                  ({-# LINE 28 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.CreateThread.args.val"
                   {-# LINE 5143 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _argsImts,_argsIself,_argsIsexpr,_argsIsexprs,_argsIvtype) =
                  args_ _argsOmts _argsOtn _argsOval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_ExtractValue :: T_PC ->
                                T_Identifier ->
                                T_Value ->
                                T_Ints ->
                                T_Instruction
sem_Instruction_ExtractValue pc_ id_ aggr_ idxs_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _aggrOmts :: (Map.Map Type SSort)
              _aggrOtn :: String
              _aggrOval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _aggrImts :: (Map.Map Type SSort)
              _aggrIself :: Value
              _aggrIsexpr :: ISExpr
              _aggrIsexprs :: SExpressions
              _aggrIvtype :: Type
              _idxsIself :: Ints
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 5182 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 5187 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ExtractValue _pcIself _idIself _aggrIself _idxsIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5196 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _aggrOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.ExtractValue.aggr.mts"
                   {-# LINE 5201 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _aggrOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5206 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _aggrOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.ExtractValue.aggr.val"
                   {-# LINE 5211 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _aggrImts,_aggrIself,_aggrIsexpr,_aggrIsexprs,_aggrIvtype) =
                  aggr_ _aggrOmts _aggrOtn _aggrOval
              ( _idxsIself) =
                  idxs_
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_FAdd :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FAdd pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 5269 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 5274 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FAdd _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5283 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FAdd.ty.mn"
                   {-# LINE 5288 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FAdd.ty.mts"
                   {-# LINE 5293 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 5298 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5303 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FAdd.op1.val"
                   {-# LINE 5308 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 5313 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5318 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FAdd.op2.val"
                   {-# LINE 5323 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_FCmp :: T_PC ->
                        T_Identifier ->
                        T_RealPredicate ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FCmp pc_ id_ cond_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _condIself :: RealPredicate
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 5385 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 5390 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FCmp _pcIself _idIself _condIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5399 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FCmp.ty.mn"
                   {-# LINE 5404 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FCmp.ty.mts"
                   {-# LINE 5409 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 5414 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5419 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FCmp.op1.val"
                   {-# LINE 5424 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 5429 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5434 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FCmp.op2.val"
                   {-# LINE 5439 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _condIself) =
                  cond_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_FDiv :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FDiv pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 5501 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 5506 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5515 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FDiv.ty.mn"
                   {-# LINE 5520 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FDiv.ty.mts"
                   {-# LINE 5525 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 5530 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5535 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FDiv.op1.val"
                   {-# LINE 5540 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 5545 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5550 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FDiv.op2.val"
                   {-# LINE 5555 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_FMul :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FMul pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 5615 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 5620 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FMul _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5629 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FMul.ty.mn"
                   {-# LINE 5634 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FMul.ty.mts"
                   {-# LINE 5639 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 5644 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5649 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FMul.op1.val"
                   {-# LINE 5654 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 5659 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5664 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FMul.op2.val"
                   {-# LINE 5669 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_FPExt :: T_PC ->
                         T_Identifier ->
                         T_Value ->
                         T_Type ->
                         T_Instruction
sem_Instruction_FPExt pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 5720 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 5725 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FPExt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5734 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FPExt.v.mts"
                   {-# LINE 5739 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5744 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FPExt.v.val"
                   {-# LINE 5749 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FPExt.ty.mn"
                   {-# LINE 5754 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _vImts
                   {-# LINE 5759 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_FPToSI :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_FPToSI pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 5808 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 5813 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FPToSI _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5822 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FPToSI.v.mts"
                   {-# LINE 5827 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5832 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FPToSI.v.val"
                   {-# LINE 5837 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FPToSI.ty.mn"
                   {-# LINE 5842 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _vImts
                   {-# LINE 5847 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_FPToUI :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_FPToUI pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 5896 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 5901 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FPToUI _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5910 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FPToUI.v.mts"
                   {-# LINE 5915 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 5920 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FPToUI.v.val"
                   {-# LINE 5925 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FPToUI.ty.mn"
                   {-# LINE 5930 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _vImts
                   {-# LINE 5935 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_FPTrunc :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           T_Type ->
                           T_Instruction
sem_Instruction_FPTrunc pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 5984 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 5989 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FPTrunc _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 5998 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FPTrunc.v.mts"
                   {-# LINE 6003 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6008 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FPTrunc.v.val"
                   {-# LINE 6013 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FPTrunc.ty.mn"
                   {-# LINE 6018 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _vImts
                   {-# LINE 6023 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_FRem :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FRem pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 6081 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 6086 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FRem _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6095 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FRem.ty.mn"
                   {-# LINE 6100 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FRem.ty.mts"
                   {-# LINE 6105 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6110 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6115 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FRem.op1.val"
                   {-# LINE 6120 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 6125 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6130 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FRem.op2.val"
                   {-# LINE 6135 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_FSub :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FSub pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 6195 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 6200 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  FSub _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6209 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FSub.ty.mn"
                   {-# LINE 6214 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.FSub.ty.mts"
                   {-# LINE 6219 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6224 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6229 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FSub.op1.val"
                   {-# LINE 6234 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 6239 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6244 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.FSub.op2.val"
                   {-# LINE 6249 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_GetElementPtr :: T_PC ->
                                 T_Identifier ->
                                 T_Type ->
                                 T_Value ->
                                 T_Values ->
                                 T_Instruction
sem_Instruction_GetElementPtr pc_ id_ ty_ struct_ idxs_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _structOmts :: (Map.Map Type SSort)
              _structOtn :: String
              _structOval :: Valuation
              _idxsOmts :: (Map.Map Type SSort)
              _idxsOtn :: String
              _idxsOval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _structImts :: (Map.Map Type SSort)
              _structIself :: Value
              _structIsexpr :: ISExpr
              _structIsexprs :: SExpressions
              _structIvtype :: Type
              _idxsImts :: (Map.Map Type SSort)
              _idxsIself :: Values
              _idxsIsexpr :: ([ISExpr])
              _idxsIsexprs :: SExpressions
              _idxsIvtype :: ([Type])
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 6309 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 6314 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  GetElementPtr _pcIself _idIself _tyIself _structIself _idxsIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6323 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.GetElementPtr.ty.mn"
                   {-# LINE 6328 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.GetElementPtr.ty.mts"
                   {-# LINE 6333 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _structOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6338 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _structOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6343 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _structOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.GetElementPtr.struct.val"
                   {-# LINE 6348 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _idxsOmts =
                  ({-# LINE 27 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _structImts
                   {-# LINE 6353 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _idxsOtn =
                  ({-# LINE 29 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6358 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _idxsOval =
                  ({-# LINE 28 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.GetElementPtr.idxs.val"
                   {-# LINE 6363 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _structImts,_structIself,_structIsexpr,_structIsexprs,_structIvtype) =
                  struct_ _structOmts _structOtn _structOval
              ( _idxsImts,_idxsIself,_idxsIsexpr,_idxsIsexprs,_idxsIvtype) =
                  idxs_ _idxsOmts _idxsOtn _idxsOval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_ICmp :: T_PC ->
                        T_Identifier ->
                        T_IntPredicate ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_ICmp pc_ id_ cond_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _condIself :: IntPredicate
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 6425 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 6430 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ICmp _pcIself _idIself _condIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6439 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.ICmp.ty.mn"
                   {-# LINE 6444 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.ICmp.ty.mts"
                   {-# LINE 6449 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6454 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6459 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.ICmp.op1.val"
                   {-# LINE 6464 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 6469 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6474 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.ICmp.op2.val"
                   {-# LINE 6479 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _condIself) =
                  cond_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_IntToPtr :: T_PC ->
                            T_Identifier ->
                            T_Value ->
                            T_Type ->
                            T_Instruction
sem_Instruction_IntToPtr pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 6532 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 6537 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  IntToPtr _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6546 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.IntToPtr.v.mts"
                   {-# LINE 6551 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6556 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.IntToPtr.v.val"
                   {-# LINE 6561 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.IntToPtr.ty.mn"
                   {-# LINE 6566 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _vImts
                   {-# LINE 6571 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_LShr :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_LShr pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 6629 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 6634 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  LShr _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6643 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.LShr.ty.mn"
                   {-# LINE 6648 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.LShr.ty.mts"
                   {-# LINE 6653 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6658 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6663 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.LShr.op1.val"
                   {-# LINE 6668 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 6673 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6678 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.LShr.op2.val"
                   {-# LINE 6683 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Load :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Align ->
                        T_Instruction
sem_Instruction_Load pc_ id_ v_ align_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOts :: Transitions
              _idOtn :: String
              _lhsOlocals :: ([Id])
              _lhsOself :: Instruction
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _alignIself :: Align
              _lhsOts =
                  ({-# LINE 69 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   [(_pcIself, True, _upst    , _npc    )]
                   {-# LINE 6728 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _idOtn =
                  ({-# LINE 70 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsItn
                   {-# LINE 6733 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _upst =
                  ({-# LINE 71 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   \gs@GlobalState{..} -> let (mi,op) = value gvals _vIself
                                              tw = wrap_Type (sem_Type $ getValueType op) $ Inh_Type { mn_Inh_Type = Nothing, mts_Inh_Type = defsorts }
                                              tysort = sort_Syn_Type tw
                                              dexpr = [ declfun _idIssymbol tysort ]
                                              symexpr = ISExpr $ IdentExpr $ SymIdent _idIssymbol
                                              gs' = updateSPC gs _lhsItn _npc
                                              vexpr =  case mi of
                                                            Nothing -> let (_,_,viexpr) = encValue op defsorts gvals _lhsItn
                                                                       in viexpr
                                                            Just miv -> ISExpr $ IdentExpr $ SymIdent $ SimpleSym miv
                                          in (gs', dexpr, sFn "=" symexpr vexpr)
                   {-# LINE 6748 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _npc =
                  ({-# LINE 82 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   case nextpc _pcIself _lhsIcfg of
                        [x] -> x
                        _   -> error "load npc"
                   {-# LINE 6755 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 6760 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Load _pcIself _idIself _vIself _alignIself
              _lhsOself =
                  _self
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Load.v.mts"
                   {-# LINE 6769 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6774 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Load.v.val"
                   {-# LINE 6779 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _alignIself) =
                  align_
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Mul :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Mul pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 6837 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 6842 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Mul _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6851 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Mul.ty.mn"
                   {-# LINE 6856 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Mul.ty.mts"
                   {-# LINE 6861 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6866 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6871 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Mul.op1.val"
                   {-# LINE 6876 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 6881 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6886 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Mul.op2.val"
                   {-# LINE 6891 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Or :: T_PC ->
                      T_Identifier ->
                      T_Type ->
                      T_Value ->
                      T_Value ->
                      T_Instruction
sem_Instruction_Or pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 6951 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 6956 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Or _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 6965 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Or.ty.mn"
                   {-# LINE 6970 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Or.ty.mts"
                   {-# LINE 6975 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 6980 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 6985 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Or.op1.val"
                   {-# LINE 6990 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 6995 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7000 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Or.op2.val"
                   {-# LINE 7005 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_PHI :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_PValues ->
                       T_Instruction
sem_Instruction_PHI pc_ id_ ty_ vals_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _valsIself :: PValues
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 7049 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 7054 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  PHI _pcIself _idIself _tyIself _valsIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7063 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.PHI.ty.mn"
                   {-# LINE 7068 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.PHI.ty.mts"
                   {-# LINE 7073 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _valsIself) =
                  vals_
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_PtrToInt :: T_PC ->
                            T_Identifier ->
                            T_Value ->
                            T_Type ->
                            T_Instruction
sem_Instruction_PtrToInt pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 7122 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 7127 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  PtrToInt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7136 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.PtrToInt.v.mts"
                   {-# LINE 7141 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7146 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.PtrToInt.v.val"
                   {-# LINE 7151 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.PtrToInt.ty.mn"
                   {-# LINE 7156 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _vImts
                   {-# LINE 7161 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Ret :: T_PC ->
                       T_RetInst ->
                       T_Instruction
sem_Instruction_Ret pc_ r_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOts :: Transitions
              _lhsOlocals :: ([Id])
              _lhsOself :: Instruction
              _pcIself :: PC
              _rIself :: RetInst
              _lhsOts =
                  ({-# LINE 47 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   [(_pcIself, False, undefined, undefined)]
                   {-# LINE 7188 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 7193 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Ret _pcIself _rIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _rIself) =
                  r_
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_SDiv :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_SDiv pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 7251 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 7256 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  SDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7265 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.SDiv.ty.mn"
                   {-# LINE 7270 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.SDiv.ty.mts"
                   {-# LINE 7275 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 7280 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7285 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.SDiv.op1.val"
                   {-# LINE 7290 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 7295 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7300 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.SDiv.op2.val"
                   {-# LINE 7305 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_SExt :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Type ->
                        T_Instruction
sem_Instruction_SExt pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 7356 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 7361 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  SExt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7370 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.SExt.v.mts"
                   {-# LINE 7375 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7380 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.SExt.v.val"
                   {-# LINE 7385 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.SExt.ty.mn"
                   {-# LINE 7390 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _vImts
                   {-# LINE 7395 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_SIToFP :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_SIToFP pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 7444 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 7449 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  SIToFP _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7458 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.SIToFP.v.mts"
                   {-# LINE 7463 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7468 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.SIToFP.v.val"
                   {-# LINE 7473 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.SIToFP.ty.mn"
                   {-# LINE 7478 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _vImts
                   {-# LINE 7483 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_SRem :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_SRem pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 7541 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 7546 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  SRem _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7555 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.SRem.ty.mn"
                   {-# LINE 7560 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.SRem.ty.mts"
                   {-# LINE 7565 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 7570 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7575 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.SRem.op1.val"
                   {-# LINE 7580 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 7585 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7590 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.SRem.op2.val"
                   {-# LINE 7595 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Select :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Value ->
                          T_Value ->
                          T_Instruction
sem_Instruction_Select pc_ id_ cond_ valt_ valf_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _condOmts :: (Map.Map Type SSort)
              _condOtn :: String
              _condOval :: Valuation
              _valtOmts :: (Map.Map Type SSort)
              _valtOtn :: String
              _valtOval :: Valuation
              _valfOmts :: (Map.Map Type SSort)
              _valfOtn :: String
              _valfOval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _condImts :: (Map.Map Type SSort)
              _condIself :: Value
              _condIsexpr :: ISExpr
              _condIsexprs :: SExpressions
              _condIvtype :: Type
              _valtImts :: (Map.Map Type SSort)
              _valtIself :: Value
              _valtIsexpr :: ISExpr
              _valtIsexprs :: SExpressions
              _valtIvtype :: Type
              _valfImts :: (Map.Map Type SSort)
              _valfIself :: Value
              _valfIsexpr :: ISExpr
              _valfIsexprs :: SExpressions
              _valfIvtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 7656 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 7661 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Select _pcIself _idIself _condIself _valtIself _valfIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7670 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _condOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Select.cond.mts"
                   {-# LINE 7675 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _condOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7680 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _condOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Select.cond.val"
                   {-# LINE 7685 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valtOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _condImts
                   {-# LINE 7690 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valtOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7695 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valtOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Select.valt.val"
                   {-# LINE 7700 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valfOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _valtImts
                   {-# LINE 7705 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valfOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7710 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _valfOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Select.valf.val"
                   {-# LINE 7715 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _condImts,_condIself,_condIsexpr,_condIsexprs,_condIvtype) =
                  cond_ _condOmts _condOtn _condOval
              ( _valtImts,_valtIself,_valtIsexpr,_valtIsexprs,_valtIvtype) =
                  valt_ _valtOmts _valtOtn _valtOval
              ( _valfImts,_valfIself,_valfIsexpr,_valfIsexprs,_valfIvtype) =
                  valf_ _valfOmts _valfOtn _valfOval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Shl :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Shl pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 7775 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 7780 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Shl _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 7789 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Shl.ty.mn"
                   {-# LINE 7794 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Shl.ty.mts"
                   {-# LINE 7799 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 7804 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7809 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Shl.op1.val"
                   {-# LINE 7814 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 7819 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7824 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Shl.op2.val"
                   {-# LINE 7829 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Store :: T_PC ->
                         T_Type ->
                         T_Value ->
                         T_Value ->
                         T_Align ->
                         T_Instruction
sem_Instruction_Store pc_ ty_ v1_ v2_ align_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOts :: Transitions
              _lhsOlocals :: ([Id])
              _lhsOself :: Instruction
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _v1Omts :: (Map.Map Type SSort)
              _v1Otn :: String
              _v1Oval :: Valuation
              _v2Omts :: (Map.Map Type SSort)
              _v2Otn :: String
              _v2Oval :: Valuation
              _pcIself :: PC
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _v1Imts :: (Map.Map Type SSort)
              _v1Iself :: Value
              _v1Isexpr :: ISExpr
              _v1Isexprs :: SExpressions
              _v1Ivtype :: Type
              _v2Imts :: (Map.Map Type SSort)
              _v2Iself :: Value
              _v2Isexpr :: ISExpr
              _v2Isexprs :: SExpressions
              _v2Ivtype :: Type
              _alignIself :: Align
              _lhsOts =
                  ({-# LINE 85 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   [(_pcIself, True, _upst    , _npc    )]
                   {-# LINE 7884 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _upst =
                  ({-# LINE 86 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   \gs@GlobalState{..} -> let (mi1,op1) = value gvals _v1Iself
                                              (mi2,op2) = value gvals _v2Iself
                                              (mts1, sexprs1, sexpr1) = encValue op1 defsorts gvals _lhsItn
                                          in  case mi2 of
                                                  Nothing   -> let (mts2, sexprs2, sexpr2) = encValue _v2Iself mts1 gvals _lhsItn
                                                                   pgs = updateSPC gs _lhsItn _npc
                                                                   gs' = pgs { defsorts = mts2, gvals = upValuation gvals op2 op1}
                                                               in (gs', sexprs1 ++ sexprs2, sFn "=" sexpr2 sexpr1)
                                                  Just mi2a -> let mi2a' = freshId mi2a
                                                                   gvals' = Map.adjust (\_ -> Left mi2a') mi2a $ Map.insert mi2a' (Right op1) $ gvals
                                                                   tw = wrap_Type (sem_Type $ getValueType _v2Iself) $ Inh_Type { mn_Inh_Type = Nothing, mts_Inh_Type = mts1 }
                                                                   sym = SimpleSym mi2a'
                                                                   symexpr = ISExpr $ IdentExpr $ SymIdent sym
                                                                   tysexprs = sexprs_Syn_Type tw
                                                                   tysort   = sort_Syn_Type   tw
                                                                   mts2     = mts_Syn_Type    tw
                                                                   pgs = updateSPC gs _lhsItn _npc
                                                                   gs' = pgs { defsorts = mts2, gvals = gvals'}
                                                               in (gs', sexprs1 ++ tysexprs ++ [ declfun sym tysort ], sFn "=" symexpr sexpr1)
                   {-# LINE 7907 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _npc =
                  ({-# LINE 105 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   case nextpc _pcIself _lhsIcfg of
                        [x] -> x
                        _   -> error "Store npc"
                   {-# LINE 7914 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 7919 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Store _pcIself _tyIself _v1Iself _v2Iself _alignIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Store.ty.mn"
                   {-# LINE 7928 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Store.ty.mts"
                   {-# LINE 7933 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _v1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 7938 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _v1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7943 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _v1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Store.v1.val"
                   {-# LINE 7948 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _v2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _v1Imts
                   {-# LINE 7953 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _v2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 7958 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _v2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Store.v2.val"
                   {-# LINE 7963 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _v1Imts,_v1Iself,_v1Isexpr,_v1Isexprs,_v1Ivtype) =
                  v1_ _v1Omts _v1Otn _v1Oval
              ( _v2Imts,_v2Iself,_v2Isexpr,_v2Isexprs,_v2Ivtype) =
                  v2_ _v2Omts _v2Otn _v2Oval
              ( _alignIself) =
                  align_
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Sub :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Sub pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 8023 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8028 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Sub _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8037 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Sub.ty.mn"
                   {-# LINE 8042 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Sub.ty.mts"
                   {-# LINE 8047 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 8052 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8057 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Sub.op1.val"
                   {-# LINE 8062 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 8067 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8072 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Sub.op2.val"
                   {-# LINE 8077 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Switch :: T_PC ->
                          T_IntTyValIdL ->
                          T_Instruction
sem_Instruction_Switch pc_ elems_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _pcIself :: PC
              _elemsIself :: IntTyValIdL
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8106 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8111 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Switch _pcIself _elemsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _elemsIself) =
                  elems_
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Trunc :: T_PC ->
                         T_Identifier ->
                         T_Value ->
                         T_Type ->
                         T_Instruction
sem_Instruction_Trunc pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 8160 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8165 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Trunc _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8174 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Trunc.v.mts"
                   {-# LINE 8179 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8184 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Trunc.v.val"
                   {-# LINE 8189 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Trunc.ty.mn"
                   {-# LINE 8194 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _vImts
                   {-# LINE 8199 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_UBr :: T_PC ->
                       T_Value ->
                       T_Instruction
sem_Instruction_UBr pc_ d_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOts :: Transitions
              _lhsOlocals :: ([Id])
              _lhsOself :: Instruction
              _dOmts :: (Map.Map Type SSort)
              _dOtn :: String
              _dOval :: Valuation
              _pcIself :: PC
              _dImts :: (Map.Map Type SSort)
              _dIself :: Value
              _dIsexpr :: ISExpr
              _dIsexprs :: SExpressions
              _dIvtype :: Type
              _lhsOts =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   [(_pcIself, True, _upst    , _npc    )]
                   {-# LINE 8233 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _upst =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   \gs -> (gs, [], ISEmpty)
                   {-# LINE 8238 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _npc =
                  ({-# LINE 50 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   case nextpc _pcIself _lhsIcfg of
                        [x] -> x
                        _   -> error "UBr npc"
                   {-# LINE 8245 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8250 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  UBr _pcIself _dIself
              _lhsOself =
                  _self
              _dOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.UBr.d.mts"
                   {-# LINE 8259 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _dOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8264 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _dOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.UBr.d.val"
                   {-# LINE 8269 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _dImts,_dIself,_dIsexpr,_dIsexprs,_dIvtype) =
                  d_ _dOmts _dOtn _dOval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_UDiv :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_UDiv pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 8323 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8328 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  UDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8337 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.UDiv.ty.mn"
                   {-# LINE 8342 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.UDiv.ty.mts"
                   {-# LINE 8347 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 8352 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8357 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.UDiv.op1.val"
                   {-# LINE 8362 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 8367 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8372 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.UDiv.op2.val"
                   {-# LINE 8377 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_UIToFP :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_UIToFP pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 8428 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8433 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  UIToFP _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8442 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.UIToFP.v.mts"
                   {-# LINE 8447 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8452 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.UIToFP.v.val"
                   {-# LINE 8457 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.UIToFP.ty.mn"
                   {-# LINE 8462 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _vImts
                   {-# LINE 8467 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_URem :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_URem pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 8525 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8530 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  URem _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8539 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.URem.ty.mn"
                   {-# LINE 8544 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.URem.ty.mts"
                   {-# LINE 8549 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 8554 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8559 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.URem.op1.val"
                   {-# LINE 8564 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 8569 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8574 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.URem.op2.val"
                   {-# LINE 8579 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Unreachable :: T_PC ->
                               T_Instruction
sem_Instruction_Unreachable pc_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _pcIself :: PC
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8606 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8611 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Unreachable _pcIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_Xor :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Xor pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _op1Omts :: (Map.Map Type SSort)
              _op1Otn :: String
              _op1Oval :: Valuation
              _op2Omts :: (Map.Map Type SSort)
              _op2Otn :: String
              _op2Oval :: Valuation
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _op1Imts :: (Map.Map Type SSort)
              _op1Iself :: Value
              _op1Isexpr :: ISExpr
              _op1Isexprs :: SExpressions
              _op1Ivtype :: Type
              _op2Imts :: (Map.Map Type SSort)
              _op2Iself :: Value
              _op2Isexpr :: ISExpr
              _op2Isexprs :: SExpressions
              _op2Ivtype :: Type
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 8667 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8672 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Xor _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8681 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Xor.ty.mn"
                   {-# LINE 8686 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.Xor.ty.mts"
                   {-# LINE 8691 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 8696 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8701 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op1Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Xor.op1.val"
                   {-# LINE 8706 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Omts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _op1Imts
                   {-# LINE 8711 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Otn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8716 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _op2Oval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.Xor.op2.val"
                   {-# LINE 8721 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
              ( _op1Imts,_op1Iself,_op1Isexpr,_op1Isexprs,_op1Ivtype) =
                  op1_ _op1Omts _op1Otn _op1Oval
              ( _op2Imts,_op2Iself,_op2Isexpr,_op2Isexprs,_op2Ivtype) =
                  op2_ _op2Omts _op2Otn _op2Oval
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instruction_ZExt :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Type ->
                        T_Instruction
sem_Instruction_ZExt pc_ id_ v_ ty_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instruction
              _idOtn :: String
              _vOmts :: (Map.Map Type SSort)
              _vOtn :: String
              _vOval :: Valuation
              _tyOmn :: (Maybe SSort)
              _tyOmts :: (Map.Map Type SSort)
              _pcIself :: PC
              _idIident :: String
              _idIlocals :: ([Id])
              _idIself :: Identifier
              _idIsexpr :: ISExpr
              _idIssymbol :: SSymbol
              _vImts :: (Map.Map Type SSort)
              _vIself :: Value
              _vIsexpr :: ISExpr
              _vIsexprs :: SExpressions
              _vIvtype :: Type
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _idIlocals
                   {-# LINE 8772 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8777 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  ZExt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              _idOtn =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
                   _lhsItn
                   {-# LINE 8786 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.ZExt.v.mts"
                   {-# LINE 8791 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 8796 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   error "missing rule: Instruction.ZExt.v.val"
                   {-# LINE 8801 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   error "missing rule: Instruction.ZExt.ty.mn"
                   {-# LINE 8806 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _vImts
                   {-# LINE 8811 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _idIident,_idIlocals,_idIself,_idIsexpr,_idIssymbol) =
                  id_ _idOtn
              ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
                  v_ _vOmts _vOtn _vOval
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
-- Instructions ------------------------------------------------
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = ([(PC,PC)]) ->
                      (Map.Map Identifier Type) ->
                      Declarations ->
                      Id ->
                      ( ([Id]),Instructions,Transitions)
data Inh_Instructions = Inh_Instructions {cfg_Inh_Instructions :: ([(PC,PC)]),dfg_Inh_Instructions :: (Map.Map Identifier Type),fdcl_Inh_Instructions :: Declarations,tn_Inh_Instructions :: Id}
data Syn_Instructions = Syn_Instructions {locals_Syn_Instructions :: ([Id]),self_Syn_Instructions :: Instructions,ts_Syn_Instructions :: Transitions}
wrap_Instructions :: T_Instructions ->
                     Inh_Instructions ->
                     Syn_Instructions
wrap_Instructions sem (Inh_Instructions _lhsIcfg _lhsIdfg _lhsIfdcl _lhsItn) =
    (let ( _lhsOlocals,_lhsOself,_lhsOts) = sem _lhsIcfg _lhsIdfg _lhsIfdcl _lhsItn
     in  (Syn_Instructions _lhsOlocals _lhsOself _lhsOts))
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instructions
              _hdOcfg :: ([(PC,PC)])
              _hdOdfg :: (Map.Map Identifier Type)
              _hdOfdcl :: Declarations
              _hdOtn :: Id
              _tlOcfg :: ([(PC,PC)])
              _tlOdfg :: (Map.Map Identifier Type)
              _tlOfdcl :: Declarations
              _tlOtn :: Id
              _hdIlocals :: ([Id])
              _hdIself :: Instruction
              _hdIts :: Transitions
              _tlIlocals :: ([Id])
              _tlIself :: Instructions
              _tlIts :: Transitions
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _hdIlocals ++ _tlIlocals
                   {-# LINE 8870 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _hdIts ++ _tlIts
                   {-# LINE 8875 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOcfg =
                  ({-# LINE 33 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIcfg
                   {-# LINE 8884 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _hdOdfg =
                  ({-# LINE 35 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIdfg
                   {-# LINE 8889 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _hdOfdcl =
                  ({-# LINE 34 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIfdcl
                   {-# LINE 8894 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _hdOtn =
                  ({-# LINE 36 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsItn
                   {-# LINE 8899 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOcfg =
                  ({-# LINE 33 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIcfg
                   {-# LINE 8904 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOdfg =
                  ({-# LINE 35 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIdfg
                   {-# LINE 8909 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOfdcl =
                  ({-# LINE 34 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsIfdcl
                   {-# LINE 8914 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 36 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   _lhsItn
                   {-# LINE 8919 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _hdIlocals,_hdIself,_hdIts) =
                  hd_ _hdOcfg _hdOdfg _hdOfdcl _hdOtn
              ( _tlIlocals,_tlIself,_tlIts) =
                  tl_ _tlOcfg _tlOdfg _tlOfdcl _tlOtn
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    (\ _lhsIcfg
       _lhsIdfg
       _lhsIfdcl
       _lhsItn ->
         (let _lhsOlocals :: ([Id])
              _lhsOts :: Transitions
              _lhsOself :: Instructions
              _lhsOlocals =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8938 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOts =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
                   []
                   {-# LINE 8943 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOlocals,_lhsOself,_lhsOts)))
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
type T_IntPredicate = ( IntPredicate)
data Inh_IntPredicate = Inh_IntPredicate {}
data Syn_IntPredicate = Syn_IntPredicate {self_Syn_IntPredicate :: IntPredicate}
wrap_IntPredicate :: T_IntPredicate ->
                     Inh_IntPredicate ->
                     Syn_IntPredicate
wrap_IntPredicate sem (Inh_IntPredicate) =
    (let ( _lhsOself) = sem
     in  (Syn_IntPredicate _lhsOself))
sem_IntPredicate_IntEQ :: T_IntPredicate
sem_IntPredicate_IntEQ =
    (let _lhsOself :: IntPredicate
         _self =
             IntEQ
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_IntPredicate_IntNE :: T_IntPredicate
sem_IntPredicate_IntNE =
    (let _lhsOself :: IntPredicate
         _self =
             IntNE
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_IntPredicate_IntSGE :: T_IntPredicate
sem_IntPredicate_IntSGE =
    (let _lhsOself :: IntPredicate
         _self =
             IntSGE
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_IntPredicate_IntSGT :: T_IntPredicate
sem_IntPredicate_IntSGT =
    (let _lhsOself :: IntPredicate
         _self =
             IntSGT
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_IntPredicate_IntSLE :: T_IntPredicate
sem_IntPredicate_IntSLE =
    (let _lhsOself :: IntPredicate
         _self =
             IntSLE
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_IntPredicate_IntSLT :: T_IntPredicate
sem_IntPredicate_IntSLT =
    (let _lhsOself :: IntPredicate
         _self =
             IntSLT
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_IntPredicate_IntUGE :: T_IntPredicate
sem_IntPredicate_IntUGE =
    (let _lhsOself :: IntPredicate
         _self =
             IntUGE
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_IntPredicate_IntUGT :: T_IntPredicate
sem_IntPredicate_IntUGT =
    (let _lhsOself :: IntPredicate
         _self =
             IntUGT
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_IntPredicate_IntULE :: T_IntPredicate
sem_IntPredicate_IntULE =
    (let _lhsOself :: IntPredicate
         _self =
             IntULE
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_IntPredicate_IntULT :: T_IntPredicate
sem_IntPredicate_IntULT =
    (let _lhsOself :: IntPredicate
         _self =
             IntULT
         _lhsOself =
             _self
     in  ( _lhsOself))
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
         _x1Omts :: (Map.Map Type SSort)
         _x2Omts :: (Map.Map Type SSort)
         _x2Otn :: String
         _x2Oval :: Valuation
         _x3Otn :: String
         _x1Imts :: (Map.Map Type SSort)
         _x1Iself :: Type
         _x1Isexprs :: SExpressions
         _x1Isort :: SSortExpr
         _x1Isortn :: SSort
         _x2Imts :: (Map.Map Type SSort)
         _x2Iself :: Value
         _x2Isexpr :: ISExpr
         _x2Isexprs :: SExpressions
         _x2Ivtype :: Type
         _x3Iident :: String
         _x3Ilocals :: ([Id])
         _x3Iself :: Identifier
         _x3Isexpr :: ISExpr
         _x3Issymbol :: SSymbol
         _self =
             (_x1Iself,_x2Iself,_x3Iself)
         _lhsOself =
             _self
         _x1Omn =
             ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
              error "missing rule: IntTyValId.Tuple.x1.mn"
              {-# LINE 9114 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x1Omts =
             ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
              error "missing rule: IntTyValId.Tuple.x1.mts"
              {-# LINE 9119 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x2Omts =
             ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              _x1Imts
              {-# LINE 9124 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x2Otn =
             ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: IntTyValId.Tuple.x2.tn"
              {-# LINE 9129 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x2Oval =
             ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: IntTyValId.Tuple.x2.val"
              {-# LINE 9134 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x3Otn =
             ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
              error "missing rule: IntTyValId.Tuple.x3.tn"
              {-# LINE 9139 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         ( _x1Imts,_x1Iself,_x1Isexprs,_x1Isort,_x1Isortn) =
             x1_ _x1Omn _x1Omts
         ( _x2Imts,_x2Iself,_x2Isexpr,_x2Isexprs,_x2Ivtype) =
             x2_ _x2Omts _x2Otn _x2Oval
         ( _x3Iident,_x3Ilocals,_x3Iself,_x3Isexpr,_x3Issymbol) =
             x3_ _x3Otn
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
    (sem_MConstant_Just x)
sem_MConstant Prelude.Nothing =
    sem_MConstant_Nothing
-- semantic domain
type T_MConstant = ( MConstant)
data Inh_MConstant = Inh_MConstant {}
data Syn_MConstant = Syn_MConstant {self_Syn_MConstant :: MConstant}
wrap_MConstant :: T_MConstant ->
                  Inh_MConstant ->
                  Syn_MConstant
wrap_MConstant sem (Inh_MConstant) =
    (let ( _lhsOself) = sem
     in  (Syn_MConstant _lhsOself))
sem_MConstant_Just :: Bool ->
                      T_MConstant
sem_MConstant_Just just_ =
    (let _lhsOself :: MConstant
         _self =
             Just just_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_MConstant_Nothing :: T_MConstant
sem_MConstant_Nothing =
    (let _lhsOself :: MConstant
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
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
         _justOtn :: String
         _justIident :: String
         _justIlocals :: ([Id])
         _justIself :: Identifier
         _justIsexpr :: ISExpr
         _justIssymbol :: SSymbol
         _self =
             Just _justIself
         _lhsOself =
             _self
         _justOtn =
             ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Identifier.ag" #-}
              error "missing rule: MIdentifier.Just.just.tn"
              {-# LINE 9732 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         ( _justIident,_justIlocals,_justIself,_justIsexpr,_justIssymbol) =
             just_ _justOtn
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
type T_MValue = (Map.Map Type SSort) ->
                String ->
                Valuation ->
                ( (Map.Map Type SSort),MValue,ISExpr,SExpressions)
data Inh_MValue = Inh_MValue {mts_Inh_MValue :: (Map.Map Type SSort),tn_Inh_MValue :: String,val_Inh_MValue :: Valuation}
data Syn_MValue = Syn_MValue {mts_Syn_MValue :: (Map.Map Type SSort),self_Syn_MValue :: MValue,sexpr_Syn_MValue :: ISExpr,sexprs_Syn_MValue :: SExpressions}
wrap_MValue :: T_MValue ->
               Inh_MValue ->
               Syn_MValue
wrap_MValue sem (Inh_MValue _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_MValue _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs))
sem_MValue_Just :: T_Value ->
                   T_MValue
sem_MValue_Just just_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOself :: MValue
              _lhsOmts :: (Map.Map Type SSort)
              _justOmts :: (Map.Map Type SSort)
              _justOtn :: String
              _justOval :: Valuation
              _justImts :: (Map.Map Type SSort)
              _justIself :: Value
              _justIsexpr :: ISExpr
              _justIsexprs :: SExpressions
              _justIvtype :: Type
              _lhsOsexpr =
                  ({-# LINE 23 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _justIsexpr
                   {-# LINE 9974 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 24 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _justIsexprs
                   {-# LINE 9979 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Just _justIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _justImts
                   {-# LINE 9988 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _justOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 9993 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _justOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 9998 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _justOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 10003 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _justImts,_justIself,_justIsexpr,_justIsexprs,_justIvtype) =
                  just_ _justOmts _justOtn _justOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs)))
sem_MValue_Nothing :: T_MValue
sem_MValue_Nothing =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOself :: MValue
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr =
                  ({-# LINE 20 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   ISEmpty
                   {-# LINE 10020 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 21 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 10025 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Nothing
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 12 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 10034 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs)))
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
         _funsOccfg :: ControlFlow
         _funsOcdfg :: (Map.Map String (Map.Map Identifier Type))
         _funsOfdcl :: Declarations
         _layoutIself :: DataLayout
         _targetIself :: TargetData
         _gvarsIgs :: GlobalState
         _gvarsIself :: Globals
         _gvarsIsexpr :: ISExpr
         _gvarsIsexprs :: SExpressions
         _funsIcts :: (Map.Map String Transitions)
         _funsIself :: Functions
         _nmdtysIself :: NamedTypes
         _self =
             Module id_ _layoutIself _targetIself _gvarsIself _funsIself _nmdtysIself
         _lhsOself =
             _self
         _gvarsOgs =
             ({-# LINE 20 "src/Concurrent/Model/ESEncoder/Global.ag" #-}
              error "missing rule: Module.Module.gvars.gs"
              {-# LINE 10161 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _funsOccfg =
             ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
              error "missing rule: Module.Module.funs.ccfg"
              {-# LINE 10166 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _funsOcdfg =
             ({-# LINE 18 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
              error "missing rule: Module.Module.funs.cdfg"
              {-# LINE 10171 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _funsOfdcl =
             ({-# LINE 17 "src/Concurrent/Model/ESEncoder/Function.ag" #-}
              error "missing rule: Module.Module.funs.fdcl"
              {-# LINE 10176 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         ( _layoutIself) =
             layout_
         ( _targetIself) =
             target_
         ( _gvarsIgs,_gvarsIself,_gvarsIsexpr,_gvarsIsexprs) =
             gvars_ _gvarsOgs
         ( _funsIcts,_funsIself) =
             funs_ _funsOccfg _funsOcdfg _funsOfdcl
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
         _valOmts :: (Map.Map Type SSort)
         _valImts :: (Map.Map Type SSort)
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
             ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
              error "missing rule: NamedTypes.Entry.val.mn"
              {-# LINE 10291 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _valOmts =
             ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
              error "missing rule: NamedTypes.Entry.val.mts"
              {-# LINE 10296 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
         _x1Omts :: (Map.Map Type SSort)
         _x1Imts :: (Map.Map Type SSort)
         _x1Iself :: Type
         _x1Isexprs :: SExpressions
         _x1Isort :: SSortExpr
         _x1Isortn :: SSort
         _self =
             (_x1Iself,x2_)
         _lhsOself =
             _self
         _x1Omn =
             ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
              error "missing rule: PTyInt.Tuple.x1.mn"
              {-# LINE 10371 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x1Omts =
             ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
              error "missing rule: PTyInt.Tuple.x1.mts"
              {-# LINE 10376 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
         _x1Omts :: (Map.Map Type SSort)
         _x1Otn :: String
         _x1Oval :: Valuation
         _x2Omts :: (Map.Map Type SSort)
         _x2Otn :: String
         _x2Oval :: Valuation
         _x1Imts :: (Map.Map Type SSort)
         _x1Iself :: Value
         _x1Isexpr :: ISExpr
         _x1Isexprs :: SExpressions
         _x1Ivtype :: Type
         _x2Imts :: (Map.Map Type SSort)
         _x2Iself :: Value
         _x2Isexpr :: ISExpr
         _x2Isexprs :: SExpressions
         _x2Ivtype :: Type
         _self =
             (_x1Iself,_x2Iself)
         _lhsOself =
             _self
         _x1Omts =
             ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: PValue.Tuple.x1.mts"
              {-# LINE 10465 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x1Otn =
             ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: PValue.Tuple.x1.tn"
              {-# LINE 10470 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x1Oval =
             ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: PValue.Tuple.x1.val"
              {-# LINE 10475 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x2Omts =
             ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              _x1Imts
              {-# LINE 10480 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x2Otn =
             ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: PValue.Tuple.x2.tn"
              {-# LINE 10485 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x2Oval =
             ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: PValue.Tuple.x2.val"
              {-# LINE 10490 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         ( _x1Imts,_x1Iself,_x1Isexpr,_x1Isexprs,_x1Ivtype) =
             x1_ _x1Omts _x1Otn _x1Oval
         ( _x2Imts,_x2Iself,_x2Isexpr,_x2Isexprs,_x2Ivtype) =
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
         _x1Omts :: (Map.Map Type SSort)
         _x1Otn :: String
         _x1Oval :: Valuation
         _x1Imts :: (Map.Map Type SSort)
         _x1Iself :: Value
         _x1Isexpr :: ISExpr
         _x1Isexprs :: SExpressions
         _x1Ivtype :: Type
         _self =
             (_x1Iself,x2_)
         _lhsOself =
             _self
         _x1Omts =
             ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: PValueIdx.Tuple.x1.mts"
              {-# LINE 10533 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x1Otn =
             ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: PValueIdx.Tuple.x1.tn"
              {-# LINE 10538 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _x1Oval =
             ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: PValueIdx.Tuple.x1.val"
              {-# LINE 10543 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         ( _x1Imts,_x1Iself,_x1Isexpr,_x1Isexprs,_x1Ivtype) =
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
    (sem_Parameter_Parameter (sem_Id _var) (sem_Type _ty))
-- semantic domain
type T_Parameter = (Map.Map Type SSort) ->
                   String ->
                   ( (Map.Map Type SSort),Parameter,ISExpr,SExpressions)
data Inh_Parameter = Inh_Parameter {mts_Inh_Parameter :: (Map.Map Type SSort),tn_Inh_Parameter :: String}
data Syn_Parameter = Syn_Parameter {mts_Syn_Parameter :: (Map.Map Type SSort),self_Syn_Parameter :: Parameter,sexpr_Syn_Parameter :: ISExpr,sexprs_Syn_Parameter :: SExpressions}
wrap_Parameter :: T_Parameter ->
                  Inh_Parameter ->
                  Syn_Parameter
wrap_Parameter sem (Inh_Parameter _lhsImts _lhsItn) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs) = sem _lhsImts _lhsItn
     in  (Syn_Parameter _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs))
sem_Parameter_Parameter :: T_Id ->
                           T_Type ->
                           T_Parameter
sem_Parameter_Parameter var_ ty_ =
    (\ _lhsImts
       _lhsItn ->
         (let _lhsOsexpr :: ISExpr
              _tyOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _lhsOsexprs :: SExpressions
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOself :: Parameter
              _varIself :: Id
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _lhsOsexpr =
                  ({-# LINE 160 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   ISExpr $ IdentExpr $ SymIdent _sym
                   {-# LINE 10627 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _sym =
                  ({-# LINE 161 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   SimpleSym $ _lhsItn ++ _varIself
                   {-# LINE 10632 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 162 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 10637 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 163 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   Nothing
                   {-# LINE 10642 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 164 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   [ declfun _sym     _tyIsort ] ++ _tyIsexprs
                   {-# LINE 10647 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 165 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 10652 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Parameter _varIself _tyIself
              _lhsOself =
                  _self
              ( _varIself) =
                  var_
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs)))
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
         _hdOmts :: (Map.Map Type SSort)
         _hdOtn :: String
         _hdImts :: (Map.Map Type SSort)
         _hdIself :: Parameter
         _hdIsexpr :: ISExpr
         _hdIsexprs :: SExpressions
         _tlIself :: Parameters
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         _hdOmts =
             ({-# LINE 153 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: Parameters.Cons.hd.mts"
              {-# LINE 10698 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _hdOtn =
             ({-# LINE 156 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: Parameters.Cons.hd.tn"
              {-# LINE 10703 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         ( _hdImts,_hdIself,_hdIsexpr,_hdIsexprs) =
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
         _vOmts :: (Map.Map Type SSort)
         _vOtn :: String
         _vOval :: Valuation
         _vImts :: (Map.Map Type SSort)
         _vIself :: Value
         _vIsexpr :: ISExpr
         _vIsexprs :: SExpressions
         _vIvtype :: Type
         _self =
             ValueRet _vIself
         _lhsOself =
             _self
         _vOmts =
             ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: RetInst.ValueRet.v.mts"
              {-# LINE 10929 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _vOtn =
             ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: RetInst.ValueRet.v.tn"
              {-# LINE 10934 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _vOval =
             ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
              error "missing rule: RetInst.ValueRet.v.val"
              {-# LINE 10939 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         ( _vImts,_vIself,_vIsexpr,_vIsexprs,_vIvtype) =
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
              (Map.Map Type SSort) ->
              ( (Map.Map Type SSort),Type,SExpressions,SSortExpr,SSort)
data Inh_Type = Inh_Type {mn_Inh_Type :: (Maybe SSort),mts_Inh_Type :: (Map.Map Type SSort)}
data Syn_Type = Syn_Type {mts_Syn_Type :: (Map.Map Type SSort),self_Syn_Type :: Type,sexprs_Syn_Type :: SExpressions,sort_Syn_Type :: SSortExpr,sortn_Syn_Type :: SSort}
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
              _tyOmts :: (Map.Map Type SSort)
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsort :: SSortExpr
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _tyOmn :: (Maybe SSort)
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _bsize =
                  ({-# LINE 56 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   getBSize numEl_
                   {-# LINE 11204 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _sortn =
                  ({-# LINE 57 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   fromMaybe ("Array" ++ show numEl_ ++ _tyIsortn) _lhsImn
                   {-# LINE 11209 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 58 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _sortn
                   {-# LINE 11214 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 59 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 11219 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 60 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   Map.insert _self _sortn     _tyImts
                   {-# LINE 11224 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 61 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   SymSort _sortn
                   {-# LINE 11229 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _sortd =
                  ({-# LINE 62 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   ArraySort (BitVector _bsize    ) _tyIsort
                   {-# LINE 11234 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 63 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _tyIsexprs ++ [ SE $ DefSort (SimpleSym _sortn    ) [] $ toSExpr _sortd     ]
                   {-# LINE 11239 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  TyArray numEl_ _tyIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 11248 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _pIself :: TyFloatPoint
              _lhsOsort =
                  ({-# LINE 52 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11267 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 53 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11272 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 54 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11277 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 13 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   []
                   {-# LINE 11282 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _rettyOmn :: (Maybe SSort)
              _rettyOmts :: (Map.Map Type SSort)
              _partyIself :: Types
              _rettyImts :: (Map.Map Type SSort)
              _rettyIself :: Type
              _rettyIsexprs :: SExpressions
              _rettyIsort :: SSortExpr
              _rettyIsortn :: SSort
              _lhsOsort =
                  ({-# LINE 65 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11313 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 66 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11318 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 67 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11323 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 13 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _rettyIsexprs
                   {-# LINE 11328 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  TyFunction _partyIself _rettyIself
              _lhsOself =
                  _self
              _rettyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 11337 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _rettyOmts =
                  ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 11342 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _sortn =
                  ({-# LINE 40 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   if p_ == 1
                   then fromMaybe "Bool"           _lhsImn
                   else fromMaybe ("I" ++ show p_) _lhsImn
                   {-# LINE 11364 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 43 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _sortn
                   {-# LINE 11369 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 44 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   SymSort _sortn
                   {-# LINE 11374 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   Map.insert _self _sortn     _lhsImts
                   {-# LINE 11379 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 46 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   case Map.lookup _self _lhsImts of
                        Nothing -> if p_ == 1
                                   then []
                                   else [ defsorti p_ ]
                        Just _  -> []
                   {-# LINE 11388 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOsort =
                  ({-# LINE 28 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11407 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 29 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11412 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 30 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11417 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 13 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   []
                   {-# LINE 11422 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOsort =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11441 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 33 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11446 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 34 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11451 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 13 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   []
                   {-# LINE 11456 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOsort =
                  ({-# LINE 36 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11475 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 37 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11480 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 38 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11485 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 13 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   []
                   {-# LINE 11490 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
              _tyOmts :: (Map.Map Type SSort)
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _tyOmn :: (Maybe SSort)
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _sortn =
                  ({-# LINE 83 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   fromMaybe _tyIsortn _lhsImn
                   {-# LINE 11517 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 84 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _sortn
                   {-# LINE 11522 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 85 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   SymSort _sortn
                   {-# LINE 11527 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 86 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 11532 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 87 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   Map.insert _self _sortn     _tyImts
                   {-# LINE 11537 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _sortd =
                  ({-# LINE 88 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _tyIsort
                   {-# LINE 11542 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 89 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _tyIsexprs
                   {-# LINE 11547 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  TyPointer _tyIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 11556 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _tysIself :: Types
              _sortn =
                  ({-# LINE 69 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   fromMaybe name_ _lhsImn
                   {-# LINE 11577 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 70 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _sortn
                   {-# LINE 11582 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tysi =
                  ({-# LINE 71 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   encTypes _tysIself _lhsImts
                   {-# LINE 11587 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _sort =
                  ({-# LINE 72 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   case _sortn     of
                        "" -> trdu _tysi
                        n  -> SymSort _sortn
                   {-# LINE 11594 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 75 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _sort
                   {-# LINE 11599 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 76 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   case _sortn     of
                        "" -> fstu _tysi
                        n  -> Map.insert _self n $ fstu _tysi
                   {-# LINE 11606 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 79 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   case _sortn     of
                        "" -> sndu _tysi
                        n  -> sndu _tysi     ++ [ SE $ DefSort (SimpleSym _sortn    ) [] $ toSExpr $ trdu _tysi     ]
                   {-# LINE 11613 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOsort =
                  ({-# LINE 91 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11634 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 92 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11639 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 93 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11644 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 13 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   []
                   {-# LINE 11649 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
              _tyOmts :: (Map.Map Type SSort)
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsort :: SSortExpr
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _tyOmn :: (Maybe SSort)
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _bsize =
                  ({-# LINE 56 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   getBSize numEl_
                   {-# LINE 11677 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _sortn =
                  ({-# LINE 57 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   fromMaybe ("Array" ++ show numEl_ ++ _tyIsortn) _lhsImn
                   {-# LINE 11682 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 58 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _sortn
                   {-# LINE 11687 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmts =
                  ({-# LINE 59 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImts
                   {-# LINE 11692 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 60 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   Map.insert _self _sortn     _tyImts
                   {-# LINE 11697 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsort =
                  ({-# LINE 61 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   SymSort _sortn
                   {-# LINE 11702 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _sortd =
                  ({-# LINE 62 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   ArraySort (BitVector _bsize    ) _tyIsort
                   {-# LINE 11707 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 63 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _tyIsexprs ++ [ SE $ DefSort (SimpleSym _sortn    ) [] $ toSExpr _sortd     ]
                   {-# LINE 11712 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  TyVector numEl_ _tyIself
              _lhsOself =
                  _self
              _tyOmn =
                  ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   _lhsImn
                   {-# LINE 11721 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexprs,_lhsOsort,_lhsOsortn)))
sem_Type_TyVoid :: T_Type
sem_Type_TyVoid =
    (\ _lhsImn
       _lhsImts ->
         (let _lhsOsort :: SSortExpr
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOsort =
                  ({-# LINE 20 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11738 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 21 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11743 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 22 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11748 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 13 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   []
                   {-# LINE 11753 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsortn :: SSort
              _lhsOsexprs :: SExpressions
              _lhsOself :: Type
              _lhsOsort =
                  ({-# LINE 24 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11772 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 25 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11777 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsortn =
                  ({-# LINE 26 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   errormessage
                   {-# LINE 11782 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 13 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
                   []
                   {-# LINE 11787 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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
    (let _lhsOself :: Types
         _hdOmn :: (Maybe SSort)
         _hdOmts :: (Map.Map Type SSort)
         _hdImts :: (Map.Map Type SSort)
         _hdIself :: Type
         _hdIsexprs :: SExpressions
         _hdIsort :: SSortExpr
         _hdIsortn :: SSort
         _tlIself :: Types
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         _hdOmn =
             ({-# LINE 16 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
              error "missing rule: Types.Cons.hd.mn"
              {-# LINE 11830 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
         _hdOmts =
             ({-# LINE 14 "src/Concurrent/Model/ESEncoder/Types.ag" #-}
              error "missing rule: Types.Cons.hd.mts"
              {-# LINE 11835 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
              )
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
type T_Value = (Map.Map Type SSort) ->
               String ->
               Valuation ->
               ( (Map.Map Type SSort),Value,ISExpr,SExpressions,Type)
data Inh_Value = Inh_Value {mts_Inh_Value :: (Map.Map Type SSort),tn_Inh_Value :: String,val_Inh_Value :: Valuation}
data Syn_Value = Syn_Value {mts_Syn_Value :: (Map.Map Type SSort),self_Syn_Value :: Value,sexpr_Syn_Value :: ISExpr,sexprs_Syn_Value :: SExpressions,vtype_Syn_Value :: Type}
wrap_Value :: T_Value ->
              Inh_Value ->
              Syn_Value
wrap_Value sem (Inh_Value _lhsImts _lhsItn _lhsIval) =
    (let ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype) = sem _lhsImts _lhsItn _lhsIval
     in  (Syn_Value _lhsOmts _lhsOself _lhsOsexpr _lhsOsexprs _lhsOvtype))
sem_Value_Constant :: T_Constant ->
                      T_Value
sem_Value_Constant c_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: Type
              _lhsOself :: Value
              _lhsOmts :: (Map.Map Type SSort)
              _cOmts :: (Map.Map Type SSort)
              _cOtn :: String
              _cOval :: Valuation
              _cImts :: (Map.Map Type SSort)
              _cIself :: Constant
              _cIsexpr :: ISExpr
              _cIsexprs :: SExpressions
              _cIvtype :: Type
              _lhsOsexpr =
                  ({-# LINE 65 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _cIsexpr
                   {-# LINE 11893 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 66 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _cIsexprs
                   {-# LINE 11898 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 67 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _cIvtype
                   {-# LINE 11903 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Constant _cIself
              _lhsOself =
                  _self
              _lhsOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _cImts
                   {-# LINE 11912 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _cOmts =
                  ({-# LINE 45 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 11917 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _cOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 11922 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _cOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 11927 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _cImts,_cIself,_cIsexpr,_cIsexprs,_cIvtype) =
                  c_ _cOmts _cOtn _cOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Value_Id :: T_Identifier ->
                T_Type ->
                T_Value
sem_Value_Id v_ ty_ =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _tyOmts :: (Map.Map Type SSort)
              _tyOmn :: (Maybe SSort)
              _vOtn :: String
              _lhsOsexpr :: ISExpr
              _lhsOsexprs :: SExpressions
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOvtype :: Type
              _lhsOself :: Value
              _vIident :: String
              _vIlocals :: ([Id])
              _vIself :: Identifier
              _vIsexpr :: ISExpr
              _vIssymbol :: SSymbol
              _tyImts :: (Map.Map Type SSort)
              _tyIself :: Type
              _tyIsexprs :: SExpressions
              _tyIsort :: SSortExpr
              _tyIsortn :: SSort
              _tyOmts =
                  ({-# LINE 54 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 11960 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tyOmn =
                  ({-# LINE 55 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   Nothing
                   {-# LINE 11965 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _vOtn =
                  ({-# LINE 56 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 11970 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 57 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   case ivalueId _lhsIval _vIident of
                        Nothing -> _vIsexpr
                        Just i  -> ISExpr $ IdentExpr $ SymIdent $ SimpleSym $ _lhsItn ++ i
                   {-# LINE 11977 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 60 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyIsexprs
                   {-# LINE 11982 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 61 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyImts
                   {-# LINE 11987 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 62 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tyIself
                   {-# LINE 11992 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  Id _vIself _tyIself
              _lhsOself =
                  _self
              ( _vIident,_vIlocals,_vIself,_vIsexpr,_vIssymbol) =
                  v_ _vOtn
              ( _tyImts,_tyIself,_tyIsexprs,_tyIsort,_tyIsortn) =
                  ty_ _tyOmn _tyOmts
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
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
type T_Values = (Map.Map Type SSort) ->
                String ->
                Valuation ->
                ( (Map.Map Type SSort),Values,([ISExpr]),SExpressions,([Type]))
data Inh_Values = Inh_Values {mts_Inh_Values :: (Map.Map Type SSort),tn_Inh_Values :: String,val_Inh_Values :: Valuation}
data Syn_Values = Syn_Values {mts_Syn_Values :: (Map.Map Type SSort),self_Syn_Values :: Values,sexpr_Syn_Values :: ([ISExpr]),sexprs_Syn_Values :: SExpressions,vtype_Syn_Values :: ([Type])}
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
         (let _hdOmts :: (Map.Map Type SSort)
              _tlOmts :: (Map.Map Type SSort)
              _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr :: ([ISExpr])
              _lhsOvtype :: ([Type])
              _lhsOsexprs :: SExpressions
              _lhsOself :: Values
              _hdOtn :: String
              _hdOval :: Valuation
              _tlOtn :: String
              _tlOval :: Valuation
              _hdImts :: (Map.Map Type SSort)
              _hdIself :: Value
              _hdIsexpr :: ISExpr
              _hdIsexprs :: SExpressions
              _hdIvtype :: Type
              _tlImts :: (Map.Map Type SSort)
              _tlIself :: Values
              _tlIsexpr :: ([ISExpr])
              _tlIsexprs :: SExpressions
              _tlIvtype :: ([Type])
              _hdOmts =
                  ({-# LINE 38 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 12093 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOmts =
                  ({-# LINE 39 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _hdImts
                   {-# LINE 12098 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOmts =
                  ({-# LINE 40 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _tlImts
                   {-# LINE 12103 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 41 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _hdIsexpr:(_tlIsexpr)
                   {-# LINE 12108 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 42 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _hdIvtype:(_tlIvtype)
                   {-# LINE 12113 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 30 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _hdIsexprs ++ _tlIsexprs
                   {-# LINE 12118 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOtn =
                  ({-# LINE 48 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12127 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _hdOval =
                  ({-# LINE 49 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12132 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOtn =
                  ({-# LINE 29 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsItn
                   {-# LINE 12137 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _tlOval =
                  ({-# LINE 28 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsIval
                   {-# LINE 12142 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              ( _hdImts,_hdIself,_hdIsexpr,_hdIsexprs,_hdIvtype) =
                  hd_ _hdOmts _hdOtn _hdOval
              ( _tlImts,_tlIself,_tlIsexpr,_tlIsexprs,_tlIvtype) =
                  tl_ _tlOmts _tlOtn _tlOval
          in  ( _lhsOmts,_lhsOself,_lhsOsexpr,_lhsOsexprs,_lhsOvtype)))
sem_Values_Nil :: T_Values
sem_Values_Nil =
    (\ _lhsImts
       _lhsItn
       _lhsIval ->
         (let _lhsOmts :: (Map.Map Type SSort)
              _lhsOsexpr :: ([ISExpr])
              _lhsOsexprs :: SExpressions
              _lhsOvtype :: ([Type])
              _lhsOself :: Values
              _lhsOmts =
                  ({-# LINE 36 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   _lhsImts
                   {-# LINE 12162 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexpr =
                  ({-# LINE 31 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 12167 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOsexprs =
                  ({-# LINE 30 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 12172 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
                   )
              _lhsOvtype =
                  ({-# LINE 32 "src/Concurrent/Model/ESEncoder/Value.ag" #-}
                   []
                   {-# LINE 12177 "src/Concurrent/Model/ESEncoder/Model.hs" #-}
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