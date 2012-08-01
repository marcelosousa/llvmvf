

-- UUAGC 0.9.40.3 (src/Concurrent/Model/Analysis/Flow.ag)
module Concurrent.Model.Analysis.Flow where

{-# LINE 12 "src/Concurrent/Model/Analysis/Flow.ag" #-}

import qualified Data.Map as Map
import qualified Data.IntMap as IM
import Data.Maybe 

import Debug.Trace

import Language.LLVMIR
{-# LINE 16 "src/Concurrent/Model/Analysis/Flow.hs" #-}

{-# LINE 11 "src/Language/LLVMIR/Grammar/Base.ag" #-}

import Prelude              hiding (sequence)
import Data.Char            (chr)
import qualified Data.Map as Data.Map
import qualified Data.Map as Map
import Data.Map
{-# LINE 25 "src/Concurrent/Model/Analysis/Flow.hs" #-}
{-# LINE 1 "src/Concurrent/Model/Analysis/Flow.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.Flow
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 32 "src/Concurrent/Model/Analysis/Flow.hs" #-}

{-# LINE 64 "src/Concurrent/Model/Analysis/Flow.ag" #-}

getId :: Value -> String
getId (Id i t) = case i of 
                   Global s -> s
                   Local  s -> s 
getId _ = error "'getId'"

getEntry :: String -> Map.Map String Int -> Int
getEntry s m = fromMaybe (error $ "getEntry " ++ s ++ show m) $ Map.lookup s m
{-# LINE 44 "src/Concurrent/Model/Analysis/Flow.hs" #-}

{-# LINE 122 "src/Concurrent/Model/Analysis/Flow.ag" #-}

type Flow = [(Int,Int)]

flowfs :: Functions -> Flow
flowfs fs = let i = bbentrypc_Syn_Functions$ wrap_Functions (sem_Functions fs) $ Inh_Functions { bbentrypci_Inh_Functions = Map.empty }
            in flow_Syn_Functions $ wrap_Functions (sem_Functions fs) $ Inh_Functions { bbentrypci_Inh_Functions = i }
{-# LINE 53 "src/Concurrent/Model/Analysis/Flow.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Grammar/Base.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Base
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 61 "src/Concurrent/Model/Analysis/Flow.hs" #-}
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
         _argIself :: Value
         _self =
             Argument _argIself
         _lhsOself =
             _self
         ( _argIself) =
             arg_
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
type T_BasicBlock = (Map.Map String Int) ->
                    Int ->
                    ( (Map.Map String Int),Int,Int,([Int]),Flow,BasicBlock)
data Inh_BasicBlock = Inh_BasicBlock {bbentrypci_Inh_BasicBlock :: (Map.Map String Int),cpc_Inh_BasicBlock :: Int}
data Syn_BasicBlock = Syn_BasicBlock {bbentrypc_Syn_BasicBlock :: (Map.Map String Int),cpc_Syn_BasicBlock :: Int,entrypc_Syn_BasicBlock :: Int,exitpcs_Syn_BasicBlock :: ([Int]),flow_Syn_BasicBlock :: Flow,self_Syn_BasicBlock :: BasicBlock}
wrap_BasicBlock :: T_BasicBlock ->
                   Inh_BasicBlock ->
                   Syn_BasicBlock
wrap_BasicBlock sem (Inh_BasicBlock _lhsIbbentrypci _lhsIcpc) =
    (let ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself) = sem _lhsIbbentrypci _lhsIcpc
     in  (Syn_BasicBlock _lhsObbentrypc _lhsOcpc _lhsOentrypc _lhsOexitpcs _lhsOflow _lhsOself))
sem_BasicBlock_BasicBlock :: T_Label ->
                             T_Instructions ->
                             T_BasicBlock
sem_BasicBlock_BasicBlock label_ instrs_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsObbentrypc :: (Map.Map String Int)
              _lhsOflow :: Flow
              _lhsOentrypc :: Int
              _lhsOexitpcs :: ([Int])
              _lhsOself :: BasicBlock
              _lhsOcpc :: Int
              _instrsObbentrypci :: (Map.Map String Int)
              _instrsOcpc :: Int
              _labelIself :: Label
              _instrsIbbentrypc :: (Map.Map String Int)
              _instrsIcpc :: Int
              _instrsIentrypc :: Int
              _instrsIexitpcs :: ([Int])
              _instrsIflow :: Flow
              _instrsIself :: Instructions
              _lhsObbentrypc =
                  ({-# LINE 105 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _bbentrypc_augmented_syn [_bbentrypc_augmented_f1]
                   {-# LINE 603 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _bbentrypc_augmented_f1 =
                  ({-# LINE 105 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.insert _labelIself _instrsIentrypc
                   {-# LINE 608 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 48 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   tail _instrsIflow
                   {-# LINE 613 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 106 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _instrsIentrypc
                   {-# LINE 618 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 107 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _instrsIexitpcs
                   {-# LINE 623 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _bbentrypc_augmented_syn =
                  ({-# LINE 105 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _instrsIbbentrypc
                   {-# LINE 628 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  BasicBlock _labelIself _instrsIself
              _lhsOself =
                  _self
              _lhsOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _instrsIcpc
                   {-# LINE 637 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _instrsObbentrypci =
                  ({-# LINE 32 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIbbentrypci
                   {-# LINE 642 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _instrsOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIcpc
                   {-# LINE 647 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              ( _labelIself) =
                  label_
              ( _instrsIbbentrypc,_instrsIcpc,_instrsIentrypc,_instrsIexitpcs,_instrsIflow,_instrsIself) =
                  instrs_ _instrsObbentrypci _instrsOcpc
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
-- BasicBlocks -------------------------------------------------
-- cata
sem_BasicBlocks :: BasicBlocks ->
                   T_BasicBlocks
sem_BasicBlocks list =
    (Prelude.foldr sem_BasicBlocks_Cons sem_BasicBlocks_Nil (Prelude.map sem_BasicBlock list))
-- semantic domain
type T_BasicBlocks = (Map.Map String Int) ->
                     Int ->
                     ( (Map.Map String Int),Int,Int,([Int]),Flow,BasicBlocks)
data Inh_BasicBlocks = Inh_BasicBlocks {bbentrypci_Inh_BasicBlocks :: (Map.Map String Int),cpc_Inh_BasicBlocks :: Int}
data Syn_BasicBlocks = Syn_BasicBlocks {bbentrypc_Syn_BasicBlocks :: (Map.Map String Int),cpc_Syn_BasicBlocks :: Int,entrypc_Syn_BasicBlocks :: Int,exitpcs_Syn_BasicBlocks :: ([Int]),flow_Syn_BasicBlocks :: Flow,self_Syn_BasicBlocks :: BasicBlocks}
wrap_BasicBlocks :: T_BasicBlocks ->
                    Inh_BasicBlocks ->
                    Syn_BasicBlocks
wrap_BasicBlocks sem (Inh_BasicBlocks _lhsIbbentrypci _lhsIcpc) =
    (let ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself) = sem _lhsIbbentrypci _lhsIcpc
     in  (Syn_BasicBlocks _lhsObbentrypc _lhsOcpc _lhsOentrypc _lhsOexitpcs _lhsOflow _lhsOself))
sem_BasicBlocks_Cons :: T_BasicBlock ->
                        T_BasicBlocks ->
                        T_BasicBlocks
sem_BasicBlocks_Cons hd_ tl_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOflow :: Flow
              _lhsOself :: BasicBlocks
              _lhsOcpc :: Int
              _hdObbentrypci :: (Map.Map String Int)
              _hdOcpc :: Int
              _tlObbentrypci :: (Map.Map String Int)
              _tlOcpc :: Int
              _hdIbbentrypc :: (Map.Map String Int)
              _hdIcpc :: Int
              _hdIentrypc :: Int
              _hdIexitpcs :: ([Int])
              _hdIflow :: Flow
              _hdIself :: BasicBlock
              _tlIbbentrypc :: (Map.Map String Int)
              _tlIcpc :: Int
              _tlIentrypc :: Int
              _tlIexitpcs :: ([Int])
              _tlIflow :: Flow
              _tlIself :: BasicBlocks
              _lhsOentrypc =
                  ({-# LINE 101 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _hdIentrypc
                   {-# LINE 703 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _hdIbbentrypc `Map.union` _tlIbbentrypc
                   {-# LINE 708 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _hdIexitpcs ++ _tlIexitpcs
                   {-# LINE 713 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 31 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _hdIflow ++ _tlIflow
                   {-# LINE 718 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _tlIcpc
                   {-# LINE 727 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _hdObbentrypci =
                  ({-# LINE 32 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIbbentrypci
                   {-# LINE 732 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _hdOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIcpc
                   {-# LINE 737 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _tlObbentrypci =
                  ({-# LINE 32 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIbbentrypci
                   {-# LINE 742 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _tlOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _hdIcpc
                   {-# LINE 747 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              ( _hdIbbentrypc,_hdIcpc,_hdIentrypc,_hdIexitpcs,_hdIflow,_hdIself) =
                  hd_ _hdObbentrypci _hdOcpc
              ( _tlIbbentrypc,_tlIcpc,_tlIentrypc,_tlIexitpcs,_tlIflow,_tlIself) =
                  tl_ _tlObbentrypci _tlOcpc
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_BasicBlocks_Nil :: T_BasicBlocks
sem_BasicBlocks_Nil =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOflow :: Flow
              _lhsOself :: BasicBlocks
              _lhsOcpc :: Int
              _lhsOentrypc =
                  ({-# LINE 99 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   error "'entrypc' of an empty bb list"
                   {-# LINE 767 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 772 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 777 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 31 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 782 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIcpc
                   {-# LINE 791 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
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
    (sem_CompareConstantExpr_FCmpExpr (sem_RealPredicate _cond) _ty (sem_Value _op1) (sem_Value _op2))
sem_CompareConstantExpr (ICmpExpr _cond _ty _op1 _op2) =
    (sem_CompareConstantExpr_ICmpExpr (sem_IntPredicate _cond) _ty (sem_Value _op1) (sem_Value _op2))
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
                                    Type ->
                                    T_Value ->
                                    T_Value ->
                                    T_CompareConstantExpr
sem_CompareConstantExpr_FCmpExpr cond_ ty_ op1_ op2_ =
    (let _lhsOself :: CompareConstantExpr
         _condIself :: RealPredicate
         _op1Iself :: Value
         _op2Iself :: Value
         _self =
             FCmpExpr _condIself ty_ _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _condIself) =
             cond_
         ( _op1Iself) =
             op1_
         ( _op2Iself) =
             op2_
     in  ( _lhsOself))
sem_CompareConstantExpr_ICmpExpr :: T_IntPredicate ->
                                    Type ->
                                    T_Value ->
                                    T_Value ->
                                    T_CompareConstantExpr
sem_CompareConstantExpr_ICmpExpr cond_ ty_ op1_ op2_ =
    (let _lhsOself :: CompareConstantExpr
         _condIself :: IntPredicate
         _op1Iself :: Value
         _op2Iself :: Value
         _self =
             ICmpExpr _condIself ty_ _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _condIself) =
             cond_
         ( _op1Iself) =
             op1_
         ( _op2Iself) =
             op2_
     in  ( _lhsOself))
-- Constant ----------------------------------------------------
-- cata
sem_Constant :: Constant ->
                T_Constant
sem_Constant (BlockAddr) =
    (sem_Constant_BlockAddr)
sem_Constant (ConstantAggregateZero _ty) =
    (sem_Constant_ConstantAggregateZero _ty)
sem_Constant (ConstantArray _ty _vals) =
    (sem_Constant_ConstantArray _ty (sem_Values _vals))
sem_Constant (ConstantDataSequential _cds) =
    (sem_Constant_ConstantDataSequential (sem_ConstantDataSequential _cds))
sem_Constant (ConstantExpr _expr) =
    (sem_Constant_ConstantExpr (sem_ConstantExpr _expr))
sem_Constant (ConstantFP _fp) =
    (sem_Constant_ConstantFP (sem_ConstantFP _fp))
sem_Constant (ConstantInt _iv _ty) =
    (sem_Constant_ConstantInt _iv _ty)
sem_Constant (ConstantPointerNull _ty) =
    (sem_Constant_ConstantPointerNull _ty)
sem_Constant (ConstantStruct _ty _vals) =
    (sem_Constant_ConstantStruct _ty (sem_Values _vals))
sem_Constant (ConstantVector) =
    (sem_Constant_ConstantVector)
sem_Constant (GlobalValue _gv) =
    (sem_Constant_GlobalValue (sem_GlobalValue _gv))
sem_Constant (UndefValue) =
    (sem_Constant_UndefValue)
-- semantic domain
type T_Constant = ( Constant)
data Inh_Constant = Inh_Constant {}
data Syn_Constant = Syn_Constant {self_Syn_Constant :: Constant}
wrap_Constant :: T_Constant ->
                 Inh_Constant ->
                 Syn_Constant
wrap_Constant sem (Inh_Constant) =
    (let ( _lhsOself) = sem
     in  (Syn_Constant _lhsOself))
sem_Constant_BlockAddr :: T_Constant
sem_Constant_BlockAddr =
    (let _lhsOself :: Constant
         _self =
             BlockAddr
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Constant_ConstantAggregateZero :: Type ->
                                      T_Constant
sem_Constant_ConstantAggregateZero ty_ =
    (let _lhsOself :: Constant
         _self =
             ConstantAggregateZero ty_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Constant_ConstantArray :: Type ->
                              T_Values ->
                              T_Constant
sem_Constant_ConstantArray ty_ vals_ =
    (let _lhsOself :: Constant
         _valsIself :: Values
         _self =
             ConstantArray ty_ _valsIself
         _lhsOself =
             _self
         ( _valsIself) =
             vals_
     in  ( _lhsOself))
sem_Constant_ConstantDataSequential :: T_ConstantDataSequential ->
                                       T_Constant
sem_Constant_ConstantDataSequential cds_ =
    (let _lhsOself :: Constant
         _cdsIself :: ConstantDataSequential
         _self =
             ConstantDataSequential _cdsIself
         _lhsOself =
             _self
         ( _cdsIself) =
             cds_
     in  ( _lhsOself))
sem_Constant_ConstantExpr :: T_ConstantExpr ->
                             T_Constant
sem_Constant_ConstantExpr expr_ =
    (let _lhsOself :: Constant
         _exprIself :: ConstantExpr
         _self =
             ConstantExpr _exprIself
         _lhsOself =
             _self
         ( _exprIself) =
             expr_
     in  ( _lhsOself))
sem_Constant_ConstantFP :: T_ConstantFP ->
                           T_Constant
sem_Constant_ConstantFP fp_ =
    (let _lhsOself :: Constant
         _fpIself :: ConstantFP
         _self =
             ConstantFP _fpIself
         _lhsOself =
             _self
         ( _fpIself) =
             fp_
     in  ( _lhsOself))
sem_Constant_ConstantInt :: Int ->
                            Type ->
                            T_Constant
sem_Constant_ConstantInt iv_ ty_ =
    (let _lhsOself :: Constant
         _self =
             ConstantInt iv_ ty_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Constant_ConstantPointerNull :: Type ->
                                    T_Constant
sem_Constant_ConstantPointerNull ty_ =
    (let _lhsOself :: Constant
         _self =
             ConstantPointerNull ty_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Constant_ConstantStruct :: Type ->
                               T_Values ->
                               T_Constant
sem_Constant_ConstantStruct ty_ vals_ =
    (let _lhsOself :: Constant
         _valsIself :: Values
         _self =
             ConstantStruct ty_ _valsIself
         _lhsOself =
             _self
         ( _valsIself) =
             vals_
     in  ( _lhsOself))
sem_Constant_ConstantVector :: T_Constant
sem_Constant_ConstantVector =
    (let _lhsOself :: Constant
         _self =
             ConstantVector
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Constant_GlobalValue :: T_GlobalValue ->
                            T_Constant
sem_Constant_GlobalValue gv_ =
    (let _lhsOself :: Constant
         _gvIself :: GlobalValue
         _self =
             GlobalValue _gvIself
         _lhsOself =
             _self
         ( _gvIself) =
             gv_
     in  ( _lhsOself))
sem_Constant_UndefValue :: T_Constant
sem_Constant_UndefValue =
    (let _lhsOself :: Constant
         _self =
             UndefValue
         _lhsOself =
             _self
     in  ( _lhsOself))
-- ConstantDataSequential --------------------------------------
-- cata
sem_ConstantDataSequential :: ConstantDataSequential ->
                              T_ConstantDataSequential
sem_ConstantDataSequential (ConstantDataArray _ty _val) =
    (sem_ConstantDataSequential_ConstantDataArray _ty _val)
sem_ConstantDataSequential (ConstantDataVector _ty _val) =
    (sem_ConstantDataSequential_ConstantDataVector _ty _val)
-- semantic domain
type T_ConstantDataSequential = ( ConstantDataSequential)
data Inh_ConstantDataSequential = Inh_ConstantDataSequential {}
data Syn_ConstantDataSequential = Syn_ConstantDataSequential {self_Syn_ConstantDataSequential :: ConstantDataSequential}
wrap_ConstantDataSequential :: T_ConstantDataSequential ->
                               Inh_ConstantDataSequential ->
                               Syn_ConstantDataSequential
wrap_ConstantDataSequential sem (Inh_ConstantDataSequential) =
    (let ( _lhsOself) = sem
     in  (Syn_ConstantDataSequential _lhsOself))
sem_ConstantDataSequential_ConstantDataArray :: Type ->
                                                String ->
                                                T_ConstantDataSequential
sem_ConstantDataSequential_ConstantDataArray ty_ val_ =
    (let _lhsOself :: ConstantDataSequential
         _self =
             ConstantDataArray ty_ val_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ConstantDataSequential_ConstantDataVector :: Type ->
                                                 String ->
                                                 T_ConstantDataSequential
sem_ConstantDataSequential_ConstantDataVector ty_ val_ =
    (let _lhsOself :: ConstantDataSequential
         _self =
             ConstantDataVector ty_ val_
         _lhsOself =
             _self
     in  ( _lhsOself))
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
    (sem_ConstantExpr_UnaryConstantExpr _name _op (sem_Value _val) _ty)
-- semantic domain
type T_ConstantExpr = ( ConstantExpr)
data Inh_ConstantExpr = Inh_ConstantExpr {}
data Syn_ConstantExpr = Syn_ConstantExpr {self_Syn_ConstantExpr :: ConstantExpr}
wrap_ConstantExpr :: T_ConstantExpr ->
                     Inh_ConstantExpr ->
                     Syn_ConstantExpr
wrap_ConstantExpr sem (Inh_ConstantExpr) =
    (let ( _lhsOself) = sem
     in  (Syn_ConstantExpr _lhsOself))
sem_ConstantExpr_BinaryConstantExpr :: T_ConstantExpr
sem_ConstantExpr_BinaryConstantExpr =
    (let _lhsOself :: ConstantExpr
         _self =
             BinaryConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ConstantExpr_CompareConstantExpr :: T_CompareConstantExpr ->
                                        T_ConstantExpr
sem_ConstantExpr_CompareConstantExpr cmpExpr_ =
    (let _lhsOself :: ConstantExpr
         _cmpExprIself :: CompareConstantExpr
         _self =
             CompareConstantExpr _cmpExprIself
         _lhsOself =
             _self
         ( _cmpExprIself) =
             cmpExpr_
     in  ( _lhsOself))
sem_ConstantExpr_ExtractElementConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ExtractElementConstantExpr =
    (let _lhsOself :: ConstantExpr
         _self =
             ExtractElementConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ConstantExpr_ExtractValueConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ExtractValueConstantExpr =
    (let _lhsOself :: ConstantExpr
         _self =
             ExtractValueConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ConstantExpr_GetElementPtrConstantExpr :: T_Value ->
                                              T_Values ->
                                              T_ConstantExpr
sem_ConstantExpr_GetElementPtrConstantExpr struct_ idxs_ =
    (let _lhsOself :: ConstantExpr
         _structIself :: Value
         _idxsIself :: Values
         _self =
             GetElementPtrConstantExpr _structIself _idxsIself
         _lhsOself =
             _self
         ( _structIself) =
             struct_
         ( _idxsIself) =
             idxs_
     in  ( _lhsOself))
sem_ConstantExpr_InsertElementConstantExpr :: T_ConstantExpr
sem_ConstantExpr_InsertElementConstantExpr =
    (let _lhsOself :: ConstantExpr
         _self =
             InsertElementConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ConstantExpr_InsertValueConstantExpr :: T_ConstantExpr
sem_ConstantExpr_InsertValueConstantExpr =
    (let _lhsOself :: ConstantExpr
         _self =
             InsertValueConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ConstantExpr_SelectConstantExpr :: T_ConstantExpr
sem_ConstantExpr_SelectConstantExpr =
    (let _lhsOself :: ConstantExpr
         _self =
             SelectConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ConstantExpr_ShuffleVectorConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ShuffleVectorConstantExpr =
    (let _lhsOself :: ConstantExpr
         _self =
             ShuffleVectorConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ConstantExpr_UnaryConstantExpr :: String ->
                                      Int ->
                                      T_Value ->
                                      Type ->
                                      T_ConstantExpr
sem_ConstantExpr_UnaryConstantExpr name_ op_ val_ ty_ =
    (let _lhsOself :: ConstantExpr
         _valIself :: Value
         _self =
             UnaryConstantExpr name_ op_ _valIself ty_
         _lhsOself =
             _self
         ( _valIself) =
             val_
     in  ( _lhsOself))
-- ConstantFP --------------------------------------------------
-- cata
sem_ConstantFP :: ConstantFP ->
                  T_ConstantFP
sem_ConstantFP (ConstantFPDouble _dbv _ty) =
    (sem_ConstantFP_ConstantFPDouble _dbv _ty)
sem_ConstantFP (ConstantFPFloat _fpv _ty) =
    (sem_ConstantFP_ConstantFPFloat _fpv _ty)
-- semantic domain
type T_ConstantFP = ( ConstantFP)
data Inh_ConstantFP = Inh_ConstantFP {}
data Syn_ConstantFP = Syn_ConstantFP {self_Syn_ConstantFP :: ConstantFP}
wrap_ConstantFP :: T_ConstantFP ->
                   Inh_ConstantFP ->
                   Syn_ConstantFP
wrap_ConstantFP sem (Inh_ConstantFP) =
    (let ( _lhsOself) = sem
     in  (Syn_ConstantFP _lhsOself))
sem_ConstantFP_ConstantFPDouble :: Double ->
                                   Type ->
                                   T_ConstantFP
sem_ConstantFP_ConstantFPDouble dbv_ ty_ =
    (let _lhsOself :: ConstantFP
         _self =
             ConstantFPDouble dbv_ ty_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ConstantFP_ConstantFPFloat :: Float ->
                                  Type ->
                                  T_ConstantFP
sem_ConstantFP_ConstantFPFloat fpv_ ty_ =
    (let _lhsOself :: ConstantFP
         _self =
             ConstantFPFloat fpv_ ty_
         _lhsOself =
             _self
     in  ( _lhsOself))
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
    (sem_Function_FunctionDecl (sem_Id _name) (sem_Linkage _linkage) _retty (sem_Parameters _params))
sem_Function (FunctionDef _name _linkage _retty _params _body) =
    (sem_Function_FunctionDef (sem_Id _name) (sem_Linkage _linkage) _retty (sem_Parameters _params) (sem_BasicBlocks _body))
-- semantic domain
type T_Function = (Map.Map String Int) ->
                  Int ->
                  ( (Map.Map String Int),Int,Int,([Int]),Flow,Function)
data Inh_Function = Inh_Function {bbentrypci_Inh_Function :: (Map.Map String Int),cpc_Inh_Function :: Int}
data Syn_Function = Syn_Function {bbentrypc_Syn_Function :: (Map.Map String Int),cpc_Syn_Function :: Int,entrypc_Syn_Function :: Int,exitpcs_Syn_Function :: ([Int]),flow_Syn_Function :: Flow,self_Syn_Function :: Function}
wrap_Function :: T_Function ->
                 Inh_Function ->
                 Syn_Function
wrap_Function sem (Inh_Function _lhsIbbentrypci _lhsIcpc) =
    (let ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself) = sem _lhsIbbentrypci _lhsIcpc
     in  (Syn_Function _lhsObbentrypc _lhsOcpc _lhsOentrypc _lhsOexitpcs _lhsOflow _lhsOself))
sem_Function_FunctionDecl :: T_Id ->
                             T_Linkage ->
                             Type ->
                             T_Parameters ->
                             T_Function
sem_Function_FunctionDecl name_ linkage_ retty_ params_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOentrypc :: Int
              _lhsOexitpcs :: ([Int])
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOflow :: Flow
              _lhsOself :: Function
              _lhsOcpc :: Int
              _nameIself :: Id
              _linkageIself :: Linkage
              _paramsIself :: Parameters
              _lhsOentrypc =
                  ({-# LINE 90 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   error "'entrypc' of a FunctionDecl"
                   {-# LINE 1790 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 91 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   error "'exitpcs' of a FunctionDecl"
                   {-# LINE 1795 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 1800 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 31 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 1805 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  FunctionDecl _nameIself _linkageIself retty_ _paramsIself
              _lhsOself =
                  _self
              _lhsOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIcpc
                   {-# LINE 1814 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              ( _nameIself) =
                  name_
              ( _linkageIself) =
                  linkage_
              ( _paramsIself) =
                  params_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Function_FunctionDef :: T_Id ->
                            T_Linkage ->
                            Type ->
                            T_Parameters ->
                            T_BasicBlocks ->
                            T_Function
sem_Function_FunctionDef name_ linkage_ retty_ params_ body_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOentrypc :: Int
              _lhsOexitpcs :: ([Int])
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOself :: Function
              _lhsOcpc :: Int
              _bodyObbentrypci :: (Map.Map String Int)
              _bodyOcpc :: Int
              _nameIself :: Id
              _linkageIself :: Linkage
              _paramsIself :: Parameters
              _bodyIbbentrypc :: (Map.Map String Int)
              _bodyIcpc :: Int
              _bodyIentrypc :: Int
              _bodyIexitpcs :: ([Int])
              _bodyIflow :: Flow
              _bodyIself :: BasicBlocks
              _lhsOflow =
                  ({-# LINE 44 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _bodyIflow
                   {-# LINE 1852 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 93 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _bodyIentrypc
                   {-# LINE 1857 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 94 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _bodyIexitpcs
                   {-# LINE 1862 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 95 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _bodyIbbentrypc
                   {-# LINE 1867 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  FunctionDef _nameIself _linkageIself retty_ _paramsIself _bodyIself
              _lhsOself =
                  _self
              _lhsOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _bodyIcpc
                   {-# LINE 1876 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _bodyObbentrypci =
                  ({-# LINE 32 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIbbentrypci
                   {-# LINE 1881 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _bodyOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIcpc
                   {-# LINE 1886 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              ( _nameIself) =
                  name_
              ( _linkageIself) =
                  linkage_
              ( _paramsIself) =
                  params_
              ( _bodyIbbentrypc,_bodyIcpc,_bodyIentrypc,_bodyIexitpcs,_bodyIflow,_bodyIself) =
                  body_ _bodyObbentrypci _bodyOcpc
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
-- Functions ---------------------------------------------------
-- cata
sem_Functions :: Functions ->
                 T_Functions
sem_Functions m =
    (Data.Map.foldrWithKey sem_Functions_Entry sem_Functions_Nil (Data.Map.map sem_Function m))
-- semantic domain
type T_Functions = (Map.Map String (Map.Map String Int)) ->
                   ( (Map.Map String (Map.Map String Int)),Flow,Functions)
data Inh_Functions = Inh_Functions {bbentrypci_Inh_Functions :: (Map.Map String (Map.Map String Int))}
data Syn_Functions = Syn_Functions {bbentrypc_Syn_Functions :: (Map.Map String (Map.Map String Int)),flow_Syn_Functions :: Flow,self_Syn_Functions :: Functions}
wrap_Functions :: T_Functions ->
                  Inh_Functions ->
                  Syn_Functions
wrap_Functions sem (Inh_Functions _lhsIbbentrypci) =
    (let ( _lhsObbentrypc,_lhsOflow,_lhsOself) = sem _lhsIbbentrypci
     in  (Syn_Functions _lhsObbentrypc _lhsOflow _lhsOself))
sem_Functions_Entry :: String ->
                       T_Function ->
                       T_Functions ->
                       T_Functions
sem_Functions_Entry key_ val_ tl_ =
    (\ _lhsIbbentrypci ->
         (let _lhsOflow :: Flow
              _valOcpc :: Int
              _valObbentrypci :: (Map.Map String Int)
              _lhsObbentrypc :: (Map.Map String (Map.Map String Int))
              _lhsOself :: Functions
              _tlObbentrypci :: (Map.Map String (Map.Map String Int))
              _valIbbentrypc :: (Map.Map String Int)
              _valIcpc :: Int
              _valIentrypc :: Int
              _valIexitpcs :: ([Int])
              _valIflow :: Flow
              _valIself :: Function
              _tlIbbentrypc :: (Map.Map String (Map.Map String Int))
              _tlIflow :: Flow
              _tlIself :: Functions
              _lhsOflow =
                  ({-# LINE 36 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _cflow     ++ _valIflow ++ _tlIflow
                   {-# LINE 1938 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _valOcpc =
                  ({-# LINE 37 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _valIentrypc
                   {-# LINE 1943 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _valObbentrypci =
                  ({-# LINE 38 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   fromMaybe (error "lookup") (Map.lookup key_ _lhsIbbentrypci)
                   {-# LINE 1948 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _cflow =
                  ({-# LINE 39 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   if key_ /= "main"
                   then [(-1,_valIentrypc)]
                   else []
                   {-# LINE 1955 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 81 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.insert key_ _valIbbentrypc _tlIbbentrypc
                   {-# LINE 1960 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Data.Map.insert key_ _valIself _tlIself
              _lhsOself =
                  _self
              _tlObbentrypci =
                  ({-# LINE 27 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIbbentrypci
                   {-# LINE 1969 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              ( _valIbbentrypc,_valIcpc,_valIentrypc,_valIexitpcs,_valIflow,_valIself) =
                  val_ _valObbentrypci _valOcpc
              ( _tlIbbentrypc,_tlIflow,_tlIself) =
                  tl_ _tlObbentrypci
          in  ( _lhsObbentrypc,_lhsOflow,_lhsOself)))
sem_Functions_Nil :: T_Functions
sem_Functions_Nil =
    (\ _lhsIbbentrypci ->
         (let _lhsObbentrypc :: (Map.Map String (Map.Map String Int))
              _lhsOflow :: Flow
              _lhsOself :: Functions
              _lhsObbentrypc =
                  ({-# LINE 77 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 1985 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 28 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 1990 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Data.Map.empty
              _lhsOself =
                  _self
          in  ( _lhsObbentrypc,_lhsOflow,_lhsOself)))
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
    (sem_Global_GlobalVar (sem_Id _name) (sem_Linkage _linkage) _isConst _isUaddr _ty (sem_MValue _ival) (sem_Align _align))
-- semantic domain
type T_Global = ( Global)
data Inh_Global = Inh_Global {}
data Syn_Global = Syn_Global {self_Syn_Global :: Global}
wrap_Global :: T_Global ->
               Inh_Global ->
               Syn_Global
wrap_Global sem (Inh_Global) =
    (let ( _lhsOself) = sem
     in  (Syn_Global _lhsOself))
sem_Global_GlobalVar :: T_Id ->
                        T_Linkage ->
                        Bool ->
                        Bool ->
                        Type ->
                        T_MValue ->
                        T_Align ->
                        T_Global
sem_Global_GlobalVar name_ linkage_ isConst_ isUaddr_ ty_ ival_ align_ =
    (let _lhsOself :: Global
         _nameIself :: Id
         _linkageIself :: Linkage
         _ivalIself :: MValue
         _alignIself :: Align
         _self =
             GlobalVar _nameIself _linkageIself isConst_ isUaddr_ ty_ _ivalIself _alignIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
         ( _linkageIself) =
             linkage_
         ( _ivalIself) =
             ival_
         ( _alignIself) =
             align_
     in  ( _lhsOself))
-- GlobalValue -------------------------------------------------
-- cata
sem_GlobalValue :: GlobalValue ->
                   T_GlobalValue
sem_GlobalValue (FunctionValue _n _ty) =
    (sem_GlobalValue_FunctionValue (sem_Identifier _n) _ty)
sem_GlobalValue (GlobalAlias _n _ty) =
    (sem_GlobalValue_GlobalAlias (sem_Identifier _n) _ty)
sem_GlobalValue (GlobalVariable _n _ty) =
    (sem_GlobalValue_GlobalVariable (sem_Identifier _n) _ty)
-- semantic domain
type T_GlobalValue = ( GlobalValue)
data Inh_GlobalValue = Inh_GlobalValue {}
data Syn_GlobalValue = Syn_GlobalValue {self_Syn_GlobalValue :: GlobalValue}
wrap_GlobalValue :: T_GlobalValue ->
                    Inh_GlobalValue ->
                    Syn_GlobalValue
wrap_GlobalValue sem (Inh_GlobalValue) =
    (let ( _lhsOself) = sem
     in  (Syn_GlobalValue _lhsOself))
sem_GlobalValue_FunctionValue :: T_Identifier ->
                                 Type ->
                                 T_GlobalValue
sem_GlobalValue_FunctionValue n_ ty_ =
    (let _lhsOself :: GlobalValue
         _nIself :: Identifier
         _self =
             FunctionValue _nIself ty_
         _lhsOself =
             _self
         ( _nIself) =
             n_
     in  ( _lhsOself))
sem_GlobalValue_GlobalAlias :: T_Identifier ->
                               Type ->
                               T_GlobalValue
sem_GlobalValue_GlobalAlias n_ ty_ =
    (let _lhsOself :: GlobalValue
         _nIself :: Identifier
         _self =
             GlobalAlias _nIself ty_
         _lhsOself =
             _self
         ( _nIself) =
             n_
     in  ( _lhsOself))
sem_GlobalValue_GlobalVariable :: T_Identifier ->
                                  Type ->
                                  T_GlobalValue
sem_GlobalValue_GlobalVariable n_ ty_ =
    (let _lhsOself :: GlobalValue
         _nIself :: Identifier
         _self =
             GlobalVariable _nIself ty_
         _lhsOself =
             _self
         ( _nIself) =
             n_
     in  ( _lhsOself))
-- Globals -----------------------------------------------------
-- cata
sem_Globals :: Globals ->
               T_Globals
sem_Globals list =
    (Prelude.foldr sem_Globals_Cons sem_Globals_Nil (Prelude.map sem_Global list))
-- semantic domain
type T_Globals = ( Globals)
data Inh_Globals = Inh_Globals {}
data Syn_Globals = Syn_Globals {self_Syn_Globals :: Globals}
wrap_Globals :: T_Globals ->
                Inh_Globals ->
                Syn_Globals
wrap_Globals sem (Inh_Globals) =
    (let ( _lhsOself) = sem
     in  (Syn_Globals _lhsOself))
sem_Globals_Cons :: T_Global ->
                    T_Globals ->
                    T_Globals
sem_Globals_Cons hd_ tl_ =
    (let _lhsOself :: Globals
         _hdIself :: Global
         _tlIself :: Globals
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Globals_Nil :: T_Globals
sem_Globals_Nil =
    (let _lhsOself :: Globals
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
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
type T_Identifier = ( Identifier)
data Inh_Identifier = Inh_Identifier {}
data Syn_Identifier = Syn_Identifier {self_Syn_Identifier :: Identifier}
wrap_Identifier :: T_Identifier ->
                   Inh_Identifier ->
                   Syn_Identifier
wrap_Identifier sem (Inh_Identifier) =
    (let ( _lhsOself) = sem
     in  (Syn_Identifier _lhsOself))
sem_Identifier_Global :: T_Id ->
                         T_Identifier
sem_Identifier_Global name_ =
    (let _lhsOself :: Identifier
         _nameIself :: Id
         _self =
             Global _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
     in  ( _lhsOself))
sem_Identifier_Local :: T_Id ->
                        T_Identifier
sem_Identifier_Local name_ =
    (let _lhsOself :: Identifier
         _nameIself :: Id
         _self =
             Local _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
     in  ( _lhsOself))
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
         _hdIself :: Identifier
         _tlIself :: Identifiers
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
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
    (sem_Instruction_AShr (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Add _pc _id _ty _op1 _op2) =
    (sem_Instruction_Add (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Alloca _pc _id _ty _align) =
    (sem_Instruction_Alloca (sem_PC _pc) (sem_Identifier _id) _ty (sem_Align _align))
sem_Instruction (And _pc _id _ty _op1 _op2) =
    (sem_Instruction_And (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (AtomicRMW _pc _id _args _op _ord) =
    (sem_Instruction_AtomicRMW (sem_PC _pc) (sem_Identifier _id) (sem_Values _args) (sem_BinOp _op) (sem_AtomicOrdering _ord))
sem_Instruction (BitCast _pc _id _v _ty) =
    (sem_Instruction_BitCast (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) _ty)
sem_Instruction (Br _pc _v _t _f) =
    (sem_Instruction_Br (sem_PC _pc) (sem_Value _v) (sem_Value _t) (sem_Value _f))
sem_Instruction (Call _pc _mres _ty _callee _args) =
    (sem_Instruction_Call (sem_PC _pc) (sem_MIdentifier _mres) _ty (sem_Identifier _callee) (sem_Values _args))
sem_Instruction (CreateThread _pc _args) =
    (sem_Instruction_CreateThread (sem_PC _pc) (sem_Values _args))
sem_Instruction (ExtractValue _pc _id _aggr _idxs) =
    (sem_Instruction_ExtractValue (sem_PC _pc) (sem_Identifier _id) (sem_Value _aggr) (sem_Ints _idxs))
sem_Instruction (FAdd _pc _id _ty _op1 _op2) =
    (sem_Instruction_FAdd (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FCmp _pc _id _cond _ty _op1 _op2) =
    (sem_Instruction_FCmp (sem_PC _pc) (sem_Identifier _id) (sem_RealPredicate _cond) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FDiv _pc _id _ty _op1 _op2) =
    (sem_Instruction_FDiv (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FMul _pc _id _ty _op1 _op2) =
    (sem_Instruction_FMul (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FPExt _pc _id _v _ty) =
    (sem_Instruction_FPExt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) _ty)
sem_Instruction (FPToSI _pc _id _v _ty) =
    (sem_Instruction_FPToSI (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) _ty)
sem_Instruction (FPToUI _pc _id _v _ty) =
    (sem_Instruction_FPToUI (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) _ty)
sem_Instruction (FPTrunc _pc _id _v _ty) =
    (sem_Instruction_FPTrunc (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) _ty)
sem_Instruction (FRem _pc _id _ty _op1 _op2) =
    (sem_Instruction_FRem (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (FSub _pc _id _ty _op1 _op2) =
    (sem_Instruction_FSub (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (GetElementPtr _pc _id _ty _struct _idxs) =
    (sem_Instruction_GetElementPtr (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _struct) (sem_Values _idxs))
sem_Instruction (ICmp _pc _id _cond _ty _op1 _op2) =
    (sem_Instruction_ICmp (sem_PC _pc) (sem_Identifier _id) (sem_IntPredicate _cond) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (IntToPtr _pc _id _v _ty) =
    (sem_Instruction_IntToPtr (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) _ty)
sem_Instruction (LShr _pc _id _ty _op1 _op2) =
    (sem_Instruction_LShr (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Load _pc _id _v _align) =
    (sem_Instruction_Load (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) (sem_Align _align))
sem_Instruction (Mul _pc _id _ty _op1 _op2) =
    (sem_Instruction_Mul (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Or _pc _id _ty _op1 _op2) =
    (sem_Instruction_Or (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (PHI _pc _id _ty _vals) =
    (sem_Instruction_PHI (sem_PC _pc) (sem_Identifier _id) _ty (sem_PValues _vals))
sem_Instruction (PtrToInt _pc _id _v _ty) =
    (sem_Instruction_PtrToInt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) _ty)
sem_Instruction (Ret _pc _r) =
    (sem_Instruction_Ret (sem_PC _pc) (sem_RetInst _r))
sem_Instruction (SDiv _pc _id _ty _op1 _op2) =
    (sem_Instruction_SDiv (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (SExt _pc _id _v _ty) =
    (sem_Instruction_SExt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) _ty)
sem_Instruction (SIToFP _pc _id _v _ty) =
    (sem_Instruction_SIToFP (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) _ty)
sem_Instruction (SRem _pc _id _ty _op1 _op2) =
    (sem_Instruction_SRem (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Select _pc _id _cond _valt _valf) =
    (sem_Instruction_Select (sem_PC _pc) (sem_Identifier _id) (sem_Value _cond) (sem_Value _valt) (sem_Value _valf))
sem_Instruction (Shl _pc _id _ty _op1 _op2) =
    (sem_Instruction_Shl (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Store _pc _ty _v1 _v2 _align) =
    (sem_Instruction_Store (sem_PC _pc) _ty (sem_Value _v1) (sem_Value _v2) (sem_Align _align))
sem_Instruction (Sub _pc _id _ty _op1 _op2) =
    (sem_Instruction_Sub (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Switch _pc _elems) =
    (sem_Instruction_Switch (sem_PC _pc) (sem_IntTyValIdL _elems))
sem_Instruction (Trunc _pc _id _v _ty) =
    (sem_Instruction_Trunc (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) _ty)
sem_Instruction (UBr _pc _d) =
    (sem_Instruction_UBr (sem_PC _pc) (sem_Value _d))
sem_Instruction (UDiv _pc _id _ty _op1 _op2) =
    (sem_Instruction_UDiv (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (UIToFP _pc _id _v _ty) =
    (sem_Instruction_UIToFP (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) _ty)
sem_Instruction (URem _pc _id _ty _op1 _op2) =
    (sem_Instruction_URem (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (Unreachable _pc) =
    (sem_Instruction_Unreachable (sem_PC _pc))
sem_Instruction (Xor _pc _id _ty _op1 _op2) =
    (sem_Instruction_Xor (sem_PC _pc) (sem_Identifier _id) _ty (sem_Value _op1) (sem_Value _op2))
sem_Instruction (ZExt _pc _id _v _ty) =
    (sem_Instruction_ZExt (sem_PC _pc) (sem_Identifier _id) (sem_Value _v) _ty)
-- semantic domain
type T_Instruction = (Map.Map String Int) ->
                     Int ->
                     ( (Map.Map String Int),Int,Int,([Int]),Flow,Instruction)
data Inh_Instruction = Inh_Instruction {bbentrypci_Inh_Instruction :: (Map.Map String Int),cpc_Inh_Instruction :: Int}
data Syn_Instruction = Syn_Instruction {bbentrypc_Syn_Instruction :: (Map.Map String Int),cpc_Syn_Instruction :: Int,entrypc_Syn_Instruction :: Int,exitpcs_Syn_Instruction :: ([Int]),flow_Syn_Instruction :: Flow,self_Syn_Instruction :: Instruction}
wrap_Instruction :: T_Instruction ->
                    Inh_Instruction ->
                    Syn_Instruction
wrap_Instruction sem (Inh_Instruction _lhsIbbentrypci _lhsIcpc) =
    (let ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself) = sem _lhsIbbentrypci _lhsIcpc
     in  (Syn_Instruction _lhsObbentrypc _lhsOcpc _lhsOentrypc _lhsOexitpcs _lhsOflow _lhsOself))
sem_Instruction_AShr :: T_PC ->
                        T_Identifier ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_AShr pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 2403 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 2408 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2413 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2418 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 2423 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2428 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2433 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  AShr _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Add :: T_PC ->
                       T_Identifier ->
                       Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Add pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 2470 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 2475 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2480 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2485 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 2490 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2495 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2500 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Add _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Alloca :: T_PC ->
                          T_Identifier ->
                          Type ->
                          T_Align ->
                          T_Instruction
sem_Instruction_Alloca pc_ id_ ty_ align_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _alignIself :: Align
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 2535 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 2540 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2545 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2550 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 2555 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2560 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2565 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Alloca _pcIself _idIself ty_ _alignIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _alignIself) =
                  align_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_And :: T_PC ->
                       T_Identifier ->
                       Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_And pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 2600 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 2605 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2610 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2615 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 2620 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2625 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2630 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  And _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_AtomicRMW :: T_PC ->
                             T_Identifier ->
                             T_Values ->
                             T_BinOp ->
                             T_AtomicOrdering ->
                             T_Instruction
sem_Instruction_AtomicRMW pc_ id_ args_ op_ ord_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _argsIself :: Values
              _opIself :: BinOp
              _ordIself :: AtomicOrdering
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 2668 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 2673 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2678 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2683 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 2688 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2693 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2698 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  AtomicRMW _pcIself _idIself _argsIself _opIself _ordIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _argsIself) =
                  args_
              ( _opIself) =
                  op_
              ( _ordIself) =
                  ord_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_BitCast :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           Type ->
                           T_Instruction
sem_Instruction_BitCast pc_ id_ v_ ty_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 2735 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 2740 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2745 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2750 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 2755 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2760 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2765 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  BitCast _pcIself _idIself _vIself ty_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Br :: T_PC ->
                      T_Value ->
                      T_Value ->
                      T_Value ->
                      T_Instruction
sem_Instruction_Br pc_ v_ t_ f_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _vIself :: Value
              _tIself :: Value
              _fIself :: Value
              _lhsOflow =
                  ({-# LINE 61 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 2799 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 61 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself), (_pcIself, getEntry (getId _tIself) _lhsIbbentrypci), (_pcIself, getEntry (getId _fIself) _lhsIbbentrypci)]
                   {-# LINE 2804 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 62 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2809 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2814 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 2819 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2824 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 61 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2829 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Br _pcIself _vIself _tIself _fIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _vIself) =
                  v_
              ( _tIself) =
                  t_
              ( _fIself) =
                  f_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Call :: T_PC ->
                        T_MIdentifier ->
                        Type ->
                        T_Identifier ->
                        T_Values ->
                        T_Instruction
sem_Instruction_Call pc_ mres_ ty_ callee_ args_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _mresIself :: MIdentifier
              _calleeIself :: Identifier
              _argsIself :: Values
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 2866 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 2871 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2876 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2881 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 2886 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2891 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2896 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Call _pcIself _mresIself ty_ _calleeIself _argsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _mresIself) =
                  mres_
              ( _calleeIself) =
                  callee_
              ( _argsIself) =
                  args_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_CreateThread :: T_PC ->
                                T_Values ->
                                T_Instruction
sem_Instruction_CreateThread pc_ args_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _argsIself :: Values
              _lhsOflow =
                  ({-# LINE 55 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 2928 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 55 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself), (_pcIself, -1)]
                   {-# LINE 2933 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 56 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2938 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 2943 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 2948 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2953 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 55 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 2958 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  CreateThread _pcIself _argsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _argsIself) =
                  args_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_ExtractValue :: T_PC ->
                                T_Identifier ->
                                T_Value ->
                                T_Ints ->
                                T_Instruction
sem_Instruction_ExtractValue pc_ id_ aggr_ idxs_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _aggrIself :: Value
              _idxsIself :: Ints
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 2990 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 2995 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3000 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3005 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3010 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3015 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3020 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  ExtractValue _pcIself _idIself _aggrIself _idxsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _aggrIself) =
                  aggr_
              ( _idxsIself) =
                  idxs_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_FAdd :: T_PC ->
                        T_Identifier ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FAdd pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3057 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3062 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3067 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3072 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3077 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3082 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3087 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  FAdd _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_FCmp :: T_PC ->
                        T_Identifier ->
                        T_RealPredicate ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FCmp pc_ id_ cond_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _condIself :: RealPredicate
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3126 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3131 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3136 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3141 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3146 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3151 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3156 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  FCmp _pcIself _idIself _condIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _condIself) =
                  cond_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_FDiv :: T_PC ->
                        T_Identifier ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FDiv pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3195 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3200 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3205 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3210 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3215 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3220 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3225 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  FDiv _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_FMul :: T_PC ->
                        T_Identifier ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FMul pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3262 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3267 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3272 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3277 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3282 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3287 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3292 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  FMul _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_FPExt :: T_PC ->
                         T_Identifier ->
                         T_Value ->
                         Type ->
                         T_Instruction
sem_Instruction_FPExt pc_ id_ v_ ty_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3327 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3332 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3337 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3342 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3347 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3352 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3357 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  FPExt _pcIself _idIself _vIself ty_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_FPToSI :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          Type ->
                          T_Instruction
sem_Instruction_FPToSI pc_ id_ v_ ty_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3390 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3395 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3400 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3405 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3410 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3415 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3420 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  FPToSI _pcIself _idIself _vIself ty_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_FPToUI :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          Type ->
                          T_Instruction
sem_Instruction_FPToUI pc_ id_ v_ ty_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3453 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3458 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3463 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3468 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3473 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3478 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3483 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  FPToUI _pcIself _idIself _vIself ty_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_FPTrunc :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           Type ->
                           T_Instruction
sem_Instruction_FPTrunc pc_ id_ v_ ty_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3516 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3521 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3526 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3531 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3536 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3541 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3546 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  FPTrunc _pcIself _idIself _vIself ty_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_FRem :: T_PC ->
                        T_Identifier ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FRem pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3581 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3586 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3591 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3596 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3601 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3606 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3611 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  FRem _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_FSub :: T_PC ->
                        T_Identifier ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FSub pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3648 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3653 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3658 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3663 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3668 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3673 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3678 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  FSub _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_GetElementPtr :: T_PC ->
                                 T_Identifier ->
                                 Type ->
                                 T_Value ->
                                 T_Values ->
                                 T_Instruction
sem_Instruction_GetElementPtr pc_ id_ ty_ struct_ idxs_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _structIself :: Value
              _idxsIself :: Values
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3715 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3720 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3725 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3730 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3735 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3740 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3745 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  GetElementPtr _pcIself _idIself ty_ _structIself _idxsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _structIself) =
                  struct_
              ( _idxsIself) =
                  idxs_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_ICmp :: T_PC ->
                        T_Identifier ->
                        T_IntPredicate ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_ICmp pc_ id_ cond_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _condIself :: IntPredicate
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3784 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3789 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3794 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3799 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3804 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3809 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3814 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  ICmp _pcIself _idIself _condIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _condIself) =
                  cond_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_IntToPtr :: T_PC ->
                            T_Identifier ->
                            T_Value ->
                            Type ->
                            T_Instruction
sem_Instruction_IntToPtr pc_ id_ v_ ty_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3851 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3856 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3861 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3866 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3871 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3876 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3881 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  IntToPtr _pcIself _idIself _vIself ty_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_LShr :: T_PC ->
                        T_Identifier ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_LShr pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3916 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3921 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3926 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3931 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 3936 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3941 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 3946 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  LShr _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Load :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Align ->
                        T_Instruction
sem_Instruction_Load pc_ id_ v_ align_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _alignIself :: Align
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 3982 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 3987 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3992 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 3997 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4002 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4007 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4012 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Load _pcIself _idIself _vIself _alignIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
              ( _alignIself) =
                  align_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Mul :: T_PC ->
                       T_Identifier ->
                       Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Mul pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4049 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4054 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4059 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4064 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4069 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4074 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4079 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Mul _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Or :: T_PC ->
                      T_Identifier ->
                      Type ->
                      T_Value ->
                      T_Value ->
                      T_Instruction
sem_Instruction_Or pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4116 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4121 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4126 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4131 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4136 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4141 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4146 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Or _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_PHI :: T_PC ->
                       T_Identifier ->
                       Type ->
                       T_PValues ->
                       T_Instruction
sem_Instruction_PHI pc_ id_ ty_ vals_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _valsIself :: PValues
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4181 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4186 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4191 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4196 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4201 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4206 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4211 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  PHI _pcIself _idIself ty_ _valsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _valsIself) =
                  vals_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_PtrToInt :: T_PC ->
                            T_Identifier ->
                            T_Value ->
                            Type ->
                            T_Instruction
sem_Instruction_PtrToInt pc_ id_ v_ ty_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4244 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4249 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4254 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4259 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4264 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4269 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4274 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  PtrToInt _pcIself _idIself _vIself ty_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Ret :: T_PC ->
                       T_RetInst ->
                       T_Instruction
sem_Instruction_Ret pc_ r_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOexitpcs :: ([Int])
              _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOself :: Instruction
              _pcIself :: PC
              _rIself :: RetInst
              _lhsOexitpcs =
                  ({-# LINE 121 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _exitpcs_augmented_syn [_exitpcs_augmented_f1]
                   {-# LINE 4304 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _exitpcs_augmented_f1 =
                  ({-# LINE 121 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [_pcIself]
                   {-# LINE 4309 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4314 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4319 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4324 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4329 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4334 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _exitpcs_augmented_syn =
                  ({-# LINE 121 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4339 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4344 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Ret _pcIself _rIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _rIself) =
                  r_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_SDiv :: T_PC ->
                        T_Identifier ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_SDiv pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4377 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4382 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4387 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4392 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4397 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4402 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4407 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  SDiv _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_SExt :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        Type ->
                        T_Instruction
sem_Instruction_SExt pc_ id_ v_ ty_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4442 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4447 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4452 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4457 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4462 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4467 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4472 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  SExt _pcIself _idIself _vIself ty_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_SIToFP :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          Type ->
                          T_Instruction
sem_Instruction_SIToFP pc_ id_ v_ ty_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4505 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4510 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4515 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4520 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4525 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4530 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4535 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  SIToFP _pcIself _idIself _vIself ty_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_SRem :: T_PC ->
                        T_Identifier ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_SRem pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4570 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4575 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4580 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4585 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4590 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4595 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4600 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  SRem _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Select :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Value ->
                          T_Value ->
                          T_Instruction
sem_Instruction_Select pc_ id_ cond_ valt_ valf_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _condIself :: Value
              _valtIself :: Value
              _valfIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4638 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4643 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4648 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4653 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4658 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4663 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4668 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Select _pcIself _idIself _condIself _valtIself _valfIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _condIself) =
                  cond_
              ( _valtIself) =
                  valt_
              ( _valfIself) =
                  valf_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Shl :: T_PC ->
                       T_Identifier ->
                       Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Shl pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4707 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4712 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4717 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4722 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4727 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4732 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4737 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Shl _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Store :: T_PC ->
                         Type ->
                         T_Value ->
                         T_Value ->
                         T_Align ->
                         T_Instruction
sem_Instruction_Store pc_ ty_ v1_ v2_ align_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _v1Iself :: Value
              _v2Iself :: Value
              _alignIself :: Align
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4774 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4779 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4784 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4789 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4794 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4799 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4804 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Store _pcIself ty_ _v1Iself _v2Iself _alignIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _v1Iself) =
                  v1_
              ( _v2Iself) =
                  v2_
              ( _alignIself) =
                  align_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Sub :: T_PC ->
                       T_Identifier ->
                       Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Sub pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4841 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4846 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4851 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4856 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4861 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4866 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4871 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Sub _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Switch :: T_PC ->
                          T_IntTyValIdL ->
                          T_Instruction
sem_Instruction_Switch pc_ elems_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOflow :: Flow
              _lhsOself :: Instruction
              _lhsOcpc :: Int
              _pcIself :: PC
              _elemsIself :: IntTyValIdL
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4903 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4908 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4913 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 31 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4918 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Switch _pcIself _elemsIself
              _lhsOself =
                  _self
              _lhsOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIcpc
                   {-# LINE 4927 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              ( _pcIself) =
                  pc_
              ( _elemsIself) =
                  elems_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Trunc :: T_PC ->
                         T_Identifier ->
                         T_Value ->
                         Type ->
                         T_Instruction
sem_Instruction_Trunc pc_ id_ v_ ty_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 4954 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 4959 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4964 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 4969 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 4974 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4979 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 4984 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Trunc _pcIself _idIself _vIself ty_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_UBr :: T_PC ->
                       T_Value ->
                       T_Instruction
sem_Instruction_UBr pc_ d_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _dIself :: Value
              _lhsOflow =
                  ({-# LINE 58 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 5014 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 58 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself), (_pcIself, getEntry (getId _dIself) _lhsIbbentrypci)]
                   {-# LINE 5019 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 59 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5024 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5029 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 5034 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5039 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 58 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5044 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  UBr _pcIself _dIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _dIself) =
                  d_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_UDiv :: T_PC ->
                        T_Identifier ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_UDiv pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 5077 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 5082 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5087 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5092 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 5097 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5102 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5107 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  UDiv _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_UIToFP :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          Type ->
                          T_Instruction
sem_Instruction_UIToFP pc_ id_ v_ ty_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 5142 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 5147 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5152 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5157 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 5162 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5167 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5172 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  UIToFP _pcIself _idIself _vIself ty_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_URem :: T_PC ->
                        T_Identifier ->
                        Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_URem pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 5207 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 5212 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5217 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5222 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 5227 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5232 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5237 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  URem _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Unreachable :: T_PC ->
                               T_Instruction
sem_Instruction_Unreachable pc_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 5267 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 5272 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5277 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5282 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 5287 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5292 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5297 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Unreachable _pcIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_Xor :: T_PC ->
                       T_Identifier ->
                       Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Xor pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _op1Iself :: Value
              _op2Iself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 5328 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 5333 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5338 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5343 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 5348 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5353 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5358 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  Xor _pcIself _idIself ty_ _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _op1Iself) =
                  op1_
              ( _op2Iself) =
                  op2_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instruction_ZExt :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        Type ->
                        T_Instruction
sem_Instruction_ZExt pc_ id_ v_ ty_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOflow :: Flow
              _lhsOcpc :: Int
              _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIself :: Identifier
              _vIself :: Value
              _lhsOflow =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   foldr ($) _flow_augmented_syn [_flow_augmented_f1]
                   {-# LINE 5393 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_f1 =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   (++) [(_lhsIcpc, _pcIself)]
                   {-# LINE 5398 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOcpc =
                  ({-# LINE 53 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5403 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOentrypc =
                  ({-# LINE 117 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _pcIself
                   {-# LINE 5408 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 5413 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5418 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _flow_augmented_syn =
                  ({-# LINE 52 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5423 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  ZExt _pcIself _idIself _vIself ty_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIself) =
                  id_
              ( _vIself) =
                  v_
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
-- Instructions ------------------------------------------------
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = (Map.Map String Int) ->
                      Int ->
                      ( (Map.Map String Int),Int,Int,([Int]),Flow,Instructions)
data Inh_Instructions = Inh_Instructions {bbentrypci_Inh_Instructions :: (Map.Map String Int),cpc_Inh_Instructions :: Int}
data Syn_Instructions = Syn_Instructions {bbentrypc_Syn_Instructions :: (Map.Map String Int),cpc_Syn_Instructions :: Int,entrypc_Syn_Instructions :: Int,exitpcs_Syn_Instructions :: ([Int]),flow_Syn_Instructions :: Flow,self_Syn_Instructions :: Instructions}
wrap_Instructions :: T_Instructions ->
                     Inh_Instructions ->
                     Syn_Instructions
wrap_Instructions sem (Inh_Instructions _lhsIbbentrypci _lhsIcpc) =
    (let ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself) = sem _lhsIbbentrypci _lhsIcpc
     in  (Syn_Instructions _lhsObbentrypc _lhsOcpc _lhsOentrypc _lhsOexitpcs _lhsOflow _lhsOself))
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOflow :: Flow
              _lhsOself :: Instructions
              _lhsOcpc :: Int
              _hdObbentrypci :: (Map.Map String Int)
              _hdOcpc :: Int
              _tlObbentrypci :: (Map.Map String Int)
              _tlOcpc :: Int
              _hdIbbentrypc :: (Map.Map String Int)
              _hdIcpc :: Int
              _hdIentrypc :: Int
              _hdIexitpcs :: ([Int])
              _hdIflow :: Flow
              _hdIself :: Instruction
              _tlIbbentrypc :: (Map.Map String Int)
              _tlIcpc :: Int
              _tlIentrypc :: Int
              _tlIexitpcs :: ([Int])
              _tlIflow :: Flow
              _tlIself :: Instructions
              _lhsOentrypc =
                  ({-# LINE 113 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _hdIentrypc
                   {-# LINE 5485 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _hdIbbentrypc `Map.union` _tlIbbentrypc
                   {-# LINE 5490 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _hdIexitpcs ++ _tlIexitpcs
                   {-# LINE 5495 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 31 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _hdIflow ++ _tlIflow
                   {-# LINE 5500 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _tlIcpc
                   {-# LINE 5509 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _hdObbentrypci =
                  ({-# LINE 32 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIbbentrypci
                   {-# LINE 5514 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _hdOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIcpc
                   {-# LINE 5519 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _tlObbentrypci =
                  ({-# LINE 32 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIbbentrypci
                   {-# LINE 5524 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _tlOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _hdIcpc
                   {-# LINE 5529 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              ( _hdIbbentrypc,_hdIcpc,_hdIentrypc,_hdIexitpcs,_hdIflow,_hdIself) =
                  hd_ _hdObbentrypci _hdOcpc
              ( _tlIbbentrypc,_tlIcpc,_tlIentrypc,_tlIexitpcs,_tlIflow,_tlIself) =
                  tl_ _tlObbentrypci _tlOcpc
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    (\ _lhsIbbentrypci
       _lhsIcpc ->
         (let _lhsOentrypc :: Int
              _lhsObbentrypc :: (Map.Map String Int)
              _lhsOexitpcs :: ([Int])
              _lhsOflow :: Flow
              _lhsOself :: Instructions
              _lhsOcpc :: Int
              _lhsOentrypc =
                  ({-# LINE 111 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   error "'entrypc' of an empty i list"
                   {-# LINE 5549 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsObbentrypc =
                  ({-# LINE 86 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   Map.empty
                   {-# LINE 5554 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOexitpcs =
                  ({-# LINE 85 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5559 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 31 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   []
                   {-# LINE 5564 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOcpc =
                  ({-# LINE 24 "src/Concurrent/Model/Analysis/Flow.ag" #-}
                   _lhsIcpc
                   {-# LINE 5573 "src/Concurrent/Model/Analysis/Flow.hs" #-}
                   )
          in  ( _lhsObbentrypc,_lhsOcpc,_lhsOentrypc,_lhsOexitpcs,_lhsOflow,_lhsOself)))
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
    (sem_IntTyValId_Tuple x1 (sem_Value x2) (sem_Identifier x3))
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
sem_IntTyValId_Tuple :: Type ->
                        T_Value ->
                        T_Identifier ->
                        T_IntTyValId
sem_IntTyValId_Tuple x1_ x2_ x3_ =
    (let _lhsOself :: IntTyValId
         _x2Iself :: Value
         _x3Iself :: Identifier
         _self =
             (x1_,_x2Iself,_x3Iself)
         _lhsOself =
             _self
         ( _x2Iself) =
             x2_
         ( _x3Iself) =
             x3_
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
         _justIself :: Identifier
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
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
         _justIself :: Value
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
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
         _funsObbentrypci :: (Map.Map String (Map.Map String Int))
         _layoutIself :: DataLayout
         _targetIself :: TargetData
         _gvarsIself :: Globals
         _funsIbbentrypc :: (Map.Map String (Map.Map String Int))
         _funsIflow :: Flow
         _funsIself :: Functions
         _nmdtysIself :: NamedTypes
         _self =
             Module id_ _layoutIself _targetIself _gvarsIself _funsIself _nmdtysIself
         _lhsOself =
             _self
         _funsObbentrypci =
             ({-# LINE 27 "src/Concurrent/Model/Analysis/Flow.ag" #-}
              error "missing rule: Module.Module.funs.bbentrypci"
              {-# LINE 6654 "src/Concurrent/Model/Analysis/Flow.hs" #-}
              )
         ( _layoutIself) =
             layout_
         ( _targetIself) =
             target_
         ( _gvarsIself) =
             gvars_
         ( _funsIbbentrypc,_funsIflow,_funsIself) =
             funs_ _funsObbentrypci
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
    (Data.Map.foldrWithKey sem_NamedTypes_Entry sem_NamedTypes_Nil m)
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
                        Type ->
                        T_NamedTypes ->
                        T_NamedTypes
sem_NamedTypes_Entry key_ val_ tl_ =
    (let _lhsOself :: NamedTypes
         _tlIself :: NamedTypes
         _self =
             Data.Map.insert key_ val_ _tlIself
         _lhsOself =
             _self
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
    (sem_PTyInt_Tuple x1 x2)
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
sem_PTyInt_Tuple :: Type ->
                    Int ->
                    T_PTyInt
sem_PTyInt_Tuple x1_ x2_ =
    (let _lhsOself :: PTyInt
         _self =
             (x1_,x2_)
         _lhsOself =
             _self
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
         _x1Iself :: Value
         _x2Iself :: Value
         _self =
             (_x1Iself,_x2Iself)
         _lhsOself =
             _self
         ( _x1Iself) =
             x1_
         ( _x2Iself) =
             x2_
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
         _x1Iself :: Value
         _self =
             (_x1Iself,x2_)
         _lhsOself =
             _self
         ( _x1Iself) =
             x1_
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
    (sem_Parameter_Parameter (sem_Id _var) _ty)
-- semantic domain
type T_Parameter = ( Parameter)
data Inh_Parameter = Inh_Parameter {}
data Syn_Parameter = Syn_Parameter {self_Syn_Parameter :: Parameter}
wrap_Parameter :: T_Parameter ->
                  Inh_Parameter ->
                  Syn_Parameter
wrap_Parameter sem (Inh_Parameter) =
    (let ( _lhsOself) = sem
     in  (Syn_Parameter _lhsOself))
sem_Parameter_Parameter :: T_Id ->
                           Type ->
                           T_Parameter
sem_Parameter_Parameter var_ ty_ =
    (let _lhsOself :: Parameter
         _varIself :: Id
         _self =
             Parameter _varIself ty_
         _lhsOself =
             _self
         ( _varIself) =
             var_
     in  ( _lhsOself))
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
         _hdIself :: Parameter
         _tlIself :: Parameters
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
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
         _vIself :: Value
         _self =
             ValueRet _vIself
         _lhsOself =
             _self
         ( _vIself) =
             v_
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
-- Value -------------------------------------------------------
-- cata
sem_Value :: Value ->
             T_Value
sem_Value (Constant _c) =
    (sem_Value_Constant (sem_Constant _c))
sem_Value (Id _v _ty) =
    (sem_Value_Id (sem_Identifier _v) _ty)
-- semantic domain
type T_Value = ( Value)
data Inh_Value = Inh_Value {}
data Syn_Value = Syn_Value {self_Syn_Value :: Value}
wrap_Value :: T_Value ->
              Inh_Value ->
              Syn_Value
wrap_Value sem (Inh_Value) =
    (let ( _lhsOself) = sem
     in  (Syn_Value _lhsOself))
sem_Value_Constant :: T_Constant ->
                      T_Value
sem_Value_Constant c_ =
    (let _lhsOself :: Value
         _cIself :: Constant
         _self =
             Constant _cIself
         _lhsOself =
             _self
         ( _cIself) =
             c_
     in  ( _lhsOself))
sem_Value_Id :: T_Identifier ->
                Type ->
                T_Value
sem_Value_Id v_ ty_ =
    (let _lhsOself :: Value
         _vIself :: Identifier
         _self =
             Id _vIself ty_
         _lhsOself =
             _self
         ( _vIself) =
             v_
     in  ( _lhsOself))
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
type T_Values = ( Values)
data Inh_Values = Inh_Values {}
data Syn_Values = Syn_Values {self_Syn_Values :: Values}
wrap_Values :: T_Values ->
               Inh_Values ->
               Syn_Values
wrap_Values sem (Inh_Values) =
    (let ( _lhsOself) = sem
     in  (Syn_Values _lhsOself))
sem_Values_Cons :: T_Value ->
                   T_Values ->
                   T_Values
sem_Values_Cons hd_ tl_ =
    (let _lhsOself :: Values
         _hdIself :: Value
         _tlIself :: Values
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Values_Nil :: T_Values
sem_Values_Nil =
    (let _lhsOself :: Values
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
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