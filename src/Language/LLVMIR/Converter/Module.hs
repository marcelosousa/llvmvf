

-- UUAGC 0.9.42.2 (src/Language/LLVMIR/Converter/Module.ag)
module Language.LLVMIR.Converter.Module where

{-# LINE 14 "./src/Language/LLVMIR/Converter/Module.ag" #-}

import Language.HTm.Base
import Language.LLVMIR
import Data.Maybe
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace (trace)
{-# LINE 16 "src/Language/LLVMIR/Converter/Module.hs" #-}

{-# LINE 11 "src/Language/LLVMIR/Grammar/Base.ag" #-}

import Prelude              hiding (sequence)
import Data.Char            (chr)
import qualified Data.Map as Data.Map
import qualified Data.Map as Map
#if __GLASGOW_HASKELL__ > 704
import Data.Map hiding (foldr)
#else
import Data.Map 
#endif
{-# LINE 29 "src/Language/LLVMIR/Converter/Module.hs" #-}
{-# LINE 1 "./src/Language/LLVMIR/Converter/Module.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Converter
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 36 "src/Language/LLVMIR/Converter/Module.hs" #-}

{-# LINE 162 "./src/Language/LLVMIR/Converter/Module.ag" #-}

getIdentifier :: Identifier -> String
getIdentifier (Global a) = a
getIdentifier (Local  a) = a

getId :: ETm -> Label 
getId (EVar i) = i 

getLabel :: Value -> Label
getLabel (Id i t) = case i of
                   Global s -> s
                   Local  s -> s
getLabel _ = error "getLabel"

calletm :: ETm -> [ETm] -> ETm
calletm c [] = c
calletm c (x:xs) = calletm (EApp c x) xs

buildbb :: Label -> Map Label ([Label], [ETm] -> ETm) -> ETm
buildbb n m = case M.lookup n m of
               Just (l, f) -> let x = Prelude.map (\e -> buildbb e m) l
                              in trace (show n ++ ": " ++ show x) $ f x
               Nothing  -> error $ n ++ " not in map:" 
{-# LINE 62 "src/Language/LLVMIR/Converter/Module.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Grammar/Base.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Base
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 70 "src/Language/LLVMIR/Converter/Module.hs" #-}

{-# LINE 152 "src/Language/LLVMIR/Grammar/Base.ag" #-}

emptyFunction :: Function
emptyFunction = FunctionDef (Global "undefined") ExternalLinkage TyVoid [] []
{-# LINE 76 "src/Language/LLVMIR/Converter/Module.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Grammar/Instruction.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Grammar.Instruction
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 84 "src/Language/LLVMIR/Converter/Module.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Type/Type.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Type.Type
-- Copyright :  (c) 2012 Marcelo Sousa
-- Standard LLVM IR Types
-------------------------------------------------------------------------------
{-# LINE 93 "src/Language/LLVMIR/Converter/Module.hs" #-}
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
         _argIetm :: ETm
         _argIself :: Value
         _self =
             Argument _argIself
         _lhsOself =
             _self
         ( _argIetm,_argIself) =
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
    (sem_BasicBlock_BasicBlock (sem_Label _label) (sem_Instructions _instrs))
-- semantic domain
type T_BasicBlock = ( ((Label, ([Label], [ETm] -> ETm))),((Label,[(Ident, [(Value, Label)])])),BasicBlock)
data Inh_BasicBlock = Inh_BasicBlock {}
data Syn_BasicBlock = Syn_BasicBlock {etm_Syn_BasicBlock :: ((Label, ([Label], [ETm] -> ETm))),phis_Syn_BasicBlock :: ((Label,[(Ident, [(Value, Label)])])),self_Syn_BasicBlock :: BasicBlock}
wrap_BasicBlock :: T_BasicBlock ->
                   Inh_BasicBlock ->
                   Syn_BasicBlock
wrap_BasicBlock sem (Inh_BasicBlock) =
    (let ( _lhsOetm,_lhsOphis,_lhsOself) = sem
     in  (Syn_BasicBlock _lhsOetm _lhsOphis _lhsOself))
sem_BasicBlock_BasicBlock :: T_Label ->
                             T_Instructions ->
                             T_BasicBlock
sem_BasicBlock_BasicBlock label_ instrs_ =
    (let _lhsOetm :: ((Label, ([Label], [ETm] -> ETm)))
         _lhsOphis :: ((Label,[(Ident, [(Value, Label)])]))
         _lhsOself :: BasicBlock
         _instrsOcbb :: Label
         _labelIself :: Label
         _instrsIetm :: (([Label], [ETm] -> ETm))
         _instrsIphis :: ([(Ident, [(Value, Label)])])
         _instrsIself :: Instructions
         _lhsOetm =
             ({-# LINE 94 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              (_labelIself,_instrsIetm)
              {-# LINE 625 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _lhsOphis =
             ({-# LINE 95 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              (_labelIself,_instrsIphis)
              {-# LINE 630 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             BasicBlock _labelIself _instrsIself
         _lhsOself =
             _self
         _instrsOcbb =
             ({-# LINE 100 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              error "missing rule: BasicBlock.BasicBlock.instrs.cbb"
              {-# LINE 639 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         ( _labelIself) =
             label_
         ( _instrsIetm,_instrsIphis,_instrsIself) =
             instrs_ _instrsOcbb
     in  ( _lhsOetm,_lhsOphis,_lhsOself))
-- BasicBlocks -------------------------------------------------
-- cata
sem_BasicBlocks :: BasicBlocks ->
                   T_BasicBlocks
sem_BasicBlocks list =
    (Prelude.foldr sem_BasicBlocks_Cons sem_BasicBlocks_Nil (Prelude.map sem_BasicBlock list))
-- semantic domain
type T_BasicBlocks = (Map Label [(Ident, [(Value, Label)])]) ->
                     ( (Map Label ([Label], [ETm] -> ETm)),(Map Label [(Ident, [(Value, Label)])]),BasicBlocks)
data Inh_BasicBlocks = Inh_BasicBlocks {phim_Inh_BasicBlocks :: (Map Label [(Ident, [(Value, Label)])])}
data Syn_BasicBlocks = Syn_BasicBlocks {etm_Syn_BasicBlocks :: (Map Label ([Label], [ETm] -> ETm)),phis_Syn_BasicBlocks :: (Map Label [(Ident, [(Value, Label)])]),self_Syn_BasicBlocks :: BasicBlocks}
wrap_BasicBlocks :: T_BasicBlocks ->
                    Inh_BasicBlocks ->
                    Syn_BasicBlocks
wrap_BasicBlocks sem (Inh_BasicBlocks _lhsIphim) =
    (let ( _lhsOetm,_lhsOphis,_lhsOself) = sem _lhsIphim
     in  (Syn_BasicBlocks _lhsOetm _lhsOphis _lhsOself))
sem_BasicBlocks_Cons :: T_BasicBlock ->
                        T_BasicBlocks ->
                        T_BasicBlocks
sem_BasicBlocks_Cons hd_ tl_ =
    (\ _lhsIphim ->
         (let _lhsOetm :: (Map Label ([Label], [ETm] -> ETm))
              _lhsOphis :: (Map Label [(Ident, [(Value, Label)])])
              _lhsOself :: BasicBlocks
              _tlOphim :: (Map Label [(Ident, [(Value, Label)])])
              _hdIetm :: ((Label, ([Label], [ETm] -> ETm)))
              _hdIphis :: ((Label,[(Ident, [(Value, Label)])]))
              _hdIself :: BasicBlock
              _tlIetm :: (Map Label ([Label], [ETm] -> ETm))
              _tlIphis :: (Map Label [(Ident, [(Value, Label)])])
              _tlIself :: BasicBlocks
              _lhsOetm =
                  ({-# LINE 86 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   let (l,etm) = _hdIetm
                   in M.insert l etm _tlIetm
                   {-# LINE 682 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 88 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   let (l,phi) = _hdIphis
                   in if phi == []
                      then _tlIphis
                      else M.insert l phi _tlIphis
                   {-# LINE 690 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _tlOphim =
                  ({-# LINE 76 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   _lhsIphim
                   {-# LINE 699 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              ( _hdIetm,_hdIphis,_hdIself) =
                  hd_
              ( _tlIetm,_tlIphis,_tlIself) =
                  tl_ _tlOphim
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_BasicBlocks_Nil :: T_BasicBlocks
sem_BasicBlocks_Nil =
    (\ _lhsIphim ->
         (let _lhsOetm :: (Map Label ([Label], [ETm] -> ETm))
              _lhsOphis :: (Map Label [(Ident, [(Value, Label)])])
              _lhsOself :: BasicBlocks
              _lhsOetm =
                  ({-# LINE 84 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   M.empty
                   {-# LINE 715 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 85 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   M.empty
                   {-# LINE 720 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
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
type T_CompareConstantExpr = ( CompareConstantExpr)
data Inh_CompareConstantExpr = Inh_CompareConstantExpr {}
data Syn_CompareConstantExpr = Syn_CompareConstantExpr {self_Syn_CompareConstantExpr :: CompareConstantExpr}
wrap_CompareConstantExpr :: T_CompareConstantExpr ->
                            Inh_CompareConstantExpr ->
                            Syn_CompareConstantExpr
wrap_CompareConstantExpr sem (Inh_CompareConstantExpr) =
    (let ( _lhsOself) = sem
     in  (Syn_CompareConstantExpr _lhsOself))
sem_CompareConstantExpr_ICmpExpr :: T_IntPredicate ->
                                    T_Type ->
                                    T_Value ->
                                    T_Value ->
                                    T_CompareConstantExpr
sem_CompareConstantExpr_ICmpExpr cond_ ty_ op1_ op2_ =
    (let _lhsOself :: CompareConstantExpr
         _condIself :: IntPredicate
         _tyIself :: Type
         _op1Ietm :: ETm
         _op1Iself :: Value
         _op2Ietm :: ETm
         _op2Iself :: Value
         _self =
             ICmpExpr _condIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _condIself) =
             cond_
         ( _tyIself) =
             ty_
         ( _op1Ietm,_op1Iself) =
             op1_
         ( _op2Ietm,_op2Iself) =
             op2_
     in  ( _lhsOself))
sem_CompareConstantExpr_FCmpExpr :: T_RealPredicate ->
                                    T_Type ->
                                    T_Value ->
                                    T_Value ->
                                    T_CompareConstantExpr
sem_CompareConstantExpr_FCmpExpr cond_ ty_ op1_ op2_ =
    (let _lhsOself :: CompareConstantExpr
         _condIself :: RealPredicate
         _tyIself :: Type
         _op1Ietm :: ETm
         _op1Iself :: Value
         _op2Ietm :: ETm
         _op2Iself :: Value
         _self =
             FCmpExpr _condIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _condIself) =
             cond_
         ( _tyIself) =
             ty_
         ( _op1Ietm,_op1Iself) =
             op1_
         ( _op2Ietm,_op2Iself) =
             op2_
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
type T_Constant = ( ETm,Constant)
data Inh_Constant = Inh_Constant {}
data Syn_Constant = Syn_Constant {etm_Syn_Constant :: ETm,self_Syn_Constant :: Constant}
wrap_Constant :: T_Constant ->
                 Inh_Constant ->
                 Syn_Constant
wrap_Constant sem (Inh_Constant) =
    (let ( _lhsOetm,_lhsOself) = sem
     in  (Syn_Constant _lhsOetm _lhsOself))
sem_Constant_BlockAddr :: T_Constant
sem_Constant_BlockAddr =
    (let _lhsOetm :: ETm
         _lhsOself :: Constant
         _lhsOetm =
             ({-# LINE 160 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1031 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             BlockAddr
         _lhsOself =
             _self
     in  ( _lhsOetm,_lhsOself))
sem_Constant_ConstantAggregateZero :: T_Type ->
                                      T_Constant
sem_Constant_ConstantAggregateZero ty_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Constant
         _tyIself :: Type
         _lhsOetm =
             ({-# LINE 160 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1047 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             ConstantAggregateZero _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
     in  ( _lhsOetm,_lhsOself))
sem_Constant_ConstantArray :: T_Type ->
                              T_Values ->
                              T_Constant
sem_Constant_ConstantArray ty_ vals_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Constant
         _tyIself :: Type
         _valsIetm :: ([ETm])
         _valsIself :: Values
         _lhsOetm =
             ({-# LINE 160 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1068 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             ConstantArray _tyIself _valsIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
         ( _valsIetm,_valsIself) =
             vals_
     in  ( _lhsOetm,_lhsOself))
sem_Constant_ConstantDataSequential :: T_ConstantDataSequential ->
                                       T_Constant
sem_Constant_ConstantDataSequential cds_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Constant
         _cdsIself :: ConstantDataSequential
         _lhsOetm =
             ({-# LINE 160 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1088 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             ConstantDataSequential _cdsIself
         _lhsOself =
             _self
         ( _cdsIself) =
             cds_
     in  ( _lhsOetm,_lhsOself))
sem_Constant_ConstantExpr :: T_ConstantExpr ->
                             T_Constant
sem_Constant_ConstantExpr expr_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Constant
         _exprIself :: ConstantExpr
         _lhsOetm =
             ({-# LINE 160 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1106 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             ConstantExpr _exprIself
         _lhsOself =
             _self
         ( _exprIself) =
             expr_
     in  ( _lhsOetm,_lhsOself))
sem_Constant_ConstantFP :: T_ConstantFP ->
                           T_Constant
sem_Constant_ConstantFP fp_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Constant
         _fpIself :: ConstantFP
         _lhsOetm =
             ({-# LINE 160 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1124 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             ConstantFP _fpIself
         _lhsOself =
             _self
         ( _fpIself) =
             fp_
     in  ( _lhsOetm,_lhsOself))
sem_Constant_ConstantInt :: Int ->
                            T_Type ->
                            T_Constant
sem_Constant_ConstantInt iv_ ty_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Constant
         _tyIself :: Type
         _lhsOetm =
             ({-# LINE 159 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              ENum iv_
              {-# LINE 1143 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             ConstantInt iv_ _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
     in  ( _lhsOetm,_lhsOself))
sem_Constant_ConstantPointerNull :: T_Type ->
                                    T_Constant
sem_Constant_ConstantPointerNull ty_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Constant
         _tyIself :: Type
         _lhsOetm =
             ({-# LINE 160 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1161 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             ConstantPointerNull _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
     in  ( _lhsOetm,_lhsOself))
sem_Constant_ConstantStruct :: T_Type ->
                               T_Values ->
                               T_Constant
sem_Constant_ConstantStruct ty_ vals_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Constant
         _tyIself :: Type
         _valsIetm :: ([ETm])
         _valsIself :: Values
         _lhsOetm =
             ({-# LINE 160 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1182 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             ConstantStruct _tyIself _valsIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
         ( _valsIetm,_valsIself) =
             vals_
     in  ( _lhsOetm,_lhsOself))
sem_Constant_ConstantVector :: T_Constant
sem_Constant_ConstantVector =
    (let _lhsOetm :: ETm
         _lhsOself :: Constant
         _lhsOetm =
             ({-# LINE 160 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1200 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             ConstantVector
         _lhsOself =
             _self
     in  ( _lhsOetm,_lhsOself))
sem_Constant_GlobalValue :: T_GlobalValue ->
                            T_Constant
sem_Constant_GlobalValue gv_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Constant
         _gvIself :: GlobalValue
         _lhsOetm =
             ({-# LINE 160 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1216 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             GlobalValue _gvIself
         _lhsOself =
             _self
         ( _gvIself) =
             gv_
     in  ( _lhsOetm,_lhsOself))
sem_Constant_UndefValue :: T_Constant
sem_Constant_UndefValue =
    (let _lhsOetm :: ETm
         _lhsOself :: Constant
         _lhsOetm =
             ({-# LINE 160 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1232 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             UndefValue
         _lhsOself =
             _self
     in  ( _lhsOetm,_lhsOself))
-- ConstantDataSequential --------------------------------------
-- cata
sem_ConstantDataSequential :: ConstantDataSequential ->
                              T_ConstantDataSequential
sem_ConstantDataSequential (ConstantDataArray _ty _val) =
    (sem_ConstantDataSequential_ConstantDataArray (sem_Type _ty) _val)
sem_ConstantDataSequential (ConstantDataVector _ty _val) =
    (sem_ConstantDataSequential_ConstantDataVector (sem_Type _ty) _val)
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
sem_ConstantDataSequential_ConstantDataArray :: T_Type ->
                                                String ->
                                                T_ConstantDataSequential
sem_ConstantDataSequential_ConstantDataArray ty_ val_ =
    (let _lhsOself :: ConstantDataSequential
         _tyIself :: Type
         _self =
             ConstantDataArray _tyIself val_
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
     in  ( _lhsOself))
sem_ConstantDataSequential_ConstantDataVector :: T_Type ->
                                                 String ->
                                                 T_ConstantDataSequential
sem_ConstantDataSequential_ConstantDataVector ty_ val_ =
    (let _lhsOself :: ConstantDataSequential
         _tyIself :: Type
         _self =
             ConstantDataVector _tyIself val_
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
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
    (sem_ConstantExpr_UnaryConstantExpr _name _op (sem_Value _val) (sem_Type _ty))
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
         _structIetm :: ETm
         _structIself :: Value
         _idxsIetm :: ([ETm])
         _idxsIself :: Values
         _self =
             GetElementPtrConstantExpr _structIself _idxsIself
         _lhsOself =
             _self
         ( _structIetm,_structIself) =
             struct_
         ( _idxsIetm,_idxsIself) =
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
                                      T_Type ->
                                      T_ConstantExpr
sem_ConstantExpr_UnaryConstantExpr name_ op_ val_ ty_ =
    (let _lhsOself :: ConstantExpr
         _valIetm :: ETm
         _valIself :: Value
         _tyIself :: Type
         _self =
             UnaryConstantExpr name_ op_ _valIself _tyIself
         _lhsOself =
             _self
         ( _valIetm,_valIself) =
             val_
         ( _tyIself) =
             ty_
     in  ( _lhsOself))
-- ConstantFP --------------------------------------------------
-- cata
sem_ConstantFP :: ConstantFP ->
                  T_ConstantFP
sem_ConstantFP (ConstantFPFloat _fpv _ty) =
    (sem_ConstantFP_ConstantFPFloat _fpv (sem_Type _ty))
sem_ConstantFP (ConstantFPDouble _dbv _ty) =
    (sem_ConstantFP_ConstantFPDouble _dbv (sem_Type _ty))
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
sem_ConstantFP_ConstantFPFloat :: Float ->
                                  T_Type ->
                                  T_ConstantFP
sem_ConstantFP_ConstantFPFloat fpv_ ty_ =
    (let _lhsOself :: ConstantFP
         _tyIself :: Type
         _self =
             ConstantFPFloat fpv_ _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
     in  ( _lhsOself))
sem_ConstantFP_ConstantFPDouble :: Double ->
                                   T_Type ->
                                   T_ConstantFP
sem_ConstantFP_ConstantFPDouble dbv_ ty_ =
    (let _lhsOself :: ConstantFP
         _tyIself :: Type
         _self =
             ConstantFPDouble dbv_ _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
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
sem_Function (FunctionDef _name _linkage _retty _params _body) =
    (sem_Function_FunctionDef (sem_Identifier _name) (sem_Linkage _linkage) (sem_Type _retty) (sem_Parameters _params) (sem_BasicBlocks _body))
sem_Function (FunctionDecl _name _linkage _retty _params) =
    (sem_Function_FunctionDecl (sem_Identifier _name) (sem_Linkage _linkage) (sem_Type _retty) (sem_Parameters _params))
-- semantic domain
type T_Function = ( ETm,Function)
data Inh_Function = Inh_Function {}
data Syn_Function = Syn_Function {etm_Syn_Function :: ETm,self_Syn_Function :: Function}
wrap_Function :: T_Function ->
                 Inh_Function ->
                 Syn_Function
wrap_Function sem (Inh_Function) =
    (let ( _lhsOetm,_lhsOself) = sem
     in  (Syn_Function _lhsOetm _lhsOself))
sem_Function_FunctionDef :: T_Identifier ->
                            T_Linkage ->
                            T_Type ->
                            T_Parameters ->
                            T_BasicBlocks ->
                            T_Function
sem_Function_FunctionDef name_ linkage_ retty_ params_ body_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Function
         _bodyOphim :: (Map Label [(Ident, [(Value, Label)])])
         _nameIetm :: ETm
         _nameIself :: Identifier
         _linkageIself :: Linkage
         _rettyIself :: Type
         _paramsIetm :: ([ETm])
         _paramsIself :: Parameters
         _bodyIetm :: (Map Label ([Label], [ETm] -> ETm))
         _bodyIphis :: (Map Label [(Ident, [(Value, Label)])])
         _bodyIself :: BasicBlocks
         _lhsOetm =
             ({-# LINE 58 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              trace (show _bodyIphis) $ EFun (getIdentifier _nameIself) _paramsIetm (buildbb "bb" _bodyIetm)
              {-# LINE 1840 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             FunctionDef _nameIself _linkageIself _rettyIself _paramsIself _bodyIself
         _lhsOself =
             _self
         _bodyOphim =
             ({-# LINE 76 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              error "missing rule: Function.FunctionDef.body.phim"
              {-# LINE 1849 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         ( _nameIetm,_nameIself) =
             name_
         ( _linkageIself) =
             linkage_
         ( _rettyIself) =
             retty_
         ( _paramsIetm,_paramsIself) =
             params_
         ( _bodyIetm,_bodyIphis,_bodyIself) =
             body_ _bodyOphim
     in  ( _lhsOetm,_lhsOself))
sem_Function_FunctionDecl :: T_Identifier ->
                             T_Linkage ->
                             T_Type ->
                             T_Parameters ->
                             T_Function
sem_Function_FunctionDecl name_ linkage_ retty_ params_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Function
         _nameIetm :: ETm
         _nameIself :: Identifier
         _linkageIself :: Linkage
         _rettyIself :: Type
         _paramsIetm :: ([ETm])
         _paramsIself :: Parameters
         _lhsOetm =
             ({-# LINE 60 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1879 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             FunctionDecl _nameIself _linkageIself _rettyIself _paramsIself
         _lhsOself =
             _self
         ( _nameIetm,_nameIself) =
             name_
         ( _linkageIself) =
             linkage_
         ( _rettyIself) =
             retty_
         ( _paramsIetm,_paramsIself) =
             params_
     in  ( _lhsOetm,_lhsOself))
-- Functions ---------------------------------------------------
-- cata
sem_Functions :: Functions ->
                 T_Functions
sem_Functions m =
    (Data.Map.foldrWithKey sem_Functions_Entry sem_Functions_Nil (Data.Map.map sem_Function m))
-- semantic domain
type T_Functions = ( ETm,Functions)
data Inh_Functions = Inh_Functions {}
data Syn_Functions = Syn_Functions {etm_Syn_Functions :: ETm,self_Syn_Functions :: Functions}
wrap_Functions :: T_Functions ->
                  Inh_Functions ->
                  Syn_Functions
wrap_Functions sem (Inh_Functions) =
    (let ( _lhsOetm,_lhsOself) = sem
     in  (Syn_Functions _lhsOetm _lhsOself))
sem_Functions_Entry :: String ->
                       T_Function ->
                       T_Functions ->
                       T_Functions
sem_Functions_Entry key_ val_ tl_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Functions
         _valIetm :: ETm
         _valIself :: Function
         _tlIetm :: ETm
         _tlIself :: Functions
         _lhsOetm =
             ({-# LINE 48 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              if key_ == "main"
              then let x = _valIetm
                   in trace (show x) $ x
              else _tlIetm
              {-# LINE 1927 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             Data.Map.insert key_ _valIself _tlIself
         _lhsOself =
             _self
         ( _valIetm,_valIself) =
             val_
         ( _tlIetm,_tlIself) =
             tl_
     in  ( _lhsOetm,_lhsOself))
sem_Functions_Nil :: T_Functions
sem_Functions_Nil =
    (let _lhsOetm :: ETm
         _lhsOself :: Functions
         _lhsOetm =
             ({-# LINE 47 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 1945 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             Data.Map.empty
         _lhsOself =
             _self
     in  ( _lhsOetm,_lhsOself))
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
type T_Global = ( (ETm -> ETm),Global)
data Inh_Global = Inh_Global {}
data Syn_Global = Syn_Global {etm_Syn_Global :: (ETm -> ETm),self_Syn_Global :: Global}
wrap_Global :: T_Global ->
               Inh_Global ->
               Syn_Global
wrap_Global sem (Inh_Global) =
    (let ( _lhsOetm,_lhsOself) = sem
     in  (Syn_Global _lhsOetm _lhsOself))
sem_Global_GlobalVar :: T_Identifier ->
                        T_Linkage ->
                        Bool ->
                        Bool ->
                        T_Type ->
                        T_MConstant ->
                        T_Align ->
                        T_Global
sem_Global_GlobalVar name_ linkage_ isConst_ isUaddr_ ty_ ival_ align_ =
    (let _lhsOetm :: (ETm -> ETm)
         _lhsOself :: Global
         _nameIetm :: ETm
         _nameIself :: Identifier
         _linkageIself :: Linkage
         _tyIself :: Type
         _ivalIself :: MConstant
         _alignIself :: Align
         _lhsOetm =
             ({-# LINE 39 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              \t -> let v = EBot
                    in ELet (getIdentifier _nameIself) v t
              {-# LINE 2014 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             GlobalVar _nameIself _linkageIself isConst_ isUaddr_ _tyIself _ivalIself _alignIself
         _lhsOself =
             _self
         ( _nameIetm,_nameIself) =
             name_
         ( _linkageIself) =
             linkage_
         ( _tyIself) =
             ty_
         ( _ivalIself) =
             ival_
         ( _alignIself) =
             align_
     in  ( _lhsOetm,_lhsOself))
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
                                 T_Type ->
                                 T_GlobalValue
sem_GlobalValue_FunctionValue n_ ty_ =
    (let _lhsOself :: GlobalValue
         _nIetm :: ETm
         _nIself :: Identifier
         _tyIself :: Type
         _self =
             FunctionValue _nIself _tyIself
         _lhsOself =
             _self
         ( _nIetm,_nIself) =
             n_
         ( _tyIself) =
             ty_
     in  ( _lhsOself))
sem_GlobalValue_GlobalAlias :: T_Identifier ->
                               T_Type ->
                               T_GlobalValue
sem_GlobalValue_GlobalAlias n_ ty_ =
    (let _lhsOself :: GlobalValue
         _nIetm :: ETm
         _nIself :: Identifier
         _tyIself :: Type
         _self =
             GlobalAlias _nIself _tyIself
         _lhsOself =
             _self
         ( _nIetm,_nIself) =
             n_
         ( _tyIself) =
             ty_
     in  ( _lhsOself))
sem_GlobalValue_GlobalVariable :: T_Identifier ->
                                  T_Type ->
                                  T_GlobalValue
sem_GlobalValue_GlobalVariable n_ ty_ =
    (let _lhsOself :: GlobalValue
         _nIetm :: ETm
         _nIself :: Identifier
         _tyIself :: Type
         _self =
             GlobalVariable _nIself _tyIself
         _lhsOself =
             _self
         ( _nIetm,_nIself) =
             n_
         ( _tyIself) =
             ty_
     in  ( _lhsOself))
-- Globals -----------------------------------------------------
-- cata
sem_Globals :: Globals ->
               T_Globals
sem_Globals list =
    (Prelude.foldr sem_Globals_Cons sem_Globals_Nil (Prelude.map sem_Global list))
-- semantic domain
type T_Globals = ( (ETm -> ETm),Globals)
data Inh_Globals = Inh_Globals {}
data Syn_Globals = Syn_Globals {etm_Syn_Globals :: (ETm -> ETm),self_Syn_Globals :: Globals}
wrap_Globals :: T_Globals ->
                Inh_Globals ->
                Syn_Globals
wrap_Globals sem (Inh_Globals) =
    (let ( _lhsOetm,_lhsOself) = sem
     in  (Syn_Globals _lhsOetm _lhsOself))
sem_Globals_Cons :: T_Global ->
                    T_Globals ->
                    T_Globals
sem_Globals_Cons hd_ tl_ =
    (let _lhsOetm :: (ETm -> ETm)
         _lhsOself :: Globals
         _hdIetm :: (ETm -> ETm)
         _hdIself :: Global
         _tlIetm :: (ETm -> ETm)
         _tlIself :: Globals
         _lhsOetm =
             ({-# LINE 36 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              _hdIetm . _tlIetm
              {-# LINE 2131 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIetm,_hdIself) =
             hd_
         ( _tlIetm,_tlIself) =
             tl_
     in  ( _lhsOetm,_lhsOself))
sem_Globals_Nil :: T_Globals
sem_Globals_Nil =
    (let _lhsOetm :: (ETm -> ETm)
         _lhsOself :: Globals
         _lhsOetm =
             ({-# LINE 35 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              id
              {-# LINE 2149 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOetm,_lhsOself))
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
type T_Identifier = ( ETm,Identifier)
data Inh_Identifier = Inh_Identifier {}
data Syn_Identifier = Syn_Identifier {etm_Syn_Identifier :: ETm,self_Syn_Identifier :: Identifier}
wrap_Identifier :: T_Identifier ->
                   Inh_Identifier ->
                   Syn_Identifier
wrap_Identifier sem (Inh_Identifier) =
    (let ( _lhsOetm,_lhsOself) = sem
     in  (Syn_Identifier _lhsOetm _lhsOself))
sem_Identifier_Global :: T_Id ->
                         T_Identifier
sem_Identifier_Global name_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Identifier
         _nameIself :: Id
         _lhsOetm =
             ({-# LINE 155 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EVar _nameIself
              {-# LINE 2208 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             Global _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
     in  ( _lhsOetm,_lhsOself))
sem_Identifier_Local :: T_Id ->
                        T_Identifier
sem_Identifier_Local name_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Identifier
         _nameIself :: Id
         _lhsOetm =
             ({-# LINE 156 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EVar _nameIself
              {-# LINE 2226 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             Local _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
     in  ( _lhsOetm,_lhsOself))
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
         _hdIetm :: ETm
         _hdIself :: Identifier
         _tlIself :: Identifiers
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIetm,_hdIself) =
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
sem_Instruction (Ret _pc _r) =
    (sem_Instruction_Ret (sem_PC _pc) (sem_RetInst _r))
sem_Instruction (Br _pc _v _t _f) =
    (sem_Instruction_Br (sem_PC _pc) (sem_Value _v) (sem_Value _t) (sem_Value _f))
sem_Instruction (UBr _pc _d) =
    (sem_Instruction_UBr (sem_PC _pc) (sem_Value _d))
sem_Instruction (Switch _pc _elems) =
    (sem_Instruction_Switch (sem_PC _pc) (sem_IntTyValIdL _elems))
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
type T_Instruction = Label ->
                     ( (([Label], [ETm] -> ETm)),([(Ident, [(Value, Label)])]),Instruction)
data Inh_Instruction = Inh_Instruction {cbb_Inh_Instruction :: Label}
data Syn_Instruction = Syn_Instruction {etm_Syn_Instruction :: (([Label], [ETm] -> ETm)),phis_Syn_Instruction :: ([(Ident, [(Value, Label)])]),self_Syn_Instruction :: Instruction}
wrap_Instruction :: T_Instruction ->
                    Inh_Instruction ->
                    Syn_Instruction
wrap_Instruction sem (Inh_Instruction _lhsIcbb) =
    (let ( _lhsOetm,_lhsOphis,_lhsOself) = sem _lhsIcbb
     in  (Syn_Instruction _lhsOetm _lhsOphis _lhsOself))
sem_Instruction_Ret :: T_PC ->
                       T_RetInst ->
                       T_Instruction
sem_Instruction_Ret pc_ r_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _rIetm :: ETm
              _rIself :: RetInst
              _lhsOetm =
                  ({-# LINE 113 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const _rIetm)
                   {-# LINE 2415 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2420 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Ret _pcIself _rIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _rIetm,_rIself) =
                  r_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Br :: T_PC ->
                      T_Value ->
                      T_Value ->
                      T_Value ->
                      T_Instruction
sem_Instruction_Br pc_ v_ t_ f_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _vIetm :: ETm
              _vIself :: Value
              _tIetm :: ETm
              _tIself :: Value
              _fIetm :: ETm
              _fIself :: Value
              _lhsOetm =
                  ({-# LINE 114 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([getId _tIetm, getId _fIetm], \t -> EIf _vIetm (t!!0) (t!!1))
                   {-# LINE 2451 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2456 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Br _pcIself _vIself _tIself _fIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _vIetm,_vIself) =
                  v_
              ( _tIetm,_tIself) =
                  t_
              ( _fIetm,_fIself) =
                  f_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_UBr :: T_PC ->
                       T_Value ->
                       T_Instruction
sem_Instruction_UBr pc_ d_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _dIetm :: ETm
              _dIself :: Value
              _lhsOetm =
                  ({-# LINE 115 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([getId _dIetm], head)
                   {-# LINE 2485 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2490 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  UBr _pcIself _dIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _dIetm,_dIself) =
                  d_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Switch :: T_PC ->
                          T_IntTyValIdL ->
                          T_Instruction
sem_Instruction_Switch pc_ elems_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _elemsIself :: IntTyValIdL
              _lhsOetm =
                  ({-# LINE 116 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   error "Switch TODO"
                   {-# LINE 2514 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2519 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Switch _pcIself _elemsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _elemsIself) =
                  elems_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Unreachable :: T_PC ->
                               T_Instruction
sem_Instruction_Unreachable pc_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _lhsOetm =
                  ({-# LINE 117 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 2541 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2546 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Unreachable _pcIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Add :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Add pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 122 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   let f t = let i = getId _idIetm
                                 o = EApp (EApp (EVar "(+)") _op1Ietm) _op2Ietm
                             in ELet i o (head t)
                   in ([], f)
                   {-# LINE 2580 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2585 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Add _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_FAdd :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FAdd pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 2624 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2629 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  FAdd _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Sub :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Sub pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 2668 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2673 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Sub _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_FSub :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FSub pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 2712 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2717 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  FSub _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Mul :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Mul pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 126 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   let f t = let i = getId _idIetm
                                 o = EApp (EApp (EVar "(*)") _op1Ietm) _op2Ietm
                             in ELet i o (head t)
                   in ([], f)
                   {-# LINE 2759 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2764 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Mul _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_FMul :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FMul pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 2803 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2808 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  FMul _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_UDiv :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_UDiv pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 2847 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2852 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  UDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_SDiv :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_SDiv pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 2891 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2896 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  SDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_FDiv :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FDiv pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 2935 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2940 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  FDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_URem :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_URem pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 2979 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 2984 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  URem _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_SRem :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_SRem pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3023 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3028 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  SRem _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_FRem :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FRem pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3067 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3072 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  FRem _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Shl :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Shl pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3111 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3116 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Shl _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_LShr :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_LShr pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3155 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3160 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  LShr _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_AShr :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_AShr pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3199 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3204 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  AShr _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_And :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_And pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3243 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3248 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  And _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Or :: T_PC ->
                      T_Identifier ->
                      T_Type ->
                      T_Value ->
                      T_Value ->
                      T_Instruction
sem_Instruction_Or pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3287 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3292 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Or _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Xor :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Xor pc_ id_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3331 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3336 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Xor _pcIself _idIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Alloca :: T_PC ->
                          T_Identifier ->
                          T_Type ->
                          T_Align ->
                          T_Instruction
sem_Instruction_Alloca pc_ id_ ty_ align_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _alignIself :: Align
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3371 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3376 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Alloca _pcIself _idIself _tyIself _alignIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _alignIself) =
                  align_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Store :: T_PC ->
                         T_Type ->
                         T_Value ->
                         T_Value ->
                         T_Align ->
                         T_Instruction
sem_Instruction_Store pc_ ty_ v1_ v2_ align_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _tyIself :: Type
              _v1Ietm :: ETm
              _v1Iself :: Value
              _v2Ietm :: ETm
              _v2Iself :: Value
              _alignIself :: Align
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3412 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3417 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Store _pcIself _tyIself _v1Iself _v2Iself _alignIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _tyIself) =
                  ty_
              ( _v1Ietm,_v1Iself) =
                  v1_
              ( _v2Ietm,_v2Iself) =
                  v2_
              ( _alignIself) =
                  align_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Load :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Align ->
                        T_Instruction
sem_Instruction_Load pc_ id_ v_ align_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _alignIself :: Align
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3453 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3458 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Load _pcIself _idIself _vIself _alignIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _alignIself) =
                  align_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_GetElementPtr :: T_PC ->
                                 T_Identifier ->
                                 T_Type ->
                                 T_Value ->
                                 T_Values ->
                                 T_Instruction
sem_Instruction_GetElementPtr pc_ id_ ty_ struct_ idxs_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _structIetm :: ETm
              _structIself :: Value
              _idxsIetm :: ([ETm])
              _idxsIself :: Values
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3495 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3500 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  GetElementPtr _pcIself _idIself _tyIself _structIself _idxsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _structIetm,_structIself) =
                  struct_
              ( _idxsIetm,_idxsIself) =
                  idxs_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Trunc :: T_PC ->
                         T_Identifier ->
                         T_Value ->
                         T_Type ->
                         T_Instruction
sem_Instruction_Trunc pc_ id_ v_ ty_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _tyIself :: Type
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3536 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3541 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Trunc _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _tyIself) =
                  ty_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_ZExt :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Type ->
                        T_Instruction
sem_Instruction_ZExt pc_ id_ v_ ty_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _tyIself :: Type
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3575 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3580 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  ZExt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _tyIself) =
                  ty_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_SExt :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Type ->
                        T_Instruction
sem_Instruction_SExt pc_ id_ v_ ty_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _tyIself :: Type
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3614 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3619 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  SExt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _tyIself) =
                  ty_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_FPToUI :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_FPToUI pc_ id_ v_ ty_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _tyIself :: Type
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3653 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3658 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  FPToUI _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _tyIself) =
                  ty_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_FPToSI :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_FPToSI pc_ id_ v_ ty_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _tyIself :: Type
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3692 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3697 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  FPToSI _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _tyIself) =
                  ty_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_UIToFP :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_UIToFP pc_ id_ v_ ty_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _tyIself :: Type
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3731 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3736 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  UIToFP _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _tyIself) =
                  ty_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_SIToFP :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_SIToFP pc_ id_ v_ ty_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _tyIself :: Type
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3770 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3775 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  SIToFP _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _tyIself) =
                  ty_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_FPTrunc :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           T_Type ->
                           T_Instruction
sem_Instruction_FPTrunc pc_ id_ v_ ty_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _tyIself :: Type
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3809 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3814 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  FPTrunc _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _tyIself) =
                  ty_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_FPExt :: T_PC ->
                         T_Identifier ->
                         T_Value ->
                         T_Type ->
                         T_Instruction
sem_Instruction_FPExt pc_ id_ v_ ty_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _tyIself :: Type
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3848 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3853 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  FPExt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _tyIself) =
                  ty_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_PtrToInt :: T_PC ->
                            T_Identifier ->
                            T_Value ->
                            T_Type ->
                            T_Instruction
sem_Instruction_PtrToInt pc_ id_ v_ ty_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _tyIself :: Type
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3887 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3892 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  PtrToInt _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _tyIself) =
                  ty_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_IntToPtr :: T_PC ->
                            T_Identifier ->
                            T_Value ->
                            T_Type ->
                            T_Instruction
sem_Instruction_IntToPtr pc_ id_ v_ ty_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _tyIself :: Type
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3926 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3931 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  IntToPtr _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _tyIself) =
                  ty_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_BitCast :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           T_Type ->
                           T_Instruction
sem_Instruction_BitCast pc_ id_ v_ ty_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _vIetm :: ETm
              _vIself :: Value
              _tyIself :: Type
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 3965 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 3970 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  BitCast _pcIself _idIself _vIself _tyIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _vIetm,_vIself) =
                  v_
              ( _tyIself) =
                  ty_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_ICmp :: T_PC ->
                        T_Identifier ->
                        T_IntPredicate ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_ICmp pc_ id_ cond_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _condIself :: IntPredicate
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 118 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   let f t = let i = getId _idIetm
                                 o = EApp (EApp (EVar $ "ICmp" ++ show _condIself) (_op1Ietm)) _op2Ietm
                             in trace (show t) $ ELet i o $! (head t)
                   in ([], f)
                   {-# LINE 4012 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4017 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  ICmp _pcIself _idIself _condIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _condIself) =
                  cond_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_FCmp :: T_PC ->
                        T_Identifier ->
                        T_RealPredicate ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FCmp pc_ id_ cond_ ty_ op1_ op2_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _condIself :: RealPredicate
              _tyIself :: Type
              _op1Ietm :: ETm
              _op1Iself :: Value
              _op2Ietm :: ETm
              _op2Iself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4060 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4065 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  FCmp _pcIself _idIself _condIself _tyIself _op1Iself _op2Iself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _condIself) =
                  cond_
              ( _tyIself) =
                  ty_
              ( _op1Ietm,_op1Iself) =
                  op1_
              ( _op2Ietm,_op2Iself) =
                  op2_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_PHI :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_PValues ->
                       T_Instruction
sem_Instruction_PHI pc_ id_ ty_ vals_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _tyIself :: Type
              _valsIself :: PValues
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4102 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 136 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   [(getId _idIetm, Prelude.map (\(a,b) -> (a, getLabel b)) _valsIself)]
                   {-# LINE 4107 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  PHI _pcIself _idIself _tyIself _valsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _tyIself) =
                  ty_
              ( _valsIself) =
                  vals_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Call :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Identifier ->
                        T_Values ->
                        T_Instruction
sem_Instruction_Call pc_ mres_ ty_ callee_ args_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _mresIetm :: ETm
              _mresIself :: Identifier
              _tyIself :: Type
              _calleeIetm :: ETm
              _calleeIself :: Identifier
              _argsIetm :: ([ETm])
              _argsIself :: Values
              _lhsOetm =
                  ({-# LINE 130 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   let f t = let i = getId _mresIetm
                                 c = EVar $ getIdentifier _calleeIself
                                 a = _argsIetm
                             in ELet i (calletm c a) (head t)
                   in ([], f)
                   {-# LINE 4148 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4153 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Call _pcIself _mresIself _tyIself _calleeIself _argsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _mresIetm,_mresIself) =
                  mres_
              ( _tyIself) =
                  ty_
              ( _calleeIetm,_calleeIself) =
                  callee_
              ( _argsIetm,_argsIself) =
                  args_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Select :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Value ->
                          T_Value ->
                          T_Instruction
sem_Instruction_Select pc_ id_ cond_ valt_ valf_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _condIetm :: ETm
              _condIself :: Value
              _valtIetm :: ETm
              _valtIself :: Value
              _valfIetm :: ETm
              _valfIself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4193 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4198 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Select _pcIself _idIself _condIself _valtIself _valfIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _condIetm,_condIself) =
                  cond_
              ( _valtIetm,_valtIself) =
                  valt_
              ( _valfIetm,_valfIself) =
                  valf_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_ExtractValue :: T_PC ->
                                T_Identifier ->
                                T_Value ->
                                T_Ints ->
                                T_Instruction
sem_Instruction_ExtractValue pc_ id_ aggr_ idxs_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _aggrIetm :: ETm
              _aggrIself :: Value
              _idxsIself :: Ints
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4234 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4239 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  ExtractValue _pcIself _idIself _aggrIself _idxsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _aggrIetm,_aggrIself) =
                  aggr_
              ( _idxsIself) =
                  idxs_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_InsertValue :: T_PC ->
                               T_Identifier ->
                               T_Value ->
                               T_Value ->
                               T_Ints ->
                               T_Instruction
sem_Instruction_InsertValue pc_ id_ aggr_ ival_ idxs_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _aggrIetm :: ETm
              _aggrIself :: Value
              _ivalIetm :: ETm
              _ivalIself :: Value
              _idxsIself :: Ints
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4276 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4281 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  InsertValue _pcIself _idIself _aggrIself _ivalIself _idxsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _aggrIetm,_aggrIself) =
                  aggr_
              ( _ivalIetm,_ivalIself) =
                  ival_
              ( _idxsIself) =
                  idxs_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_Cmpxchg :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           T_Value ->
                           T_Value ->
                           T_AtomicOrdering ->
                           T_Instruction
sem_Instruction_Cmpxchg pc_ id_ mptr_ cval_ nval_ ord_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _mptrIetm :: ETm
              _mptrIself :: Value
              _cvalIetm :: ETm
              _cvalIself :: Value
              _nvalIetm :: ETm
              _nvalIself :: Value
              _ordIself :: AtomicOrdering
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4323 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4328 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  Cmpxchg _pcIself _idIself _mptrIself _cvalIself _nvalIself _ordIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _mptrIetm,_mptrIself) =
                  mptr_
              ( _cvalIetm,_cvalIself) =
                  cval_
              ( _nvalIetm,_nvalIself) =
                  nval_
              ( _ordIself) =
                  ord_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_AtomicRMW :: T_PC ->
                             T_Identifier ->
                             T_Values ->
                             T_BinOp ->
                             T_AtomicOrdering ->
                             T_Instruction
sem_Instruction_AtomicRMW pc_ id_ args_ op_ ord_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _idIetm :: ETm
              _idIself :: Identifier
              _argsIetm :: ([ETm])
              _argsIself :: Values
              _opIself :: BinOp
              _ordIself :: AtomicOrdering
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4368 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4373 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  AtomicRMW _pcIself _idIself _argsIself _opIself _ordIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _idIetm,_idIself) =
                  id_
              ( _argsIetm,_argsIself) =
                  args_
              ( _opIself) =
                  op_
              ( _ordIself) =
                  ord_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_CreateThread :: T_PC ->
                                T_Values ->
                                T_Instruction
sem_Instruction_CreateThread pc_ args_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _argsIetm :: ([ETm])
              _argsIself :: Values
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4404 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4409 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  CreateThread _pcIself _argsIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _argsIetm,_argsIself) =
                  args_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_MutexInit :: T_PC ->
                             T_Identifier ->
                             T_Value ->
                             T_Instruction
sem_Instruction_MutexInit pc_ rv_ mutex_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _rvIetm :: ETm
              _rvIself :: Identifier
              _mutexIetm :: ETm
              _mutexIself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4437 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4442 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  MutexInit _pcIself _rvIself _mutexIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _rvIetm,_rvIself) =
                  rv_
              ( _mutexIetm,_mutexIself) =
                  mutex_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_MutexLock :: T_PC ->
                             T_Identifier ->
                             T_Value ->
                             T_Instruction
sem_Instruction_MutexLock pc_ rv_ mutex_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _rvIetm :: ETm
              _rvIself :: Identifier
              _mutexIetm :: ETm
              _mutexIself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4472 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4477 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  MutexLock _pcIself _rvIself _mutexIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _rvIetm,_rvIself) =
                  rv_
              ( _mutexIetm,_mutexIself) =
                  mutex_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_MutexUnlock :: T_PC ->
                               T_Identifier ->
                               T_Value ->
                               T_Instruction
sem_Instruction_MutexUnlock pc_ rv_ mutex_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _rvIetm :: ETm
              _rvIself :: Identifier
              _mutexIetm :: ETm
              _mutexIself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4507 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4512 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  MutexUnlock _pcIself _rvIself _mutexIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _rvIetm,_rvIself) =
                  rv_
              ( _mutexIetm,_mutexIself) =
                  mutex_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_WaitEvent :: T_PC ->
                             Int ->
                             T_Instruction
sem_Instruction_WaitEvent pc_ event_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4537 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4542 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  WaitEvent _pcIself event_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_NotifyEvent :: T_PC ->
                               Int ->
                               T_Instruction
sem_Instruction_NotifyEvent pc_ event_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4563 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4568 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  NotifyEvent _pcIself event_
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instruction_WaitTime :: T_PC ->
                            T_Value ->
                            T_Instruction
sem_Instruction_WaitTime pc_ time_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instruction
              _pcIself :: PC
              _timeIetm :: ETm
              _timeIself :: Value
              _lhsOetm =
                  ({-# LINE 135 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   ([], const EBot)
                   {-# LINE 4591 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4596 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  WaitTime _pcIself _timeIself
              _lhsOself =
                  _self
              ( _pcIself) =
                  pc_
              ( _timeIetm,_timeIself) =
                  time_
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
-- Instructions ------------------------------------------------
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = Label ->
                      ( (([Label], [ETm] -> ETm)),([(Ident, [(Value, Label)])]),Instructions)
data Inh_Instructions = Inh_Instructions {cbb_Inh_Instructions :: Label}
data Syn_Instructions = Syn_Instructions {etm_Syn_Instructions :: (([Label], [ETm] -> ETm)),phis_Syn_Instructions :: ([(Ident, [(Value, Label)])]),self_Syn_Instructions :: Instructions}
wrap_Instructions :: T_Instructions ->
                     Inh_Instructions ->
                     Syn_Instructions
wrap_Instructions sem (Inh_Instructions _lhsIcbb) =
    (let ( _lhsOetm,_lhsOphis,_lhsOself) = sem _lhsIcbb
     in  (Syn_Instructions _lhsOetm _lhsOphis _lhsOself))
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instructions
              _hdOcbb :: Label
              _tlOcbb :: Label
              _hdIetm :: (([Label], [ETm] -> ETm))
              _hdIphis :: ([(Ident, [(Value, Label)])])
              _hdIself :: Instruction
              _tlIetm :: (([Label], [ETm] -> ETm))
              _tlIphis :: ([(Ident, [(Value, Label)])])
              _tlIself :: Instructions
              _lhsOetm =
                  ({-# LINE 105 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   let (l,f) = _hdIetm
                       (m,g) = _tlIetm
                   in case m of
                       ["-1"] -> _hdIetm
                       x      -> (l ++ m, \t -> f [g t])
                   {-# LINE 4647 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   _hdIphis ++ _tlIphis
                   {-# LINE 4652 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOcbb =
                  ({-# LINE 100 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   _lhsIcbb
                   {-# LINE 4661 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _tlOcbb =
                  ({-# LINE 100 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   _lhsIcbb
                   {-# LINE 4666 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              ( _hdIetm,_hdIphis,_hdIself) =
                  hd_ _hdOcbb
              ( _tlIetm,_tlIphis,_tlIself) =
                  tl_ _tlOcbb
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    (\ _lhsIcbb ->
         (let _lhsOetm :: (([Label], [ETm] -> ETm))
              _lhsOphis :: ([(Ident, [(Value, Label)])])
              _lhsOself :: Instructions
              _lhsOetm =
                  ({-# LINE 104 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   (["-1"], const EBot)
                   {-# LINE 4682 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _lhsOphis =
                  ({-# LINE 99 "./src/Language/LLVMIR/Converter/Module.ag" #-}
                   []
                   {-# LINE 4687 "src/Language/LLVMIR/Converter/Module.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOetm,_lhsOphis,_lhsOself)))
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
sem_IntPredicate_IntUGT :: T_IntPredicate
sem_IntPredicate_IntUGT =
    (let _lhsOself :: IntPredicate
         _self =
             IntUGT
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
sem_IntPredicate_IntULT :: T_IntPredicate
sem_IntPredicate_IntULT =
    (let _lhsOself :: IntPredicate
         _self =
             IntULT
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
sem_IntPredicate_IntSGT :: T_IntPredicate
sem_IntPredicate_IntSGT =
    (let _lhsOself :: IntPredicate
         _self =
             IntSGT
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
sem_IntPredicate_IntSLT :: T_IntPredicate
sem_IntPredicate_IntSLT =
    (let _lhsOself :: IntPredicate
         _self =
             IntSLT
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
         _x1Iself :: Type
         _x2Ietm :: ETm
         _x2Iself :: Value
         _x3Ietm :: ETm
         _x3Iself :: Identifier
         _self =
             (_x1Iself,_x2Iself,_x3Iself)
         _lhsOself =
             _self
         ( _x1Iself) =
             x1_
         ( _x2Ietm,_x2Iself) =
             x2_
         ( _x3Ietm,_x3Iself) =
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
type T_MConstant = ( MConstant)
data Inh_MConstant = Inh_MConstant {}
data Syn_MConstant = Syn_MConstant {self_Syn_MConstant :: MConstant}
wrap_MConstant :: T_MConstant ->
                  Inh_MConstant ->
                  Syn_MConstant
wrap_MConstant sem (Inh_MConstant) =
    (let ( _lhsOself) = sem
     in  (Syn_MConstant _lhsOself))
sem_MConstant_Just :: T_Constant ->
                      T_MConstant
sem_MConstant_Just just_ =
    (let _lhsOself :: MConstant
         _justIetm :: ETm
         _justIself :: Constant
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIetm,_justIself) =
             just_
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
         _justIetm :: ETm
         _justIself :: Identifier
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIetm,_justIself) =
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
         _justIetm :: ETm
         _justIself :: Value
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIetm,_justIself) =
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
type T_Module = ( HTm,Module)
data Inh_Module = Inh_Module {}
data Syn_Module = Syn_Module {htm_Syn_Module :: HTm,self_Syn_Module :: Module}
wrap_Module :: T_Module ->
               Inh_Module ->
               Syn_Module
wrap_Module sem (Inh_Module) =
    (let ( _lhsOhtm,_lhsOself) = sem
     in  (Syn_Module _lhsOhtm _lhsOself))
sem_Module_Module :: String ->
                     T_DataLayout ->
                     T_TargetData ->
                     T_Globals ->
                     T_Functions ->
                     T_NamedTypes ->
                     T_Module
sem_Module_Module id_ layout_ target_ gvars_ funs_ nmdtys_ =
    (let _lhsOhtm :: HTm
         _lhsOself :: Module
         _layoutIself :: DataLayout
         _targetIself :: TargetData
         _gvarsIetm :: (ETm -> ETm)
         _gvarsIself :: Globals
         _funsIetm :: ETm
         _funsIself :: Functions
         _nmdtysIself :: NamedTypes
         _lhsOhtm =
             ({-# LINE 28 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              HTm (_gvarsIetm _funsIetm)
              {-# LINE 5779 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             Module id_ _layoutIself _targetIself _gvarsIself _funsIself _nmdtysIself
         _lhsOself =
             _self
         ( _layoutIself) =
             layout_
         ( _targetIself) =
             target_
         ( _gvarsIetm,_gvarsIself) =
             gvars_
         ( _funsIetm,_funsIself) =
             funs_
         ( _nmdtysIself) =
             nmdtys_
     in  ( _lhsOhtm,_lhsOself))
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
         _valIself :: Type
         _tlIself :: NamedTypes
         _self =
             Data.Map.insert key_ _valIself _tlIself
         _lhsOself =
             _self
         ( _valIself) =
             val_
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
         _x1Iself :: Type
         _self =
             (_x1Iself,x2_)
         _lhsOself =
             _self
         ( _x1Iself) =
             x1_
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
         _x1Ietm :: ETm
         _x1Iself :: Value
         _x2Ietm :: ETm
         _x2Iself :: Value
         _self =
             (_x1Iself,_x2Iself)
         _lhsOself =
             _self
         ( _x1Ietm,_x1Iself) =
             x1_
         ( _x2Ietm,_x2Iself) =
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
         _x1Ietm :: ETm
         _x1Iself :: Value
         _self =
             (_x1Iself,x2_)
         _lhsOself =
             _self
         ( _x1Ietm,_x1Iself) =
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
    (sem_Parameter_Parameter (sem_Identifier _var) (sem_Type _ty))
-- semantic domain
type T_Parameter = ( ETm,Parameter)
data Inh_Parameter = Inh_Parameter {}
data Syn_Parameter = Syn_Parameter {etm_Syn_Parameter :: ETm,self_Syn_Parameter :: Parameter}
wrap_Parameter :: T_Parameter ->
                  Inh_Parameter ->
                  Syn_Parameter
wrap_Parameter sem (Inh_Parameter) =
    (let ( _lhsOetm,_lhsOself) = sem
     in  (Syn_Parameter _lhsOetm _lhsOself))
sem_Parameter_Parameter :: T_Identifier ->
                           T_Type ->
                           T_Parameter
sem_Parameter_Parameter var_ ty_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Parameter
         _varIetm :: ETm
         _varIself :: Identifier
         _tyIself :: Type
         _lhsOetm =
             ({-# LINE 70 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EVar $ getIdentifier _varIself
              {-# LINE 6128 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             Parameter _varIself _tyIself
         _lhsOself =
             _self
         ( _varIetm,_varIself) =
             var_
         ( _tyIself) =
             ty_
     in  ( _lhsOetm,_lhsOself))
-- Parameters --------------------------------------------------
-- cata
sem_Parameters :: Parameters ->
                  T_Parameters
sem_Parameters list =
    (Prelude.foldr sem_Parameters_Cons sem_Parameters_Nil (Prelude.map sem_Parameter list))
-- semantic domain
type T_Parameters = ( ([ETm]),Parameters)
data Inh_Parameters = Inh_Parameters {}
data Syn_Parameters = Syn_Parameters {etm_Syn_Parameters :: ([ETm]),self_Syn_Parameters :: Parameters}
wrap_Parameters :: T_Parameters ->
                   Inh_Parameters ->
                   Syn_Parameters
wrap_Parameters sem (Inh_Parameters) =
    (let ( _lhsOetm,_lhsOself) = sem
     in  (Syn_Parameters _lhsOetm _lhsOself))
sem_Parameters_Cons :: T_Parameter ->
                       T_Parameters ->
                       T_Parameters
sem_Parameters_Cons hd_ tl_ =
    (let _lhsOetm :: ([ETm])
         _lhsOself :: Parameters
         _hdIetm :: ETm
         _hdIself :: Parameter
         _tlIetm :: ([ETm])
         _tlIself :: Parameters
         _lhsOetm =
             ({-# LINE 64 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              _hdIetm : _tlIetm
              {-# LINE 6168 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIetm,_hdIself) =
             hd_
         ( _tlIetm,_tlIself) =
             tl_
     in  ( _lhsOetm,_lhsOself))
sem_Parameters_Nil :: T_Parameters
sem_Parameters_Nil =
    (let _lhsOetm :: ([ETm])
         _lhsOself :: Parameters
         _lhsOetm =
             ({-# LINE 64 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              []
              {-# LINE 6186 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOetm,_lhsOself))
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
type T_RetInst = ( ETm,RetInst)
data Inh_RetInst = Inh_RetInst {}
data Syn_RetInst = Syn_RetInst {etm_Syn_RetInst :: ETm,self_Syn_RetInst :: RetInst}
wrap_RetInst :: T_RetInst ->
                Inh_RetInst ->
                Syn_RetInst
wrap_RetInst sem (Inh_RetInst) =
    (let ( _lhsOetm,_lhsOself) = sem
     in  (Syn_RetInst _lhsOetm _lhsOself))
sem_RetInst_ValueRet :: T_Value ->
                        T_RetInst
sem_RetInst_ValueRet v_ =
    (let _lhsOetm :: ETm
         _lhsOself :: RetInst
         _vIetm :: ETm
         _vIself :: Value
         _lhsOetm =
             ({-# LINE 146 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              _vIetm
              {-# LINE 6395 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             ValueRet _vIself
         _lhsOself =
             _self
         ( _vIetm,_vIself) =
             v_
     in  ( _lhsOetm,_lhsOself))
sem_RetInst_VoidRet :: T_RetInst
sem_RetInst_VoidRet =
    (let _lhsOetm :: ETm
         _lhsOself :: RetInst
         _lhsOetm =
             ({-# LINE 147 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              EBot
              {-# LINE 6411 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             VoidRet
         _lhsOself =
             _self
     in  ( _lhsOetm,_lhsOself))
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
sem_Type (TyFunction _party _retty) =
    (sem_Type_TyFunction (sem_Types _party) (sem_Type _retty))
sem_Type (TyStruct _name _numEl _tys) =
    (sem_Type_TyStruct _name _numEl (sem_Types _tys))
sem_Type (TyPointer _ty) =
    (sem_Type_TyPointer (sem_Type _ty))
sem_Type (TyVector _numEl _ty) =
    (sem_Type_TyVector _numEl (sem_Type _ty))
sem_Type (TyUndefined) =
    (sem_Type_TyUndefined)
-- semantic domain
type T_Type = ( Type)
data Inh_Type = Inh_Type {}
data Syn_Type = Syn_Type {self_Syn_Type :: Type}
wrap_Type :: T_Type ->
             Inh_Type ->
             Syn_Type
wrap_Type sem (Inh_Type) =
    (let ( _lhsOself) = sem
     in  (Syn_Type _lhsOself))
sem_Type_TyVoid :: T_Type
sem_Type_TyVoid =
    (let _lhsOself :: Type
         _self =
             TyVoid
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Type_Tyx86MMX :: T_Type
sem_Type_Tyx86MMX =
    (let _lhsOself :: Type
         _self =
             Tyx86MMX
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Type_TyLabel :: T_Type
sem_Type_TyLabel =
    (let _lhsOself :: Type
         _self =
             TyLabel
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Type_TyMetadata :: T_Type
sem_Type_TyMetadata =
    (let _lhsOself :: Type
         _self =
             TyMetadata
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Type_TyOpaque :: T_Type
sem_Type_TyOpaque =
    (let _lhsOself :: Type
         _self =
             TyOpaque
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Type_TyInt :: Int ->
                  T_Type
sem_Type_TyInt p_ =
    (let _lhsOself :: Type
         _self =
             TyInt p_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Type_TyFloatPoint :: T_TyFloatPoint ->
                         T_Type
sem_Type_TyFloatPoint p_ =
    (let _lhsOself :: Type
         _pIself :: TyFloatPoint
         _self =
             TyFloatPoint _pIself
         _lhsOself =
             _self
         ( _pIself) =
             p_
     in  ( _lhsOself))
sem_Type_TyArray :: Int ->
                    T_Type ->
                    T_Type
sem_Type_TyArray numEl_ ty_ =
    (let _lhsOself :: Type
         _tyIself :: Type
         _self =
             TyArray numEl_ _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
     in  ( _lhsOself))
sem_Type_TyFunction :: T_Types ->
                       T_Type ->
                       T_Type
sem_Type_TyFunction party_ retty_ =
    (let _lhsOself :: Type
         _partyIself :: Types
         _rettyIself :: Type
         _self =
             TyFunction _partyIself _rettyIself
         _lhsOself =
             _self
         ( _partyIself) =
             party_
         ( _rettyIself) =
             retty_
     in  ( _lhsOself))
sem_Type_TyStruct :: String ->
                     Int ->
                     T_Types ->
                     T_Type
sem_Type_TyStruct name_ numEl_ tys_ =
    (let _lhsOself :: Type
         _tysIself :: Types
         _self =
             TyStruct name_ numEl_ _tysIself
         _lhsOself =
             _self
         ( _tysIself) =
             tys_
     in  ( _lhsOself))
sem_Type_TyPointer :: T_Type ->
                      T_Type
sem_Type_TyPointer ty_ =
    (let _lhsOself :: Type
         _tyIself :: Type
         _self =
             TyPointer _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
     in  ( _lhsOself))
sem_Type_TyVector :: Int ->
                     T_Type ->
                     T_Type
sem_Type_TyVector numEl_ ty_ =
    (let _lhsOself :: Type
         _tyIself :: Type
         _self =
             TyVector numEl_ _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
     in  ( _lhsOself))
sem_Type_TyUndefined :: T_Type
sem_Type_TyUndefined =
    (let _lhsOself :: Type
         _self =
             TyUndefined
         _lhsOself =
             _self
     in  ( _lhsOself))
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
         _hdIself :: Type
         _tlIself :: Types
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
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
sem_Value (Id _v _ty) =
    (sem_Value_Id (sem_Identifier _v) (sem_Type _ty))
sem_Value (Constant _c) =
    (sem_Value_Constant (sem_Constant _c))
-- semantic domain
type T_Value = ( ETm,Value)
data Inh_Value = Inh_Value {}
data Syn_Value = Syn_Value {etm_Syn_Value :: ETm,self_Syn_Value :: Value}
wrap_Value :: T_Value ->
              Inh_Value ->
              Syn_Value
wrap_Value sem (Inh_Value) =
    (let ( _lhsOetm,_lhsOself) = sem
     in  (Syn_Value _lhsOetm _lhsOself))
sem_Value_Id :: T_Identifier ->
                T_Type ->
                T_Value
sem_Value_Id v_ ty_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Value
         _vIetm :: ETm
         _vIself :: Identifier
         _tyIself :: Type
         _lhsOetm =
             ({-# LINE 150 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              _vIetm
              {-# LINE 6854 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             Id _vIself _tyIself
         _lhsOself =
             _self
         ( _vIetm,_vIself) =
             v_
         ( _tyIself) =
             ty_
     in  ( _lhsOetm,_lhsOself))
sem_Value_Constant :: T_Constant ->
                      T_Value
sem_Value_Constant c_ =
    (let _lhsOetm :: ETm
         _lhsOself :: Value
         _cIetm :: ETm
         _cIself :: Constant
         _lhsOetm =
             ({-# LINE 151 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              _cIetm
              {-# LINE 6875 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             Constant _cIself
         _lhsOself =
             _self
         ( _cIetm,_cIself) =
             c_
     in  ( _lhsOetm,_lhsOself))
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
type T_Values = ( ([ETm]),Values)
data Inh_Values = Inh_Values {}
data Syn_Values = Syn_Values {etm_Syn_Values :: ([ETm]),self_Syn_Values :: Values}
wrap_Values :: T_Values ->
               Inh_Values ->
               Syn_Values
wrap_Values sem (Inh_Values) =
    (let ( _lhsOetm,_lhsOself) = sem
     in  (Syn_Values _lhsOetm _lhsOself))
sem_Values_Cons :: T_Value ->
                   T_Values ->
                   T_Values
sem_Values_Cons hd_ tl_ =
    (let _lhsOetm :: ([ETm])
         _lhsOself :: Values
         _hdIetm :: ETm
         _hdIself :: Value
         _tlIetm :: ([ETm])
         _tlIself :: Values
         _lhsOetm =
             ({-# LINE 143 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              _hdIetm : _tlIetm
              {-# LINE 6953 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIetm,_hdIself) =
             hd_
         ( _tlIetm,_tlIself) =
             tl_
     in  ( _lhsOetm,_lhsOself))
sem_Values_Nil :: T_Values
sem_Values_Nil =
    (let _lhsOetm :: ([ETm])
         _lhsOself :: Values
         _lhsOetm =
             ({-# LINE 143 "./src/Language/LLVMIR/Converter/Module.ag" #-}
              []
              {-# LINE 6971 "src/Language/LLVMIR/Converter/Module.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOetm,_lhsOself))
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