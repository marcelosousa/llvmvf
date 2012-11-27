

{-#LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- UUAGC 0.9.40.3 (src/Concurrent/Model/Visualizer.ag)
module Concurrent.Model.Visualizer where

{-# LINE 16 "src/Concurrent/Model/Visualizer.ag" #-}

import qualified Data.Map as Map
import qualified Data.IntMap as IM
import Data.Maybe 

import UU.PPrint as P
import Concurrent.Model
import Concurrent.Model.Analysis.ControlFlow (ControlFlow(..))
import Language.LLVMIR
{-# LINE 18 "src/Concurrent/Model/Visualizer.hs" #-}

{-# LINE 14 "src/Language/LLVMIR/Printer/Module.ag" #-}

import UU.PPrint as P
import Language.LLVMIR
{-# LINE 24 "src/Concurrent/Model/Visualizer.hs" #-}

{-# LINE 11 "src/Language/LLVMIR/Grammar/Base.ag" #-}

import Prelude              hiding (sequence)
import Data.Char            (chr)
import qualified Data.Map as Data.Map
import qualified Data.Map as Map
import Data.Map hiding (foldr)
{-# LINE 33 "src/Concurrent/Model/Visualizer.hs" #-}
{-# LINE 5 "src/Concurrent/Model/Visualizer.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Visualizer
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 40 "src/Concurrent/Model/Visualizer.hs" #-}

{-# LINE 79 "src/Concurrent/Model/Visualizer.ag" #-}

type ILabel = Int

nextUnique :: Int -> (Int, Int)
nextUnique u = (u+1, u)

dotLabel :: Int -> Doc -> Doc
dotLabel i d = text "[label=\"" <> int i <+> d <> text "\"];"

instance (Pretty t, SCModel t) => Pretty (Model t)  where
  --pretty :: Model -> Doc
  pretty mdl = text "digraph structs {" </>
               text "bgcolor=transparent;" </>
               text "node [shape=Mrecord];" </>
               text "-1 [label=\"||\", shape=Mdiamond];" </>
               pretty (mainf mdl) </>
               vsep (Prelude.map pretty (IM.elems $ procs mdl)) </>
               pretty' (controlflow mdl) </>
               text "}"

instance Pretty Process where
  --pretty :: Process -> Doc
  pretty (Process i f) = ppccfg_Syn_Function $ wrap_Function (sem_Function f) $ Inh_Function { ilabel_Inh_Function = 0 }

class Pretty' t where
  pretty' :: t -> Doc

instance Pretty' ControlFlow where
  pretty' (ControlFlow _ cfg) = vsep $ Prelude.map pretty' $ concat $ Map.elems cfg

instance Pretty' (Int,Int) where
  pretty' (a,b) = int a <+> text "->" <+> int b <> char ';'
 
{-# LINE 76 "src/Concurrent/Model/Visualizer.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Printer/Module.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Printer
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 84 "src/Concurrent/Model/Visualizer.hs" #-}

{-# LINE 135 "src/Language/LLVMIR/Printer/Module.ag" #-}

pConvOp :: Doc -> String -> Doc -> Doc -> Doc
pConvOp id c v ty = id <+> char '=' <+> text c <+> v <+> text "to" <+> ty

pBinOp :: Doc -> Doc -> String -> Doc -> Doc -> Doc
pBinOp ty id c op1 op2 = ty <+> id <+> char '=' <+> text c <+> op1 <> char ',' <+> op2

pBitBinOp :: Doc -> Doc -> String -> Doc -> Doc -> Doc
pBitBinOp ty id c op1 op2 = id <+> char '=' <+> text c <+> ty <+> op1 <> char ',' <+> op2

{-# LINE 97 "src/Concurrent/Model/Visualizer.hs" #-}

{-# LINE 306 "src/Language/LLVMIR/Printer/Module.ag" #-}

ppPName :: String -> Doc
ppPName s = if (take 2 s == "0x")
            then P.empty
            else space <> char '%' <> text s

escaped :: String -> Doc
escaped ('\n':xs) = text "\\0A\\00" <> escaped xs
escaped (c:xs)    = char c <> escaped xs
escaped []        = P.empty

ppKeyword :: Bool -> String -> Doc
ppKeyword True s = text s
ppKeyword _    _ = P.empty
{-# LINE 114 "src/Concurrent/Model/Visualizer.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Grammar/Base.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Base
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 122 "src/Concurrent/Model/Visualizer.hs" #-}

{-# LINE 146 "src/Language/LLVMIR/Grammar/Base.ag" #-}

emptyFunction :: Function
emptyFunction = FunctionDef "undefined" ExternalLinkage TyVoid [] []
{-# LINE 128 "src/Concurrent/Model/Visualizer.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Grammar/Type.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Type
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 136 "src/Concurrent/Model/Visualizer.hs" #-}
-- Alias -------------------------------------------------------
-- cata
sem_Alias :: Alias ->
             T_Alias
sem_Alias (Alias _name) =
    (sem_Alias_Alias (sem_Id _name))
-- semantic domain
type T_Alias = ( Doc,Alias)
data Inh_Alias = Inh_Alias {}
data Syn_Alias = Syn_Alias {pp_Syn_Alias :: Doc,self_Syn_Alias :: Alias}
wrap_Alias :: T_Alias ->
              Inh_Alias ->
              Syn_Alias
wrap_Alias sem (Inh_Alias) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Alias _lhsOpp _lhsOself))
sem_Alias_Alias :: T_Id ->
                   T_Alias
sem_Alias_Alias name_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Alias
         _nameIself :: Id
         _lhsOpp =
             ({-# LINE 148 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "%" <> text (show _nameIself)
              {-# LINE 162 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Alias _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
     in  ( _lhsOpp,_lhsOself))
-- Aliases -----------------------------------------------------
-- cata
sem_Aliases :: Aliases ->
               T_Aliases
sem_Aliases list =
    (Prelude.foldr sem_Aliases_Cons sem_Aliases_Nil (Prelude.map sem_Alias list))
-- semantic domain
type T_Aliases = ( Doc,Aliases)
data Inh_Aliases = Inh_Aliases {}
data Syn_Aliases = Syn_Aliases {pp_Syn_Aliases :: Doc,self_Syn_Aliases :: Aliases}
wrap_Aliases :: T_Aliases ->
                Inh_Aliases ->
                Syn_Aliases
wrap_Aliases sem (Inh_Aliases) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Aliases _lhsOpp _lhsOself))
sem_Aliases_Cons :: T_Alias ->
                    T_Aliases ->
                    T_Aliases
sem_Aliases_Cons hd_ tl_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Aliases
         _hdIpp :: Doc
         _hdIself :: Alias
         _tlIpp :: Doc
         _tlIself :: Aliases
         _lhsOpp =
             ({-# LINE 37 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _hdIpp <$> _tlIpp
              {-# LINE 200 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIpp,_hdIself) =
             hd_
         ( _tlIpp,_tlIself) =
             tl_
     in  ( _lhsOpp,_lhsOself))
sem_Aliases_Nil :: T_Aliases
sem_Aliases_Nil =
    (let _lhsOpp :: Doc
         _lhsOself :: Aliases
         _lhsOpp =
             ({-# LINE 37 "src/Language/LLVMIR/Printer/Module.ag" #-}
              P.empty
              {-# LINE 218 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Align -------------------------------------------------------
-- cata
sem_Align :: Align ->
             T_Align
sem_Align (Align _n) =
    (sem_Align_Align _n)
-- semantic domain
type T_Align = ( Doc,Align)
data Inh_Align = Inh_Align {}
data Syn_Align = Syn_Align {pp_Syn_Align :: Doc,self_Syn_Align :: Align}
wrap_Align :: T_Align ->
              Inh_Align ->
              Syn_Align
wrap_Align sem (Inh_Align) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Align _lhsOpp _lhsOself))
sem_Align_Align :: Int ->
                   T_Align
sem_Align_Align n_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Align
         _lhsOpp =
             ({-# LINE 230 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "align" <+> text (show n_)
              {-# LINE 249 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Align n_
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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
         _argIpp :: Doc
         _argIself :: Value
         _self =
             Argument _argIself
         _lhsOself =
             _self
         ( _argIpp,_argIself) =
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
type T_AtomicOrdering = ( Doc,AtomicOrdering)
data Inh_AtomicOrdering = Inh_AtomicOrdering {}
data Syn_AtomicOrdering = Syn_AtomicOrdering {pp_Syn_AtomicOrdering :: Doc,self_Syn_AtomicOrdering :: AtomicOrdering}
wrap_AtomicOrdering :: T_AtomicOrdering ->
                       Inh_AtomicOrdering ->
                       Syn_AtomicOrdering
wrap_AtomicOrdering sem (Inh_AtomicOrdering) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_AtomicOrdering _lhsOpp _lhsOself))
sem_AtomicOrdering_Acquire :: T_AtomicOrdering
sem_AtomicOrdering_Acquire =
    (let _lhsOpp :: Doc
         _lhsOself :: AtomicOrdering
         _lhsOpp =
             ({-# LINE 175 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "acquire"
              {-# LINE 360 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Acquire
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_AtomicOrdering_AcquireRelease :: T_AtomicOrdering
sem_AtomicOrdering_AcquireRelease =
    (let _lhsOpp :: Doc
         _lhsOself :: AtomicOrdering
         _lhsOpp =
             ({-# LINE 177 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "acq_rel"
              {-# LINE 374 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             AcquireRelease
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_AtomicOrdering_Monotonic :: T_AtomicOrdering
sem_AtomicOrdering_Monotonic =
    (let _lhsOpp :: Doc
         _lhsOself :: AtomicOrdering
         _lhsOpp =
             ({-# LINE 174 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "monotonic"
              {-# LINE 388 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Monotonic
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_AtomicOrdering_NotAtomic :: T_AtomicOrdering
sem_AtomicOrdering_NotAtomic =
    (let _lhsOpp :: Doc
         _lhsOself :: AtomicOrdering
         _lhsOpp =
             ({-# LINE 172 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "non_atomic"
              {-# LINE 402 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             NotAtomic
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_AtomicOrdering_Release :: T_AtomicOrdering
sem_AtomicOrdering_Release =
    (let _lhsOpp :: Doc
         _lhsOself :: AtomicOrdering
         _lhsOpp =
             ({-# LINE 176 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "release"
              {-# LINE 416 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Release
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_AtomicOrdering_SequentiallyConsistent :: T_AtomicOrdering
sem_AtomicOrdering_SequentiallyConsistent =
    (let _lhsOpp :: Doc
         _lhsOself :: AtomicOrdering
         _lhsOpp =
             ({-# LINE 178 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "seq_cst"
              {-# LINE 430 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             SequentiallyConsistent
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_AtomicOrdering_Unordered :: T_AtomicOrdering
sem_AtomicOrdering_Unordered =
    (let _lhsOpp :: Doc
         _lhsOself :: AtomicOrdering
         _lhsOpp =
             ({-# LINE 173 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "unordered"
              {-# LINE 444 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Unordered
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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
type T_BasicBlock = String ->
                    ILabel ->
                    ( ILabel,Doc,Doc,BasicBlock)
data Inh_BasicBlock = Inh_BasicBlock {fname_Inh_BasicBlock :: String,ilabel_Inh_BasicBlock :: ILabel}
data Syn_BasicBlock = Syn_BasicBlock {ilabel_Syn_BasicBlock :: ILabel,pp_Syn_BasicBlock :: Doc,ppccfg_Syn_BasicBlock :: Doc,self_Syn_BasicBlock :: BasicBlock}
wrap_BasicBlock :: T_BasicBlock ->
                   Inh_BasicBlock ->
                   Syn_BasicBlock
wrap_BasicBlock sem (Inh_BasicBlock _lhsIfname _lhsIilabel) =
    (let ( _lhsOilabel,_lhsOpp,_lhsOppccfg,_lhsOself) = sem _lhsIfname _lhsIilabel
     in  (Syn_BasicBlock _lhsOilabel _lhsOpp _lhsOppccfg _lhsOself))
sem_BasicBlock_BasicBlock :: T_Label ->
                             T_Instructions ->
                             T_BasicBlock
sem_BasicBlock_BasicBlock label_ instrs_ =
    (\ _lhsIfname
       _lhsIilabel ->
         (let _lhsOppccfg :: Doc
              _lhsOpp :: Doc
              _lhsOilabel :: ILabel
              _ilabel :: ILabel
              _lhsOself :: BasicBlock
              _labelIself :: Label
              _instrsIpp :: Doc
              _instrsIppccfg :: Doc
              _instrsIself :: Instructions
              _lhsOppccfg =
                  ({-# LINE 53 "src/Concurrent/Model/Visualizer.ag" #-}
                   text "subgraph cluster_" <> text _lhsIfname <> int _ilabel <+> text "{" </>
                   text "style=filled;" </>
                   text "color=gray;" </>
                   _instrsIppccfg </>
                   text "label = \"basic block" <+> text _labelIself <> text "\"; }"
                   {-# LINE 745 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 72 "src/Language/LLVMIR/Printer/Module.ag" #-}
                   text "; <label>:" <> text _labelIself <$> _instrsIpp
                   {-# LINE 750 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              (_lhsOilabel,_ilabel) =
                  ({-# LINE 58 "src/Concurrent/Model/Visualizer.ag" #-}
                   let __cont = _lhsIilabel in seq __cont ( case nextUnique __cont of { (__cont, ilabel) -> (__cont,ilabel)} )
                   {-# LINE 755 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _self =
                  BasicBlock _labelIself _instrsIself
              _lhsOself =
                  _self
              ( _labelIself) =
                  label_
              ( _instrsIpp,_instrsIppccfg,_instrsIself) =
                  instrs_
          in  ( _lhsOilabel,_lhsOpp,_lhsOppccfg,_lhsOself)))
-- BasicBlocks -------------------------------------------------
-- cata
sem_BasicBlocks :: BasicBlocks ->
                   T_BasicBlocks
sem_BasicBlocks list =
    (Prelude.foldr sem_BasicBlocks_Cons sem_BasicBlocks_Nil (Prelude.map sem_BasicBlock list))
-- semantic domain
type T_BasicBlocks = String ->
                     ILabel ->
                     ( ILabel,Doc,Doc,BasicBlocks)
data Inh_BasicBlocks = Inh_BasicBlocks {fname_Inh_BasicBlocks :: String,ilabel_Inh_BasicBlocks :: ILabel}
data Syn_BasicBlocks = Syn_BasicBlocks {ilabel_Syn_BasicBlocks :: ILabel,pp_Syn_BasicBlocks :: Doc,ppccfg_Syn_BasicBlocks :: Doc,self_Syn_BasicBlocks :: BasicBlocks}
wrap_BasicBlocks :: T_BasicBlocks ->
                    Inh_BasicBlocks ->
                    Syn_BasicBlocks
wrap_BasicBlocks sem (Inh_BasicBlocks _lhsIfname _lhsIilabel) =
    (let ( _lhsOilabel,_lhsOpp,_lhsOppccfg,_lhsOself) = sem _lhsIfname _lhsIilabel
     in  (Syn_BasicBlocks _lhsOilabel _lhsOpp _lhsOppccfg _lhsOself))
sem_BasicBlocks_Cons :: T_BasicBlock ->
                        T_BasicBlocks ->
                        T_BasicBlocks
sem_BasicBlocks_Cons hd_ tl_ =
    (\ _lhsIfname
       _lhsIilabel ->
         (let _lhsOpp :: Doc
              _lhsOppccfg :: Doc
              _lhsOself :: BasicBlocks
              _lhsOilabel :: ILabel
              _hdOfname :: String
              _hdOilabel :: ILabel
              _tlOfname :: String
              _tlOilabel :: ILabel
              _hdIilabel :: ILabel
              _hdIpp :: Doc
              _hdIppccfg :: Doc
              _hdIself :: BasicBlock
              _tlIilabel :: ILabel
              _tlIpp :: Doc
              _tlIppccfg :: Doc
              _tlIself :: BasicBlocks
              _lhsOpp =
                  ({-# LINE 37 "src/Language/LLVMIR/Printer/Module.ag" #-}
                   _hdIpp <$> _tlIpp
                   {-# LINE 809 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _lhsOppccfg =
                  ({-# LINE 31 "src/Concurrent/Model/Visualizer.ag" #-}
                   _hdIppccfg <$> _tlIppccfg
                   {-# LINE 814 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOilabel =
                  ({-# LINE 28 "src/Concurrent/Model/Visualizer.ag" #-}
                   _tlIilabel
                   {-# LINE 823 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _hdOfname =
                  ({-# LINE 34 "src/Concurrent/Model/Visualizer.ag" #-}
                   _lhsIfname
                   {-# LINE 828 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _hdOilabel =
                  ({-# LINE 28 "src/Concurrent/Model/Visualizer.ag" #-}
                   _lhsIilabel
                   {-# LINE 833 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _tlOfname =
                  ({-# LINE 34 "src/Concurrent/Model/Visualizer.ag" #-}
                   _lhsIfname
                   {-# LINE 838 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _tlOilabel =
                  ({-# LINE 28 "src/Concurrent/Model/Visualizer.ag" #-}
                   _hdIilabel
                   {-# LINE 843 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              ( _hdIilabel,_hdIpp,_hdIppccfg,_hdIself) =
                  hd_ _hdOfname _hdOilabel
              ( _tlIilabel,_tlIpp,_tlIppccfg,_tlIself) =
                  tl_ _tlOfname _tlOilabel
          in  ( _lhsOilabel,_lhsOpp,_lhsOppccfg,_lhsOself)))
sem_BasicBlocks_Nil :: T_BasicBlocks
sem_BasicBlocks_Nil =
    (\ _lhsIfname
       _lhsIilabel ->
         (let _lhsOpp :: Doc
              _lhsOppccfg :: Doc
              _lhsOself :: BasicBlocks
              _lhsOilabel :: ILabel
              _lhsOpp =
                  ({-# LINE 37 "src/Language/LLVMIR/Printer/Module.ag" #-}
                   P.empty
                   {-# LINE 861 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _lhsOppccfg =
                  ({-# LINE 31 "src/Concurrent/Model/Visualizer.ag" #-}
                   P.empty
                   {-# LINE 866 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOilabel =
                  ({-# LINE 28 "src/Concurrent/Model/Visualizer.ag" #-}
                   _lhsIilabel
                   {-# LINE 875 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
          in  ( _lhsOilabel,_lhsOpp,_lhsOppccfg,_lhsOself)))
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
type T_BinOp = ( Doc,BinOp)
data Inh_BinOp = Inh_BinOp {}
data Syn_BinOp = Syn_BinOp {pp_Syn_BinOp :: Doc,self_Syn_BinOp :: BinOp}
wrap_BinOp :: T_BinOp ->
              Inh_BinOp ->
              Syn_BinOp
wrap_BinOp sem (Inh_BinOp) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_BinOp _lhsOpp _lhsOself))
sem_BinOp_OpAdd :: T_BinOp
sem_BinOp_OpAdd =
    (let _lhsOpp :: Doc
         _lhsOself :: BinOp
         _lhsOpp =
             ({-# LINE 160 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "add"
              {-# LINE 921 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             OpAdd
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_BinOp_OpAnd :: T_BinOp
sem_BinOp_OpAnd =
    (let _lhsOpp :: Doc
         _lhsOself :: BinOp
         _lhsOpp =
             ({-# LINE 162 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "and"
              {-# LINE 935 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             OpAnd
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_BinOp_OpMax :: T_BinOp
sem_BinOp_OpMax =
    (let _lhsOpp :: Doc
         _lhsOself :: BinOp
         _lhsOpp =
             ({-# LINE 166 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "max"
              {-# LINE 949 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             OpMax
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_BinOp_OpMin :: T_BinOp
sem_BinOp_OpMin =
    (let _lhsOpp :: Doc
         _lhsOself :: BinOp
         _lhsOpp =
             ({-# LINE 167 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "min"
              {-# LINE 963 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             OpMin
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_BinOp_OpNand :: T_BinOp
sem_BinOp_OpNand =
    (let _lhsOpp :: Doc
         _lhsOself :: BinOp
         _lhsOpp =
             ({-# LINE 163 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "nand"
              {-# LINE 977 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             OpNand
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_BinOp_OpOr :: T_BinOp
sem_BinOp_OpOr =
    (let _lhsOpp :: Doc
         _lhsOself :: BinOp
         _lhsOpp =
             ({-# LINE 164 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "or"
              {-# LINE 991 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             OpOr
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_BinOp_OpSub :: T_BinOp
sem_BinOp_OpSub =
    (let _lhsOpp :: Doc
         _lhsOself :: BinOp
         _lhsOpp =
             ({-# LINE 161 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "sub"
              {-# LINE 1005 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             OpSub
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_BinOp_OpUMax :: T_BinOp
sem_BinOp_OpUMax =
    (let _lhsOpp :: Doc
         _lhsOself :: BinOp
         _lhsOpp =
             ({-# LINE 168 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "umax"
              {-# LINE 1019 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             OpUMax
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_BinOp_OpUMin :: T_BinOp
sem_BinOp_OpUMin =
    (let _lhsOpp :: Doc
         _lhsOself :: BinOp
         _lhsOpp =
             ({-# LINE 169 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "umin"
              {-# LINE 1033 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             OpUMin
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_BinOp_OpXchg :: T_BinOp
sem_BinOp_OpXchg =
    (let _lhsOpp :: Doc
         _lhsOself :: BinOp
         _lhsOpp =
             ({-# LINE 159 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "xchg"
              {-# LINE 1047 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             OpXchg
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_BinOp_OpXor :: T_BinOp
sem_BinOp_OpXor =
    (let _lhsOpp :: Doc
         _lhsOself :: BinOp
         _lhsOpp =
             ({-# LINE 165 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "xor"
              {-# LINE 1061 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             OpXor
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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
type T_CompareConstantExpr = ( Doc,CompareConstantExpr)
data Inh_CompareConstantExpr = Inh_CompareConstantExpr {}
data Syn_CompareConstantExpr = Syn_CompareConstantExpr {pp_Syn_CompareConstantExpr :: Doc,self_Syn_CompareConstantExpr :: CompareConstantExpr}
wrap_CompareConstantExpr :: T_CompareConstantExpr ->
                            Inh_CompareConstantExpr ->
                            Syn_CompareConstantExpr
wrap_CompareConstantExpr sem (Inh_CompareConstantExpr) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_CompareConstantExpr _lhsOpp _lhsOself))
sem_CompareConstantExpr_FCmpExpr :: T_RealPredicate ->
                                    T_Type ->
                                    T_Value ->
                                    T_Value ->
                                    T_CompareConstantExpr
sem_CompareConstantExpr_FCmpExpr cond_ ty_ op1_ op2_ =
    (let _lhsOpp :: Doc
         _lhsOself :: CompareConstantExpr
         _condIpp :: Doc
         _condIself :: RealPredicate
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOpp =
             ({-# LINE 276 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "icmp"   <+> _condIpp <+> _op1Ipp <> char ',' <+> _op2Ipp
              {-# LINE 1170 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             FCmpExpr _condIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _condIpp,_condIself) =
             cond_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOself))
sem_CompareConstantExpr_ICmpExpr :: T_IntPredicate ->
                                    T_Type ->
                                    T_Value ->
                                    T_Value ->
                                    T_CompareConstantExpr
sem_CompareConstantExpr_ICmpExpr cond_ ty_ op1_ op2_ =
    (let _lhsOpp :: Doc
         _lhsOself :: CompareConstantExpr
         _condIpp :: Doc
         _condIself :: IntPredicate
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOpp =
             ({-# LINE 275 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "icmp"   <+> _condIpp <+> _op1Ipp <> char ',' <+> _op2Ipp
              {-# LINE 1204 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ICmpExpr _condIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _condIpp,_condIself) =
             cond_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOself))
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
type T_Constant = ( Doc,Constant)
data Inh_Constant = Inh_Constant {}
data Syn_Constant = Syn_Constant {pp_Syn_Constant :: Doc,self_Syn_Constant :: Constant}
wrap_Constant :: T_Constant ->
                 Inh_Constant ->
                 Syn_Constant
wrap_Constant sem (Inh_Constant) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Constant _lhsOpp _lhsOself))
sem_Constant_BlockAddr :: T_Constant
sem_Constant_BlockAddr =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _lhsOpp =
             ({-# LINE 241 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "blockaddr"
              {-# LINE 1264 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             BlockAddr
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Constant_ConstantAggregateZero :: T_Type ->
                                      T_Constant
sem_Constant_ConstantAggregateZero ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 242 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> text "zeroinitializer"
              {-# LINE 1281 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantAggregateZero _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_ConstantArray :: T_Type ->
                              T_Values ->
                              T_Constant
sem_Constant_ConstantArray ty_ vals_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _tyIpp :: Doc
         _tyIself :: Type
         _valsIpp :: Doc
         _valsIself :: Values
         _lhsOpp =
             ({-# LINE 243 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> brackets _valsIpp
              {-# LINE 1303 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantArray _tyIself _valsIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
         ( _valsIpp,_valsIself) =
             vals_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_ConstantDataSequential :: T_ConstantDataSequential ->
                                       T_Constant
sem_Constant_ConstantDataSequential cds_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _cdsIpp :: Doc
         _cdsIself :: ConstantDataSequential
         _lhsOpp =
             ({-# LINE 244 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _cdsIpp
              {-# LINE 1324 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantDataSequential _cdsIself
         _lhsOself =
             _self
         ( _cdsIpp,_cdsIself) =
             cds_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_ConstantExpr :: T_ConstantExpr ->
                             T_Constant
sem_Constant_ConstantExpr expr_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _exprIpp :: Doc
         _exprIself :: ConstantExpr
         _lhsOpp =
             ({-# LINE 245 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _exprIpp
              {-# LINE 1343 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantExpr _exprIself
         _lhsOself =
             _self
         ( _exprIpp,_exprIself) =
             expr_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_ConstantFP :: T_ConstantFP ->
                           T_Constant
sem_Constant_ConstantFP fp_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _fpIpp :: Doc
         _fpIself :: ConstantFP
         _lhsOpp =
             ({-# LINE 246 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _fpIpp
              {-# LINE 1362 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantFP _fpIself
         _lhsOself =
             _self
         ( _fpIpp,_fpIself) =
             fp_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_ConstantInt :: Int ->
                            T_Type ->
                            T_Constant
sem_Constant_ConstantInt iv_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 247 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> int   iv_
              {-# LINE 1382 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantInt iv_ _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_ConstantPointerNull :: T_Type ->
                                    T_Constant
sem_Constant_ConstantPointerNull ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 248 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> text "null"
              {-# LINE 1401 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantPointerNull _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_ConstantStruct :: T_Type ->
                               T_Values ->
                               T_Constant
sem_Constant_ConstantStruct ty_ vals_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _tyIpp :: Doc
         _tyIself :: Type
         _valsIpp :: Doc
         _valsIself :: Values
         _lhsOpp =
             ({-# LINE 249 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> braces _valsIpp
              {-# LINE 1423 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantStruct _tyIself _valsIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
         ( _valsIpp,_valsIself) =
             vals_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_ConstantVector :: T_Constant
sem_Constant_ConstantVector =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _lhsOpp =
             ({-# LINE 250 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ConstantVector"
              {-# LINE 1441 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantVector
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Constant_GlobalValue :: T_GlobalValue ->
                            T_Constant
sem_Constant_GlobalValue gv_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _gvIpp :: Doc
         _gvIself :: GlobalValue
         _lhsOpp =
             ({-# LINE 251 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _gvIpp
              {-# LINE 1458 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             GlobalValue _gvIself
         _lhsOself =
             _self
         ( _gvIpp,_gvIself) =
             gv_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_UndefValue :: T_Constant
sem_Constant_UndefValue =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _lhsOpp =
             ({-# LINE 252 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "undef"
              {-# LINE 1474 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             UndefValue
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- ConstantDataSequential --------------------------------------
-- cata
sem_ConstantDataSequential :: ConstantDataSequential ->
                              T_ConstantDataSequential
sem_ConstantDataSequential (ConstantDataArray _ty _val) =
    (sem_ConstantDataSequential_ConstantDataArray (sem_Type _ty) _val)
sem_ConstantDataSequential (ConstantDataVector _ty _val) =
    (sem_ConstantDataSequential_ConstantDataVector (sem_Type _ty) _val)
-- semantic domain
type T_ConstantDataSequential = ( Doc,ConstantDataSequential)
data Inh_ConstantDataSequential = Inh_ConstantDataSequential {}
data Syn_ConstantDataSequential = Syn_ConstantDataSequential {pp_Syn_ConstantDataSequential :: Doc,self_Syn_ConstantDataSequential :: ConstantDataSequential}
wrap_ConstantDataSequential :: T_ConstantDataSequential ->
                               Inh_ConstantDataSequential ->
                               Syn_ConstantDataSequential
wrap_ConstantDataSequential sem (Inh_ConstantDataSequential) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_ConstantDataSequential _lhsOpp _lhsOself))
sem_ConstantDataSequential_ConstantDataArray :: T_Type ->
                                                String ->
                                                T_ConstantDataSequential
sem_ConstantDataSequential_ConstantDataArray ty_ val_ =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantDataSequential
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 259 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> text "c" <> dquotes (escaped val_)
              {-# LINE 1510 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantDataArray _tyIself val_
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_ConstantDataSequential_ConstantDataVector :: T_Type ->
                                                 String ->
                                                 T_ConstantDataSequential
sem_ConstantDataSequential_ConstantDataVector ty_ val_ =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantDataSequential
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 260 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> text "c" <> dquotes (escaped val_)
              {-# LINE 1530 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantDataVector _tyIself val_
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
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
type T_ConstantExpr = ( Doc,ConstantExpr)
data Inh_ConstantExpr = Inh_ConstantExpr {}
data Syn_ConstantExpr = Syn_ConstantExpr {pp_Syn_ConstantExpr :: Doc,self_Syn_ConstantExpr :: ConstantExpr}
wrap_ConstantExpr :: T_ConstantExpr ->
                     Inh_ConstantExpr ->
                     Syn_ConstantExpr
wrap_ConstantExpr sem (Inh_ConstantExpr) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_ConstantExpr _lhsOpp _lhsOself))
sem_ConstantExpr_BinaryConstantExpr :: T_ConstantExpr
sem_ConstantExpr_BinaryConstantExpr =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantExpr
         _lhsOpp =
             ({-# LINE 263 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "BinaryConstantExpr"
              {-# LINE 1580 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             BinaryConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_ConstantExpr_CompareConstantExpr :: T_CompareConstantExpr ->
                                        T_ConstantExpr
sem_ConstantExpr_CompareConstantExpr cmpExpr_ =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantExpr
         _cmpExprIpp :: Doc
         _cmpExprIself :: CompareConstantExpr
         _lhsOpp =
             ({-# LINE 264 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _cmpExprIpp
              {-# LINE 1597 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             CompareConstantExpr _cmpExprIself
         _lhsOself =
             _self
         ( _cmpExprIpp,_cmpExprIself) =
             cmpExpr_
     in  ( _lhsOpp,_lhsOself))
sem_ConstantExpr_ExtractElementConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ExtractElementConstantExpr =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantExpr
         _lhsOpp =
             ({-# LINE 265 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ExtractElementConstantExpr"
              {-# LINE 1613 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ExtractElementConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_ConstantExpr_ExtractValueConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ExtractValueConstantExpr =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantExpr
         _lhsOpp =
             ({-# LINE 266 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ExtractValueConstantExpr"
              {-# LINE 1627 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ExtractValueConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_ConstantExpr_GetElementPtrConstantExpr :: T_Value ->
                                              T_Values ->
                                              T_ConstantExpr
sem_ConstantExpr_GetElementPtrConstantExpr struct_ idxs_ =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantExpr
         _structIpp :: Doc
         _structIself :: Value
         _idxsIpp :: Doc
         _idxsIself :: Values
         _lhsOpp =
             ({-# LINE 267 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "getelementptr" <> parens (_structIpp <> char ',' <+> _idxsIpp)
              {-# LINE 1647 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             GetElementPtrConstantExpr _structIself _idxsIself
         _lhsOself =
             _self
         ( _structIpp,_structIself) =
             struct_
         ( _idxsIpp,_idxsIself) =
             idxs_
     in  ( _lhsOpp,_lhsOself))
sem_ConstantExpr_InsertElementConstantExpr :: T_ConstantExpr
sem_ConstantExpr_InsertElementConstantExpr =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantExpr
         _lhsOpp =
             ({-# LINE 268 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "InsertElementConstantExpr"
              {-# LINE 1665 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             InsertElementConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_ConstantExpr_InsertValueConstantExpr :: T_ConstantExpr
sem_ConstantExpr_InsertValueConstantExpr =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantExpr
         _lhsOpp =
             ({-# LINE 269 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "InsertValueConstantExpr"
              {-# LINE 1679 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             InsertValueConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_ConstantExpr_SelectConstantExpr :: T_ConstantExpr
sem_ConstantExpr_SelectConstantExpr =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantExpr
         _lhsOpp =
             ({-# LINE 270 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "SelectConstantExpr"
              {-# LINE 1693 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             SelectConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_ConstantExpr_ShuffleVectorConstantExpr :: T_ConstantExpr
sem_ConstantExpr_ShuffleVectorConstantExpr =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantExpr
         _lhsOpp =
             ({-# LINE 271 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ShuffleVectorConstantExpr"
              {-# LINE 1707 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ShuffleVectorConstantExpr
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_ConstantExpr_UnaryConstantExpr :: String ->
                                      Int ->
                                      T_Value ->
                                      T_Type ->
                                      T_ConstantExpr
sem_ConstantExpr_UnaryConstantExpr name_ op_ val_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantExpr
         _valIpp :: Doc
         _valIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 272 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> text name_ <+> _valIpp
              {-# LINE 1729 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             UnaryConstantExpr name_ op_ _valIself _tyIself
         _lhsOself =
             _self
         ( _valIpp,_valIself) =
             val_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
-- ConstantFP --------------------------------------------------
-- cata
sem_ConstantFP :: ConstantFP ->
                  T_ConstantFP
sem_ConstantFP (ConstantFPDouble _dbv _ty) =
    (sem_ConstantFP_ConstantFPDouble _dbv (sem_Type _ty))
sem_ConstantFP (ConstantFPFloat _fpv _ty) =
    (sem_ConstantFP_ConstantFPFloat _fpv (sem_Type _ty))
-- semantic domain
type T_ConstantFP = ( Doc,ConstantFP)
data Inh_ConstantFP = Inh_ConstantFP {}
data Syn_ConstantFP = Syn_ConstantFP {pp_Syn_ConstantFP :: Doc,self_Syn_ConstantFP :: ConstantFP}
wrap_ConstantFP :: T_ConstantFP ->
                   Inh_ConstantFP ->
                   Syn_ConstantFP
wrap_ConstantFP sem (Inh_ConstantFP) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_ConstantFP _lhsOpp _lhsOself))
sem_ConstantFP_ConstantFPDouble :: Double ->
                                   T_Type ->
                                   T_ConstantFP
sem_ConstantFP_ConstantFPDouble dbv_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantFP
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 256 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> double dbv_
              {-# LINE 1769 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantFPDouble dbv_ _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_ConstantFP_ConstantFPFloat :: Float ->
                                  T_Type ->
                                  T_ConstantFP
sem_ConstantFP_ConstantFPFloat fpv_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: ConstantFP
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 255 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> float fpv_
              {-# LINE 1789 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ConstantFPFloat fpv_ _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
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
type T_DataLayout = ( Doc,DataLayout)
data Inh_DataLayout = Inh_DataLayout {}
data Syn_DataLayout = Syn_DataLayout {pp_Syn_DataLayout :: Doc,self_Syn_DataLayout :: DataLayout}
wrap_DataLayout :: T_DataLayout ->
                   Inh_DataLayout ->
                   Syn_DataLayout
wrap_DataLayout sem (Inh_DataLayout) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_DataLayout _lhsOpp _lhsOself))
sem_DataLayout_DataLayout :: T_DLayout ->
                             T_DataLayout
sem_DataLayout_DataLayout s_ =
    (let _lhsOpp :: Doc
         _lhsOself :: DataLayout
         _sIself :: DLayout
         _lhsOpp =
             ({-# LINE 48 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "target datalayout =" <+> dquotes (Prelude.foldr1 (\x y -> x <> char '-' <> y) (Prelude.map text _sIself))
              {-# LINE 1860 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             DataLayout _sIself
         _lhsOself =
             _self
         ( _sIself) =
             s_
     in  ( _lhsOpp,_lhsOself))
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
type T_Function = ILabel ->
                  ( ILabel,Doc,Doc,Function)
data Inh_Function = Inh_Function {ilabel_Inh_Function :: ILabel}
data Syn_Function = Syn_Function {ilabel_Syn_Function :: ILabel,pp_Syn_Function :: Doc,ppccfg_Syn_Function :: Doc,self_Syn_Function :: Function}
wrap_Function :: T_Function ->
                 Inh_Function ->
                 Syn_Function
wrap_Function sem (Inh_Function _lhsIilabel) =
    (let ( _lhsOilabel,_lhsOpp,_lhsOppccfg,_lhsOself) = sem _lhsIilabel
     in  (Syn_Function _lhsOilabel _lhsOpp _lhsOppccfg _lhsOself))
sem_Function_FunctionDecl :: T_Id ->
                             T_Linkage ->
                             T_Type ->
                             T_Parameters ->
                             T_Function
sem_Function_FunctionDecl name_ linkage_ retty_ params_ =
    (\ _lhsIilabel ->
         (let _lhsOppccfg :: Doc
              _lhsOpp :: Doc
              _lhsOself :: Function
              _lhsOilabel :: ILabel
              _nameIself :: Id
              _linkageIpp :: Doc
              _linkageIself :: Linkage
              _rettyIpp :: Doc
              _rettyIself :: Type
              _paramsIpp :: Doc
              _paramsIself :: Parameters
              _lhsOppccfg =
                  ({-# LINE 41 "src/Concurrent/Model/Visualizer.ag" #-}
                   P.empty
                   {-# LINE 2178 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 66 "src/Language/LLVMIR/Printer/Module.ag" #-}
                   text "declare" <+> _linkageIpp <+> _rettyIpp <+> char '@' <> text _nameIself <> parens _paramsIpp
                   {-# LINE 2183 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _self =
                  FunctionDecl _nameIself _linkageIself _rettyIself _paramsIself
              _lhsOself =
                  _self
              _lhsOilabel =
                  ({-# LINE 28 "src/Concurrent/Model/Visualizer.ag" #-}
                   _lhsIilabel
                   {-# LINE 2192 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              ( _nameIself) =
                  name_
              ( _linkageIpp,_linkageIself) =
                  linkage_
              ( _rettyIpp,_rettyIself) =
                  retty_
              ( _paramsIpp,_paramsIself) =
                  params_
          in  ( _lhsOilabel,_lhsOpp,_lhsOppccfg,_lhsOself)))
sem_Function_FunctionDef :: T_Id ->
                            T_Linkage ->
                            T_Type ->
                            T_Parameters ->
                            T_BasicBlocks ->
                            T_Function
sem_Function_FunctionDef name_ linkage_ retty_ params_ body_ =
    (\ _lhsIilabel ->
         (let _lhsOppccfg :: Doc
              _bodyOfname :: String
              _lhsOpp :: Doc
              _bodyOilabel :: ILabel
              _ilabel :: ILabel
              _lhsOself :: Function
              _lhsOilabel :: ILabel
              _nameIself :: Id
              _linkageIpp :: Doc
              _linkageIself :: Linkage
              _rettyIpp :: Doc
              _rettyIself :: Type
              _paramsIpp :: Doc
              _paramsIself :: Parameters
              _bodyIilabel :: ILabel
              _bodyIpp :: Doc
              _bodyIppccfg :: Doc
              _bodyIself :: BasicBlocks
              _lhsOppccfg =
                  ({-# LINE 43 "src/Concurrent/Model/Visualizer.ag" #-}
                   text "subgraph cluster_" <> text _nameIself <+> text "{" </>
                   text "style=filled;" </>
                   text "color=lightblue;" </>
                   _bodyIppccfg </>
                   text "label = \"function" <+> text _nameIself <> text "\"; }"
                   {-# LINE 2236 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _bodyOfname =
                  ({-# LINE 49 "src/Concurrent/Model/Visualizer.ag" #-}
                   _nameIself
                   {-# LINE 2241 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 65 "src/Language/LLVMIR/Printer/Module.ag" #-}
                   text "define"  <+> _linkageIpp <+> _rettyIpp <+> char '@' <> text _nameIself <> parens _paramsIpp <> char '{' <$> _bodyIpp <$> char '}'
                   {-# LINE 2246 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              (_bodyOilabel,_ilabel) =
                  ({-# LINE 48 "src/Concurrent/Model/Visualizer.ag" #-}
                   let __cont = _lhsIilabel in seq __cont ( case nextUnique __cont of { (__cont, ilabel) -> (__cont,ilabel)} )
                   {-# LINE 2251 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _self =
                  FunctionDef _nameIself _linkageIself _rettyIself _paramsIself _bodyIself
              _lhsOself =
                  _self
              _lhsOilabel =
                  ({-# LINE 28 "src/Concurrent/Model/Visualizer.ag" #-}
                   _ilabel
                   {-# LINE 2260 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              ( _nameIself) =
                  name_
              ( _linkageIpp,_linkageIself) =
                  linkage_
              ( _rettyIpp,_rettyIself) =
                  retty_
              ( _paramsIpp,_paramsIself) =
                  params_
              ( _bodyIilabel,_bodyIpp,_bodyIppccfg,_bodyIself) =
                  body_ _bodyOfname _bodyOilabel
          in  ( _lhsOilabel,_lhsOpp,_lhsOppccfg,_lhsOself)))
-- Functions ---------------------------------------------------
-- cata
sem_Functions :: Functions ->
                 T_Functions
sem_Functions m =
    (Data.Map.foldrWithKey sem_Functions_Entry sem_Functions_Nil (Data.Map.map sem_Function m))
-- semantic domain
type T_Functions = ILabel ->
                   ( ILabel,Doc,Functions)
data Inh_Functions = Inh_Functions {ilabel_Inh_Functions :: ILabel}
data Syn_Functions = Syn_Functions {ilabel_Syn_Functions :: ILabel,pp_Syn_Functions :: Doc,self_Syn_Functions :: Functions}
wrap_Functions :: T_Functions ->
                  Inh_Functions ->
                  Syn_Functions
wrap_Functions sem (Inh_Functions _lhsIilabel) =
    (let ( _lhsOilabel,_lhsOpp,_lhsOself) = sem _lhsIilabel
     in  (Syn_Functions _lhsOilabel _lhsOpp _lhsOself))
sem_Functions_Entry :: String ->
                       T_Function ->
                       T_Functions ->
                       T_Functions
sem_Functions_Entry key_ val_ tl_ =
    (\ _lhsIilabel ->
         (let _valOilabel :: ILabel
              _lhsOpp :: Doc
              _lhsOself :: Functions
              _lhsOilabel :: ILabel
              _tlOilabel :: ILabel
              _valIilabel :: ILabel
              _valIpp :: Doc
              _valIppccfg :: Doc
              _valIself :: Function
              _tlIilabel :: ILabel
              _tlIpp :: Doc
              _tlIself :: Functions
              _valOilabel =
                  ({-# LINE 38 "src/Concurrent/Model/Visualizer.ag" #-}
                   _lhsIilabel
                   {-# LINE 2311 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _lhsOpp =
                  ({-# LINE 156 "src/Language/LLVMIR/Printer/Module.ag" #-}
                   _valIpp <$> _tlIpp
                   {-# LINE 2316 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _self =
                  Data.Map.insert key_ _valIself _tlIself
              _lhsOself =
                  _self
              _lhsOilabel =
                  ({-# LINE 28 "src/Concurrent/Model/Visualizer.ag" #-}
                   _tlIilabel
                   {-# LINE 2325 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _tlOilabel =
                  ({-# LINE 28 "src/Concurrent/Model/Visualizer.ag" #-}
                   _valIilabel
                   {-# LINE 2330 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              ( _valIilabel,_valIpp,_valIppccfg,_valIself) =
                  val_ _valOilabel
              ( _tlIilabel,_tlIpp,_tlIself) =
                  tl_ _tlOilabel
          in  ( _lhsOilabel,_lhsOpp,_lhsOself)))
sem_Functions_Nil :: T_Functions
sem_Functions_Nil =
    (\ _lhsIilabel ->
         (let _lhsOpp :: Doc
              _lhsOself :: Functions
              _lhsOilabel :: ILabel
              _lhsOpp =
                  ({-# LINE 155 "src/Language/LLVMIR/Printer/Module.ag" #-}
                   P.empty
                   {-# LINE 2346 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
              _self =
                  Data.Map.empty
              _lhsOself =
                  _self
              _lhsOilabel =
                  ({-# LINE 28 "src/Concurrent/Model/Visualizer.ag" #-}
                   _lhsIilabel
                   {-# LINE 2355 "src/Concurrent/Model/Visualizer.hs" #-}
                   )
          in  ( _lhsOilabel,_lhsOpp,_lhsOself)))
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
type T_Global = ( Doc,Global)
data Inh_Global = Inh_Global {}
data Syn_Global = Syn_Global {pp_Syn_Global :: Doc,self_Syn_Global :: Global}
wrap_Global :: T_Global ->
               Inh_Global ->
               Syn_Global
wrap_Global sem (Inh_Global) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Global _lhsOpp _lhsOself))
sem_Global_GlobalVar :: T_Id ->
                        T_Linkage ->
                        Bool ->
                        Bool ->
                        T_Type ->
                        T_MValue ->
                        T_Align ->
                        T_Global
sem_Global_GlobalVar name_ linkage_ isConst_ isUaddr_ ty_ ival_ align_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Global
         _nameIself :: Id
         _linkageIpp :: Doc
         _linkageIself :: Linkage
         _tyIpp :: Doc
         _tyIself :: Type
         _ivalIpp :: Doc
         _ivalIself :: MValue
         _alignIpp :: Doc
         _alignIself :: Align
         _lhsOpp =
             ({-# LINE 62 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "@" <> text _nameIself <+> text "=" <+>  _linkageIpp <+> ppKeyword isUaddr_ "unnamed_addr" <+> ppKeyword isConst_ "constant" <+> text "," <+> _ivalIpp <> text "," <+> _alignIpp
              {-# LINE 2422 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             GlobalVar _nameIself _linkageIself isConst_ isUaddr_ _tyIself _ivalIself _alignIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
         ( _linkageIpp,_linkageIself) =
             linkage_
         ( _tyIpp,_tyIself) =
             ty_
         ( _ivalIpp,_ivalIself) =
             ival_
         ( _alignIpp,_alignIself) =
             align_
     in  ( _lhsOpp,_lhsOself))
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
type T_GlobalValue = ( Doc,GlobalValue)
data Inh_GlobalValue = Inh_GlobalValue {}
data Syn_GlobalValue = Syn_GlobalValue {pp_Syn_GlobalValue :: Doc,self_Syn_GlobalValue :: GlobalValue}
wrap_GlobalValue :: T_GlobalValue ->
                    Inh_GlobalValue ->
                    Syn_GlobalValue
wrap_GlobalValue sem (Inh_GlobalValue) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_GlobalValue _lhsOpp _lhsOself))
sem_GlobalValue_FunctionValue :: T_Identifier ->
                                 T_Type ->
                                 T_GlobalValue
sem_GlobalValue_FunctionValue n_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: GlobalValue
         _nIpp :: Doc
         _nIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 279 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> _nIpp
              {-# LINE 2472 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             FunctionValue _nIself _tyIself
         _lhsOself =
             _self
         ( _nIpp,_nIself) =
             n_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_GlobalValue_GlobalAlias :: T_Identifier ->
                               T_Type ->
                               T_GlobalValue
sem_GlobalValue_GlobalAlias n_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: GlobalValue
         _nIpp :: Doc
         _nIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 280 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> _nIpp
              {-# LINE 2496 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             GlobalAlias _nIself _tyIself
         _lhsOself =
             _self
         ( _nIpp,_nIself) =
             n_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_GlobalValue_GlobalVariable :: T_Identifier ->
                                  T_Type ->
                                  T_GlobalValue
sem_GlobalValue_GlobalVariable n_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: GlobalValue
         _nIpp :: Doc
         _nIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 281 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> _nIpp
              {-# LINE 2520 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             GlobalVariable _nIself _tyIself
         _lhsOself =
             _self
         ( _nIpp,_nIself) =
             n_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
-- Globals -----------------------------------------------------
-- cata
sem_Globals :: Globals ->
               T_Globals
sem_Globals list =
    (Prelude.foldr sem_Globals_Cons sem_Globals_Nil (Prelude.map sem_Global list))
-- semantic domain
type T_Globals = ( Doc,Globals)
data Inh_Globals = Inh_Globals {}
data Syn_Globals = Syn_Globals {pp_Syn_Globals :: Doc,self_Syn_Globals :: Globals}
wrap_Globals :: T_Globals ->
                Inh_Globals ->
                Syn_Globals
wrap_Globals sem (Inh_Globals) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Globals _lhsOpp _lhsOself))
sem_Globals_Cons :: T_Global ->
                    T_Globals ->
                    T_Globals
sem_Globals_Cons hd_ tl_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Globals
         _hdIpp :: Doc
         _hdIself :: Global
         _tlIpp :: Doc
         _tlIself :: Globals
         _lhsOpp =
             ({-# LINE 37 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _hdIpp <$> _tlIpp
              {-# LINE 2560 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIpp,_hdIself) =
             hd_
         ( _tlIpp,_tlIself) =
             tl_
     in  ( _lhsOpp,_lhsOself))
sem_Globals_Nil :: T_Globals
sem_Globals_Nil =
    (let _lhsOpp :: Doc
         _lhsOself :: Globals
         _lhsOpp =
             ({-# LINE 37 "src/Language/LLVMIR/Printer/Module.ag" #-}
              P.empty
              {-# LINE 2578 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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
type T_Identifier = ( Doc,Identifier)
data Inh_Identifier = Inh_Identifier {}
data Syn_Identifier = Syn_Identifier {pp_Syn_Identifier :: Doc,self_Syn_Identifier :: Identifier}
wrap_Identifier :: T_Identifier ->
                   Inh_Identifier ->
                   Syn_Identifier
wrap_Identifier sem (Inh_Identifier) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Identifier _lhsOpp _lhsOself))
sem_Identifier_Global :: T_Id ->
                         T_Identifier
sem_Identifier_Global name_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Identifier
         _nameIself :: Id
         _lhsOpp =
             ({-# LINE 43 "src/Language/LLVMIR/Printer/Module.ag" #-}
              char '@' <> text _nameIself
              {-# LINE 2637 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Global _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
     in  ( _lhsOpp,_lhsOself))
sem_Identifier_Local :: T_Id ->
                        T_Identifier
sem_Identifier_Local name_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Identifier
         _nameIself :: Id
         _lhsOpp =
             ({-# LINE 44 "src/Language/LLVMIR/Printer/Module.ag" #-}
              char '%' <> text _nameIself
              {-# LINE 2655 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Local _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
     in  ( _lhsOpp,_lhsOself))
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
         _hdIpp :: Doc
         _hdIself :: Identifier
         _tlIself :: Identifiers
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIpp,_hdIself) =
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
type T_Instruction = ( Doc,Doc,Instruction)
data Inh_Instruction = Inh_Instruction {}
data Syn_Instruction = Syn_Instruction {pp_Syn_Instruction :: Doc,ppccfg_Syn_Instruction :: Doc,self_Syn_Instruction :: Instruction}
wrap_Instruction :: T_Instruction ->
                    Inh_Instruction ->
                    Syn_Instruction
wrap_Instruction sem (Inh_Instruction) =
    (let ( _lhsOpp,_lhsOppccfg,_lhsOself) = sem
     in  (Syn_Instruction _lhsOpp _lhsOppccfg _lhsOself))
sem_Instruction_AShr :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_AShr pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 2849 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 2854 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 118 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBitBinOp _tyIpp _idIpp "ashr" _op1Ipp _op2Ipp
              {-# LINE 2859 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             AShr _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Add :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Add pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 2898 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 2903 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 104 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBinOp _tyIpp _idIpp "add"  _op1Ipp _op2Ipp
              {-# LINE 2908 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Add _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Alloca :: T_PC ->
                          T_Identifier ->
                          T_Type ->
                          T_Align ->
                          T_Instruction
sem_Instruction_Alloca pc_ id_ ty_ align_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _alignIpp :: Doc
         _alignIself :: Align
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 2944 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 2949 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 82 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _idIpp <+> char '=' <+> text "alloca" <+> _tyIpp <> char ',' <+> _alignIpp
              {-# LINE 2954 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Alloca _pcIself _idIself _tyIself _alignIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _alignIpp,_alignIself) =
             align_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_And :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_And pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 2991 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 2996 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 119 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBitBinOp _tyIpp _idIpp "and"  _op1Ipp _op2Ipp
              {-# LINE 3001 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             And _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_AtomicRMW :: T_PC ->
                             T_Identifier ->
                             T_Values ->
                             T_BinOp ->
                             T_AtomicOrdering ->
                             T_Instruction
sem_Instruction_AtomicRMW pc_ id_ args_ op_ ord_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _argsIpp :: Doc
         _argsIself :: Values
         _opIpp :: Doc
         _opIself :: BinOp
         _ordIpp :: Doc
         _ordIself :: AtomicOrdering
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3040 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3045 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 126 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _idIpp <+> char '=' <+> text "atomicrmw" <+> _opIpp <+>  _argsIpp  <+> _ordIpp
              {-# LINE 3050 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             AtomicRMW _pcIself _idIself _argsIself _opIself _ordIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _argsIpp,_argsIself) =
             args_
         ( _opIpp,_opIself) =
             op_
         ( _ordIpp,_ordIself) =
             ord_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_BitCast :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           T_Type ->
                           T_Instruction
sem_Instruction_BitCast pc_ id_ v_ ty_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3086 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3091 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 102 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pConvOp _idIpp "bitcast"  _vIpp _tyIpp
              {-# LINE 3096 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             BitCast _pcIself _idIself _vIself _tyIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Br :: T_PC ->
                      T_Value ->
                      T_Value ->
                      T_Value ->
                      T_Instruction
sem_Instruction_Br pc_ v_ t_ f_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _vIpp :: Doc
         _vIself :: Value
         _tIpp :: Doc
         _tIself :: Value
         _fIpp :: Doc
         _fIself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3130 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3135 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 88 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "br" <+> _vIpp <> comma <+> _tIpp <> comma <+> _fIpp
              {-# LINE 3140 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Br _pcIself _vIself _tIself _fIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _vIpp,_vIself) =
             v_
         ( _tIpp,_tIself) =
             t_
         ( _fIpp,_fIself) =
             f_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Call :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Id ->
                        T_Values ->
                        T_Instruction
sem_Instruction_Call pc_ mres_ ty_ callee_ args_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _mresIpp :: Doc
         _mresIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _calleeIself :: Id
         _argsIpp :: Doc
         _argsIself :: Values
         _lhsOppccfg =
             ({-# LINE 78 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself (text "call" <+> text callee_)
              {-# LINE 3176 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3181 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 89 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _mresIpp <+> char '=' <+> text "call" <+> _tyIpp <+> char '@' <> text callee_ <> char '(' <> _argsIpp <> char ')'
              {-# LINE 3186 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Call _pcIself _mresIself _tyIself _calleeIself _argsIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _mresIpp,_mresIself) =
             mres_
         ( _tyIpp,_tyIself) =
             ty_
         ( _calleeIself) =
             callee_
         ( _argsIpp,_argsIself) =
             args_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_CreateThread :: T_PC ->
                                T_Values ->
                                T_Instruction
sem_Instruction_CreateThread pc_ args_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _argsIpp :: Doc
         _argsIself :: Values
         _lhsOppccfg =
             ({-# LINE 64 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself (text "create_thread")
              {-# LINE 3216 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3221 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 127 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "create_thread" <+> parens _argsIpp
              {-# LINE 3226 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             CreateThread _pcIself _argsIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _argsIpp,_argsIself) =
             args_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_ExtractValue :: T_PC ->
                                T_Identifier ->
                                T_Value ->
                                T_Ints ->
                                T_Instruction
sem_Instruction_ExtractValue pc_ id_ aggr_ idxs_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _aggrIpp :: Doc
         _aggrIself :: Value
         _idxsIpp :: Doc
         _idxsIself :: Ints
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3256 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3261 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 124 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _idIpp <+> char '=' <+> text "extractvalue" <+> _aggrIpp <> char ',' <+> _idxsIpp
              {-# LINE 3266 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ExtractValue _pcIself _idIself _aggrIself _idxsIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _aggrIpp,_aggrIself) =
             aggr_
         ( _idxsIpp,_idxsIself) =
             idxs_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_FAdd :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FAdd pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3303 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3308 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 105 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBinOp _tyIpp _idIpp "fadd" _op1Ipp _op2Ipp
              {-# LINE 3313 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             FAdd _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_FCmp :: T_PC ->
                        T_Identifier ->
                        T_RealPredicate ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FCmp pc_ id_ cond_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _condIpp :: Doc
         _condIself :: RealPredicate
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3355 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3360 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 86 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _idIpp <+> char '=' <+> text "icmp"   <+> _condIpp <+> _op1Ipp <> char ',' <+> _op2Ipp
              {-# LINE 3365 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             FCmp _pcIself _idIself _condIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _condIpp,_condIself) =
             cond_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_FDiv :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FDiv pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3406 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3411 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 112 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBinOp _tyIpp _idIpp "fdiv" _op1Ipp _op2Ipp
              {-# LINE 3416 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             FDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_FMul :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FMul pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3455 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3460 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 109 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBinOp _tyIpp _idIpp "fmul" _op1Ipp _op2Ipp
              {-# LINE 3465 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             FMul _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_FPExt :: T_PC ->
                         T_Identifier ->
                         T_Value ->
                         T_Type ->
                         T_Instruction
sem_Instruction_FPExt pc_ id_ v_ ty_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3501 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3506 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 99 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pConvOp _idIpp "fpext"    _vIpp _tyIpp
              {-# LINE 3511 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             FPExt _pcIself _idIself _vIself _tyIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_FPToSI :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_FPToSI pc_ id_ v_ ty_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3545 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3550 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 95 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pConvOp _idIpp "fptosi"   _vIpp _tyIpp
              {-# LINE 3555 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             FPToSI _pcIself _idIself _vIself _tyIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_FPToUI :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_FPToUI pc_ id_ v_ ty_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3589 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3594 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 94 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pConvOp _idIpp "fptoui"   _vIpp _tyIpp
              {-# LINE 3599 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             FPToUI _pcIself _idIself _vIself _tyIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_FPTrunc :: T_PC ->
                           T_Identifier ->
                           T_Value ->
                           T_Type ->
                           T_Instruction
sem_Instruction_FPTrunc pc_ id_ v_ ty_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3633 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3638 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 98 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pConvOp _idIpp "fptrunc"  _vIpp _tyIpp
              {-# LINE 3643 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             FPTrunc _pcIself _idIself _vIself _tyIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_FRem :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FRem pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3680 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3685 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 115 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBinOp _tyIpp _idIpp "frem" _op1Ipp _op2Ipp
              {-# LINE 3690 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             FRem _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_FSub :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_FSub pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3729 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3734 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 107 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBinOp _tyIpp _idIpp "fsub" _op1Ipp _op2Ipp
              {-# LINE 3739 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             FSub _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_GetElementPtr :: T_PC ->
                                 T_Identifier ->
                                 T_Type ->
                                 T_Value ->
                                 T_Values ->
                                 T_Instruction
sem_Instruction_GetElementPtr pc_ id_ ty_ struct_ idxs_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _structIpp :: Doc
         _structIself :: Value
         _idxsIpp :: Doc
         _idxsIself :: Values
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3778 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3783 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 103 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _idIpp <+> char '=' <+> text "getelementptr" <+> _structIpp <> char ',' <+> _idxsIpp
              {-# LINE 3788 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             GetElementPtr _pcIself _idIself _tyIself _structIself _idxsIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _structIpp,_structIself) =
             struct_
         ( _idxsIpp,_idxsIself) =
             idxs_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_ICmp :: T_PC ->
                        T_Identifier ->
                        T_IntPredicate ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_ICmp pc_ id_ cond_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _condIpp :: Doc
         _condIself :: IntPredicate
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3830 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3835 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 85 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _idIpp <+> char '=' <+> text "icmp"   <+> _condIpp <+> _op1Ipp <> char ',' <+> _op2Ipp
              {-# LINE 3840 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ICmp _pcIself _idIself _condIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _condIpp,_condIself) =
             cond_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_InsertValue :: T_PC ->
                               T_Identifier ->
                               T_Value ->
                               T_Value ->
                               T_Ints ->
                               T_Instruction
sem_Instruction_InsertValue pc_ id_ aggr_ ival_ idxs_ =
    (let _lhsOpp :: Doc
         _lhsOppccfg :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _aggrIpp :: Doc
         _aggrIself :: Value
         _ivalIpp :: Doc
         _ivalIself :: Value
         _idxsIpp :: Doc
         _idxsIself :: Ints
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3881 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 125 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _idIpp <+> char '=' <+> text "insertvalue" <+> _aggrIpp <> char ',' <+> _ivalIpp <> char ',' <+> _idxsIpp
              {-# LINE 3886 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOppccfg =
             ({-# LINE 31 "src/Concurrent/Model/Visualizer.ag" #-}
              P.empty
              {-# LINE 3891 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             InsertValue _pcIself _idIself _aggrIself _ivalIself _idxsIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _aggrIpp,_aggrIself) =
             aggr_
         ( _ivalIpp,_ivalIself) =
             ival_
         ( _idxsIpp,_idxsIself) =
             idxs_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_IntToPtr :: T_PC ->
                            T_Identifier ->
                            T_Value ->
                            T_Type ->
                            T_Instruction
sem_Instruction_IntToPtr pc_ id_ v_ ty_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3927 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3932 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 101 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pConvOp _idIpp "inttoptr" _vIpp _tyIpp
              {-# LINE 3937 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             IntToPtr _pcIself _idIself _vIself _tyIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_LShr :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_LShr pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 3974 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 3979 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 117 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBitBinOp _tyIpp _idIpp "lshr" _op1Ipp _op2Ipp
              {-# LINE 3984 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LShr _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Load :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Align ->
                        T_Instruction
sem_Instruction_Load pc_ id_ v_ align_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _alignIpp :: Doc
         _alignIself :: Align
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4020 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4025 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 83 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _idIpp <+> char '=' <+> text "load"   <+> _vIpp  <> char ',' <+> _alignIpp
              {-# LINE 4030 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Load _pcIself _idIself _vIself _alignIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _alignIpp,_alignIself) =
             align_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Mul :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Mul pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4067 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4072 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 108 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBinOp _tyIpp _idIpp "mul"  _op1Ipp _op2Ipp
              {-# LINE 4077 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Mul _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_MutexInit :: T_PC ->
                             T_Identifier ->
                             T_Value ->
                             T_Instruction
sem_Instruction_MutexInit pc_ rv_ mutex_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _rvIpp :: Doc
         _rvIself :: Identifier
         _mutexIpp :: Doc
         _mutexIself :: Value
         _lhsOppccfg =
             ({-# LINE 66 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself (text "mutex_init")
              {-# LINE 4110 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4115 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 128 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "mutex_init"    <+> _mutexIpp
              {-# LINE 4120 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             MutexInit _pcIself _rvIself _mutexIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _rvIpp,_rvIself) =
             rv_
         ( _mutexIpp,_mutexIself) =
             mutex_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_MutexLock :: T_PC ->
                             T_Identifier ->
                             T_Value ->
                             T_Instruction
sem_Instruction_MutexLock pc_ rv_ mutex_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _rvIpp :: Doc
         _rvIself :: Identifier
         _mutexIpp :: Doc
         _mutexIself :: Value
         _lhsOppccfg =
             ({-# LINE 68 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself (text "mutex_lock")
              {-# LINE 4149 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4154 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 129 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "mutex_lock"    <+> _mutexIpp
              {-# LINE 4159 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             MutexLock _pcIself _rvIself _mutexIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _rvIpp,_rvIself) =
             rv_
         ( _mutexIpp,_mutexIself) =
             mutex_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_MutexUnlock :: T_PC ->
                               T_Identifier ->
                               T_Value ->
                               T_Instruction
sem_Instruction_MutexUnlock pc_ rv_ mutex_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _rvIpp :: Doc
         _rvIself :: Identifier
         _mutexIpp :: Doc
         _mutexIself :: Value
         _lhsOppccfg =
             ({-# LINE 70 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself (text "mutex_unlock")
              {-# LINE 4188 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4193 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 130 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "mutex_unlock"  <+> _mutexIpp
              {-# LINE 4198 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             MutexUnlock _pcIself _rvIself _mutexIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _rvIpp,_rvIself) =
             rv_
         ( _mutexIpp,_mutexIself) =
             mutex_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_NotifyEvent :: T_PC ->
                               Int ->
                               T_Instruction
sem_Instruction_NotifyEvent pc_ event_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _lhsOppccfg =
             ({-# LINE 76 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself (text "notify_event")
              {-# LINE 4222 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 132 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "notify_event"  <+> int event_
              {-# LINE 4227 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 37 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4232 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             NotifyEvent _pcIself event_
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Or :: T_PC ->
                      T_Identifier ->
                      T_Type ->
                      T_Value ->
                      T_Value ->
                      T_Instruction
sem_Instruction_Or pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4263 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4268 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 120 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBitBinOp _tyIpp _idIpp "or"   _op1Ipp _op2Ipp
              {-# LINE 4273 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Or _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_PHI :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_PValues ->
                       T_Instruction
sem_Instruction_PHI pc_ id_ ty_ vals_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _valsIpp :: Doc
         _valsIself :: PValues
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4309 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4314 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 122 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _idIpp <+> char '=' <+> text "phi" <+> _tyIpp <+> _valsIpp
              {-# LINE 4319 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             PHI _pcIself _idIself _tyIself _valsIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _valsIpp,_valsIself) =
             vals_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_PtrToInt :: T_PC ->
                            T_Identifier ->
                            T_Value ->
                            T_Type ->
                            T_Instruction
sem_Instruction_PtrToInt pc_ id_ v_ ty_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4353 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4358 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 100 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pConvOp _idIpp "ptrtoint" _vIpp _tyIpp
              {-# LINE 4363 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             PtrToInt _pcIself _idIself _vIself _tyIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Ret :: T_PC ->
                       T_RetInst ->
                       T_Instruction
sem_Instruction_Ret pc_ r_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _rIpp :: Doc
         _rIself :: RetInst
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4391 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4396 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 81 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ret" <+> _rIpp
              {-# LINE 4401 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Ret _pcIself _rIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _rIpp,_rIself) =
             r_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_SDiv :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_SDiv pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4434 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4439 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 111 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBinOp _tyIpp _idIpp "sdiv" _op1Ipp _op2Ipp
              {-# LINE 4444 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             SDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_SExt :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Type ->
                        T_Instruction
sem_Instruction_SExt pc_ id_ v_ ty_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4480 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4485 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 93 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pConvOp _idIpp "sext"     _vIpp _tyIpp
              {-# LINE 4490 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             SExt _pcIself _idIself _vIself _tyIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_SIToFP :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_SIToFP pc_ id_ v_ ty_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4524 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4529 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 97 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pConvOp _idIpp "sitofp"   _vIpp _tyIpp
              {-# LINE 4534 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             SIToFP _pcIself _idIself _vIself _tyIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_SRem :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_SRem pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4571 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4576 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 114 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBinOp _tyIpp _idIpp "srem" _op1Ipp _op2Ipp
              {-# LINE 4581 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             SRem _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Select :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Value ->
                          T_Value ->
                          T_Instruction
sem_Instruction_Select pc_ id_ cond_ valt_ valf_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _condIpp :: Doc
         _condIself :: Value
         _valtIpp :: Doc
         _valtIself :: Value
         _valfIpp :: Doc
         _valfIself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4620 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4625 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 123 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _idIpp <+> char '=' <+> _condIpp <> char ',' <+> _valtIpp <> char ',' <+> _valfIpp
              {-# LINE 4630 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Select _pcIself _idIself _condIself _valtIself _valfIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _condIpp,_condIself) =
             cond_
         ( _valtIpp,_valtIself) =
             valt_
         ( _valfIpp,_valfIself) =
             valf_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Shl :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Shl pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4669 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4674 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 116 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBitBinOp _tyIpp _idIpp "shl"  _op1Ipp _op2Ipp
              {-# LINE 4679 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Shl _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Store :: T_PC ->
                         T_Type ->
                         T_Value ->
                         T_Value ->
                         T_Align ->
                         T_Instruction
sem_Instruction_Store pc_ ty_ v1_ v2_ align_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _tyIpp :: Doc
         _tyIself :: Type
         _v1Ipp :: Doc
         _v1Iself :: Value
         _v2Ipp :: Doc
         _v2Iself :: Value
         _alignIpp :: Doc
         _alignIself :: Align
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4718 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4723 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 84 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> text "store" <+> _v1Ipp <> char ',' <+> _v2Ipp <> char ',' <+> _alignIpp
              {-# LINE 4728 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Store _pcIself _tyIself _v1Iself _v2Iself _alignIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _tyIpp,_tyIself) =
             ty_
         ( _v1Ipp,_v1Iself) =
             v1_
         ( _v2Ipp,_v2Iself) =
             v2_
         ( _alignIpp,_alignIself) =
             align_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Sub :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Sub pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4767 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4772 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 106 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBinOp _tyIpp _idIpp "sub"  _op1Ipp _op2Ipp
              {-# LINE 4777 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Sub _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Switch :: T_PC ->
                          T_IntTyValIdL ->
                          T_Instruction
sem_Instruction_Switch pc_ elems_ =
    (let _lhsOpp :: Doc
         _lhsOppccfg :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _elemsIself :: IntTyValIdL
         _lhsOpp =
             ({-# LINE 37 "src/Language/LLVMIR/Printer/Module.ag" #-}
              P.empty
              {-# LINE 4806 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOppccfg =
             ({-# LINE 31 "src/Concurrent/Model/Visualizer.ag" #-}
              P.empty
              {-# LINE 4811 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Switch _pcIself _elemsIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _elemsIself) =
             elems_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Trunc :: T_PC ->
                         T_Identifier ->
                         T_Value ->
                         T_Type ->
                         T_Instruction
sem_Instruction_Trunc pc_ id_ v_ ty_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4841 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4846 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 91 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pConvOp _idIpp "trunc"    _vIpp _tyIpp
              {-# LINE 4851 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Trunc _pcIself _idIself _vIself _tyIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_UBr :: T_PC ->
                       T_Value ->
                       T_Instruction
sem_Instruction_UBr pc_ d_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _dIpp :: Doc
         _dIself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4879 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4884 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 87 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "br" <+> _dIpp
              {-# LINE 4889 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             UBr _pcIself _dIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _dIpp,_dIself) =
             d_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_UDiv :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_UDiv pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4922 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4927 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 110 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBinOp _tyIpp _idIpp "udiv" _op1Ipp _op2Ipp
              {-# LINE 4932 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             UDiv _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_UIToFP :: T_PC ->
                          T_Identifier ->
                          T_Value ->
                          T_Type ->
                          T_Instruction
sem_Instruction_UIToFP pc_ id_ v_ ty_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 4968 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 4973 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 96 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pConvOp _idIpp "uitofp"   _vIpp _tyIpp
              {-# LINE 4978 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             UIToFP _pcIself _idIself _vIself _tyIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_URem :: T_PC ->
                        T_Identifier ->
                        T_Type ->
                        T_Value ->
                        T_Value ->
                        T_Instruction
sem_Instruction_URem pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 5015 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 5020 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 113 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBinOp _tyIpp _idIpp "urem" _op1Ipp _op2Ipp
              {-# LINE 5025 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             URem _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Unreachable :: T_PC ->
                               T_Instruction
sem_Instruction_Unreachable pc_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 5052 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 5057 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 90 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "unreachable"
              {-# LINE 5062 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Unreachable _pcIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_WaitEvent :: T_PC ->
                             Int ->
                             T_Instruction
sem_Instruction_WaitEvent pc_ event_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _lhsOppccfg =
             ({-# LINE 72 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself (text "wait_event")
              {-# LINE 5082 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 131 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "wait_event"    <+> int event_
              {-# LINE 5087 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 37 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 5092 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             WaitEvent _pcIself event_
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_WaitTime :: T_PC ->
                            T_Value ->
                            T_Instruction
sem_Instruction_WaitTime pc_ time_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _timeIpp :: Doc
         _timeIself :: Value
         _lhsOppccfg =
             ({-# LINE 74 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself (text "wait_time")
              {-# LINE 5114 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 133 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "wait_time"     <+> _timeIpp
              {-# LINE 5119 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 37 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 5124 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             WaitTime _pcIself _timeIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _timeIpp,_timeIself) =
             time_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_Xor :: T_PC ->
                       T_Identifier ->
                       T_Type ->
                       T_Value ->
                       T_Value ->
                       T_Instruction
sem_Instruction_Xor pc_ id_ ty_ op1_ op2_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _op1Ipp :: Doc
         _op1Iself :: Value
         _op2Ipp :: Doc
         _op2Iself :: Value
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 5157 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 5162 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 121 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pBitBinOp _tyIpp _idIpp "xor"  _op1Ipp _op2Ipp
              {-# LINE 5167 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Xor _pcIself _idIself _tyIself _op1Iself _op2Iself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
         ( _op1Ipp,_op1Iself) =
             op1_
         ( _op2Ipp,_op2Iself) =
             op2_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instruction_ZExt :: T_PC ->
                        T_Identifier ->
                        T_Value ->
                        T_Type ->
                        T_Instruction
sem_Instruction_ZExt pc_ id_ v_ ty_ =
    (let _lhsOppccfg :: Doc
         _lhsOpp :: Doc
         _lhsOself :: Instruction
         _pcIself :: PC
         _idIpp :: Doc
         _idIself :: Identifier
         _vIpp :: Doc
         _vIself :: Value
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOppccfg =
             ({-# LINE 62 "src/Concurrent/Model/Visualizer.ag" #-}
              int _pcIself <+> dotLabel _pcIself _pp
              {-# LINE 5203 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOpp =
             ({-# LINE 80 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pp
              {-# LINE 5208 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _pp =
             ({-# LINE 92 "src/Language/LLVMIR/Printer/Module.ag" #-}
              pConvOp _idIpp "zext"     _vIpp _tyIpp
              {-# LINE 5213 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ZExt _pcIself _idIself _vIself _tyIself
         _lhsOself =
             _self
         ( _pcIself) =
             pc_
         ( _idIpp,_idIself) =
             id_
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
-- Instructions ------------------------------------------------
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = ( Doc,Doc,Instructions)
data Inh_Instructions = Inh_Instructions {}
data Syn_Instructions = Syn_Instructions {pp_Syn_Instructions :: Doc,ppccfg_Syn_Instructions :: Doc,self_Syn_Instructions :: Instructions}
wrap_Instructions :: T_Instructions ->
                     Inh_Instructions ->
                     Syn_Instructions
wrap_Instructions sem (Inh_Instructions) =
    (let ( _lhsOpp,_lhsOppccfg,_lhsOself) = sem
     in  (Syn_Instructions _lhsOpp _lhsOppccfg _lhsOself))
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    (let _lhsOpp :: Doc
         _lhsOppccfg :: Doc
         _lhsOself :: Instructions
         _hdIpp :: Doc
         _hdIppccfg :: Doc
         _hdIself :: Instruction
         _tlIpp :: Doc
         _tlIppccfg :: Doc
         _tlIself :: Instructions
         _lhsOpp =
             ({-# LINE 37 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _hdIpp <$> _tlIpp
              {-# LINE 5260 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOppccfg =
             ({-# LINE 31 "src/Concurrent/Model/Visualizer.ag" #-}
              _hdIppccfg <$> _tlIppccfg
              {-# LINE 5265 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIpp,_hdIppccfg,_hdIself) =
             hd_
         ( _tlIpp,_tlIppccfg,_tlIself) =
             tl_
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    (let _lhsOpp :: Doc
         _lhsOppccfg :: Doc
         _lhsOself :: Instructions
         _lhsOpp =
             ({-# LINE 37 "src/Language/LLVMIR/Printer/Module.ag" #-}
              P.empty
              {-# LINE 5284 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _lhsOppccfg =
             ({-# LINE 31 "src/Concurrent/Model/Visualizer.ag" #-}
              P.empty
              {-# LINE 5289 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOppccfg,_lhsOself))
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
type T_IntPredicate = ( Doc,IntPredicate)
data Inh_IntPredicate = Inh_IntPredicate {}
data Syn_IntPredicate = Syn_IntPredicate {pp_Syn_IntPredicate :: Doc,self_Syn_IntPredicate :: IntPredicate}
wrap_IntPredicate :: T_IntPredicate ->
                     Inh_IntPredicate ->
                     Syn_IntPredicate
wrap_IntPredicate sem (Inh_IntPredicate) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_IntPredicate _lhsOpp _lhsOself))
sem_IntPredicate_IntEQ :: T_IntPredicate
sem_IntPredicate_IntEQ =
    (let _lhsOpp :: Doc
         _lhsOself :: IntPredicate
         _lhsOpp =
             ({-# LINE 181 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "eq"
              {-# LINE 5337 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             IntEQ
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_IntPredicate_IntNE :: T_IntPredicate
sem_IntPredicate_IntNE =
    (let _lhsOpp :: Doc
         _lhsOself :: IntPredicate
         _lhsOpp =
             ({-# LINE 182 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ne"
              {-# LINE 5351 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             IntNE
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_IntPredicate_IntSGE :: T_IntPredicate
sem_IntPredicate_IntSGE =
    (let _lhsOpp :: Doc
         _lhsOself :: IntPredicate
         _lhsOpp =
             ({-# LINE 188 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "sge"
              {-# LINE 5365 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             IntSGE
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_IntPredicate_IntSGT :: T_IntPredicate
sem_IntPredicate_IntSGT =
    (let _lhsOpp :: Doc
         _lhsOself :: IntPredicate
         _lhsOpp =
             ({-# LINE 187 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "sgt"
              {-# LINE 5379 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             IntSGT
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_IntPredicate_IntSLE :: T_IntPredicate
sem_IntPredicate_IntSLE =
    (let _lhsOpp :: Doc
         _lhsOself :: IntPredicate
         _lhsOpp =
             ({-# LINE 190 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "sle"
              {-# LINE 5393 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             IntSLE
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_IntPredicate_IntSLT :: T_IntPredicate
sem_IntPredicate_IntSLT =
    (let _lhsOpp :: Doc
         _lhsOself :: IntPredicate
         _lhsOpp =
             ({-# LINE 189 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "slt"
              {-# LINE 5407 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             IntSLT
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_IntPredicate_IntUGE :: T_IntPredicate
sem_IntPredicate_IntUGE =
    (let _lhsOpp :: Doc
         _lhsOself :: IntPredicate
         _lhsOpp =
             ({-# LINE 184 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "uge"
              {-# LINE 5421 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             IntUGE
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_IntPredicate_IntUGT :: T_IntPredicate
sem_IntPredicate_IntUGT =
    (let _lhsOpp :: Doc
         _lhsOself :: IntPredicate
         _lhsOpp =
             ({-# LINE 183 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ugt"
              {-# LINE 5435 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             IntUGT
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_IntPredicate_IntULE :: T_IntPredicate
sem_IntPredicate_IntULE =
    (let _lhsOpp :: Doc
         _lhsOself :: IntPredicate
         _lhsOpp =
             ({-# LINE 186 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ule"
              {-# LINE 5449 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             IntULE
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_IntPredicate_IntULT :: T_IntPredicate
sem_IntPredicate_IntULT =
    (let _lhsOpp :: Doc
         _lhsOself :: IntPredicate
         _lhsOpp =
             ({-# LINE 185 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ult"
              {-# LINE 5463 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             IntULT
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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
         _x1Ipp :: Doc
         _x1Iself :: Type
         _x2Ipp :: Doc
         _x2Iself :: Value
         _x3Ipp :: Doc
         _x3Iself :: Identifier
         _self =
             (_x1Iself,_x2Iself,_x3Iself)
         _lhsOself =
             _self
         ( _x1Ipp,_x1Iself) =
             x1_
         ( _x2Ipp,_x2Iself) =
             x2_
         ( _x3Ipp,_x3Iself) =
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
type T_Ints = ( Doc,Ints)
data Inh_Ints = Inh_Ints {}
data Syn_Ints = Syn_Ints {pp_Syn_Ints :: Doc,self_Syn_Ints :: Ints}
wrap_Ints :: T_Ints ->
             Inh_Ints ->
             Syn_Ints
wrap_Ints sem (Inh_Ints) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Ints _lhsOpp _lhsOself))
sem_Ints_Cons :: Int ->
                 T_Ints ->
                 T_Ints
sem_Ints_Cons hd_ tl_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Ints
         _tlIpp :: Doc
         _tlIself :: Ints
         _lhsOpp =
             ({-# LINE 30 "src/Language/LLVMIR/Printer/Module.ag" #-}
              if (length _tlIself == 0)
              then int hd_
              else int hd_ <> char ',' <+> _tlIpp
              {-# LINE 5578 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             (:) hd_ _tlIself
         _lhsOself =
             _self
         ( _tlIpp,_tlIself) =
             tl_
     in  ( _lhsOpp,_lhsOself))
sem_Ints_Nil :: T_Ints
sem_Ints_Nil =
    (let _lhsOpp :: Doc
         _lhsOself :: Ints
         _lhsOpp =
             ({-# LINE 29 "src/Language/LLVMIR/Printer/Module.ag" #-}
              P.empty
              {-# LINE 5594 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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
type T_Linkage = ( Doc,Linkage)
data Inh_Linkage = Inh_Linkage {}
data Syn_Linkage = Syn_Linkage {pp_Syn_Linkage :: Doc,self_Syn_Linkage :: Linkage}
wrap_Linkage :: T_Linkage ->
                Inh_Linkage ->
                Syn_Linkage
wrap_Linkage sem (Inh_Linkage) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Linkage _lhsOpp _lhsOself))
sem_Linkage_AppendingLinkage :: T_Linkage
sem_Linkage_AppendingLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 217 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "appending"
              {-# LINE 5681 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             AppendingLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_AvailableExternallyLinkage :: T_Linkage
sem_Linkage_AvailableExternallyLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 212 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "available_externally"
              {-# LINE 5695 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             AvailableExternallyLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_CommonLinkage :: T_Linkage
sem_Linkage_CommonLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 224 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "common"
              {-# LINE 5709 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             CommonLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_DLLExportLinkage :: T_Linkage
sem_Linkage_DLLExportLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 221 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "dllexport"
              {-# LINE 5723 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             DLLExportLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_DLLImportLinkage :: T_Linkage
sem_Linkage_DLLImportLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 220 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "dllimport"
              {-# LINE 5737 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             DLLImportLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_ExternalLinkage :: T_Linkage
sem_Linkage_ExternalLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 211 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "external"
              {-# LINE 5751 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ExternalLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_ExternalWeakLinkage :: T_Linkage
sem_Linkage_ExternalWeakLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 222 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "external"
              {-# LINE 5765 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ExternalWeakLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_GhostLinkage :: T_Linkage
sem_Linkage_GhostLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 223 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ghost"
              {-# LINE 5779 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             GhostLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_InternalLinkage :: T_Linkage
sem_Linkage_InternalLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 218 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "internal"
              {-# LINE 5793 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             InternalLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_LinkOnceAnyLinkage :: T_Linkage
sem_Linkage_LinkOnceAnyLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 213 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "linkonce"
              {-# LINE 5807 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LinkOnceAnyLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_LinkOnceODRLinkage :: T_Linkage
sem_Linkage_LinkOnceODRLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 214 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "linkonce_odr"
              {-# LINE 5821 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LinkOnceODRLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_LinkerPrivateLinkage :: T_Linkage
sem_Linkage_LinkerPrivateLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 225 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "linker_private"
              {-# LINE 5835 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LinkerPrivateLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_LinkerPrivateWeakDefAutoLinkage :: T_Linkage
sem_Linkage_LinkerPrivateWeakDefAutoLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 227 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "linker_private_weak_def_auto"
              {-# LINE 5849 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LinkerPrivateWeakDefAutoLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_LinkerPrivateWeakLinkage :: T_Linkage
sem_Linkage_LinkerPrivateWeakLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 226 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "linker_private_weak"
              {-# LINE 5863 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LinkerPrivateWeakLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_PrivateLinkage :: T_Linkage
sem_Linkage_PrivateLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 219 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "private"
              {-# LINE 5877 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             PrivateLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_WeakAnyLinkage :: T_Linkage
sem_Linkage_WeakAnyLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 215 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "weak"
              {-# LINE 5891 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             WeakAnyLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Linkage_WeakODRLinkage :: T_Linkage
sem_Linkage_WeakODRLinkage =
    (let _lhsOpp :: Doc
         _lhsOself :: Linkage
         _lhsOpp =
             ({-# LINE 216 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "weak_odr"
              {-# LINE 5905 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             WeakODRLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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
         _justIpp :: Doc
         _justIself :: Align
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIpp,_justIself) =
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
         _justIpp :: Doc
         _justIself :: Identifier
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIpp,_justIself) =
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
         _justIpp :: Doc
         _justIself :: Linkage
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIpp,_justIself) =
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
type T_MValue = ( Doc,MValue)
data Inh_MValue = Inh_MValue {}
data Syn_MValue = Syn_MValue {pp_Syn_MValue :: Doc,self_Syn_MValue :: MValue}
wrap_MValue :: T_MValue ->
               Inh_MValue ->
               Syn_MValue
wrap_MValue sem (Inh_MValue) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_MValue _lhsOpp _lhsOself))
sem_MValue_Just :: T_Value ->
                   T_MValue
sem_MValue_Just just_ =
    (let _lhsOpp :: Doc
         _lhsOself :: MValue
         _justIpp :: Doc
         _justIself :: Value
         _lhsOpp =
             ({-# LINE 234 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _justIpp
              {-# LINE 6431 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIpp,_justIself) =
             just_
     in  ( _lhsOpp,_lhsOself))
sem_MValue_Nothing :: T_MValue
sem_MValue_Nothing =
    (let _lhsOpp :: Doc
         _lhsOself :: MValue
         _lhsOpp =
             ({-# LINE 233 "src/Language/LLVMIR/Printer/Module.ag" #-}
              P.empty
              {-# LINE 6447 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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
type T_Module = ( Doc,Module)
data Inh_Module = Inh_Module {}
data Syn_Module = Syn_Module {pp_Syn_Module :: Doc,self_Syn_Module :: Module}
wrap_Module :: T_Module ->
               Inh_Module ->
               Syn_Module
wrap_Module sem (Inh_Module) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Module _lhsOpp _lhsOself))
sem_Module_Module :: String ->
                     T_DataLayout ->
                     T_TargetData ->
                     T_Globals ->
                     T_Functions ->
                     T_NamedTypes ->
                     T_Module
sem_Module_Module id_ layout_ target_ gvars_ funs_ nmdtys_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Module
         _funsOilabel :: ILabel
         _layoutIpp :: Doc
         _layoutIself :: DataLayout
         _targetIpp :: Doc
         _targetIself :: TargetData
         _gvarsIpp :: Doc
         _gvarsIself :: Globals
         _funsIilabel :: ILabel
         _funsIpp :: Doc
         _funsIself :: Functions
         _nmdtysIpp :: Doc
         _nmdtysIself :: NamedTypes
         _lhsOpp =
             ({-# LINE 59 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text ("; ModuleID ='" ++ id_ ++ "'") <$> _layoutIpp <$> _targetIpp <$> P.empty <$>  _nmdtysIpp <$> _gvarsIpp <$> _funsIpp
              {-# LINE 6574 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Module id_ _layoutIself _targetIself _gvarsIself _funsIself _nmdtysIself
         _lhsOself =
             _self
         _funsOilabel =
             ({-# LINE 28 "src/Concurrent/Model/Visualizer.ag" #-}
              error "missing rule: Module.Module.funs.ilabel"
              {-# LINE 6583 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         ( _layoutIpp,_layoutIself) =
             layout_
         ( _targetIpp,_targetIself) =
             target_
         ( _gvarsIpp,_gvarsIself) =
             gvars_
         ( _funsIilabel,_funsIpp,_funsIself) =
             funs_ _funsOilabel
         ( _nmdtysIpp,_nmdtysIself) =
             nmdtys_
     in  ( _lhsOpp,_lhsOself))
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
type T_NamedTypes = ( Doc,NamedTypes)
data Inh_NamedTypes = Inh_NamedTypes {}
data Syn_NamedTypes = Syn_NamedTypes {pp_Syn_NamedTypes :: Doc,self_Syn_NamedTypes :: NamedTypes}
wrap_NamedTypes :: T_NamedTypes ->
                   Inh_NamedTypes ->
                   Syn_NamedTypes
wrap_NamedTypes sem (Inh_NamedTypes) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_NamedTypes _lhsOpp _lhsOself))
sem_NamedTypes_Entry :: Id ->
                        T_Type ->
                        T_NamedTypes ->
                        T_NamedTypes
sem_NamedTypes_Entry key_ val_ tl_ =
    (let _lhsOpp :: Doc
         _lhsOself :: NamedTypes
         _valIpp :: Doc
         _valIself :: Type
         _tlIpp :: Doc
         _tlIself :: NamedTypes
         _lhsOpp =
             ({-# LINE 152 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _valIpp <$> _tlIpp
              {-# LINE 6691 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Data.Map.insert key_ _valIself _tlIself
         _lhsOself =
             _self
         ( _valIpp,_valIself) =
             val_
         ( _tlIpp,_tlIself) =
             tl_
     in  ( _lhsOpp,_lhsOself))
sem_NamedTypes_Nil :: T_NamedTypes
sem_NamedTypes_Nil =
    (let _lhsOpp :: Doc
         _lhsOself :: NamedTypes
         _lhsOpp =
             ({-# LINE 151 "src/Language/LLVMIR/Printer/Module.ag" #-}
              P.empty
              {-# LINE 6709 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Data.Map.empty
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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
         _x1Ipp :: Doc
         _x1Iself :: Type
         _self =
             (_x1Iself,x2_)
         _lhsOself =
             _self
         ( _x1Ipp,_x1Iself) =
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
type T_PValue = ( Doc,PValue)
data Inh_PValue = Inh_PValue {}
data Syn_PValue = Syn_PValue {pp_Syn_PValue :: Doc,self_Syn_PValue :: PValue}
wrap_PValue :: T_PValue ->
               Inh_PValue ->
               Syn_PValue
wrap_PValue sem (Inh_PValue) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_PValue _lhsOpp _lhsOself))
sem_PValue_Tuple :: T_Value ->
                    T_Value ->
                    T_PValue
sem_PValue_Tuple x1_ x2_ =
    (let _lhsOpp :: Doc
         _lhsOself :: PValue
         _x1Ipp :: Doc
         _x1Iself :: Value
         _x2Ipp :: Doc
         _x2Iself :: Value
         _lhsOpp =
             ({-# LINE 34 "src/Language/LLVMIR/Printer/Module.ag" #-}
              char '[' <+> _x1Ipp <> char ',' <+> _x2Ipp <+> char ']'
              {-# LINE 6840 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             (_x1Iself,_x2Iself)
         _lhsOself =
             _self
         ( _x1Ipp,_x1Iself) =
             x1_
         ( _x2Ipp,_x2Iself) =
             x2_
     in  ( _lhsOpp,_lhsOself))
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
         _x1Ipp :: Doc
         _x1Iself :: Value
         _self =
             (_x1Iself,x2_)
         _lhsOself =
             _self
         ( _x1Ipp,_x1Iself) =
             x1_
     in  ( _lhsOself))
-- PValues -----------------------------------------------------
-- cata
sem_PValues :: PValues ->
               T_PValues
sem_PValues list =
    (Prelude.foldr sem_PValues_Cons sem_PValues_Nil (Prelude.map sem_PValue list))
-- semantic domain
type T_PValues = ( Doc,PValues)
data Inh_PValues = Inh_PValues {}
data Syn_PValues = Syn_PValues {pp_Syn_PValues :: Doc,self_Syn_PValues :: PValues}
wrap_PValues :: T_PValues ->
                Inh_PValues ->
                Syn_PValues
wrap_PValues sem (Inh_PValues) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_PValues _lhsOpp _lhsOself))
sem_PValues_Cons :: T_PValue ->
                    T_PValues ->
                    T_PValues
sem_PValues_Cons hd_ tl_ =
    (let _lhsOpp :: Doc
         _lhsOself :: PValues
         _hdIpp :: Doc
         _hdIself :: PValue
         _tlIpp :: Doc
         _tlIself :: PValues
         _lhsOpp =
             ({-# LINE 24 "src/Language/LLVMIR/Printer/Module.ag" #-}
              if (length _tlIself == 0)
              then _hdIpp
              else _hdIpp <> char ',' <+> _tlIpp
              {-# LINE 6912 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIpp,_hdIself) =
             hd_
         ( _tlIpp,_tlIself) =
             tl_
     in  ( _lhsOpp,_lhsOself))
sem_PValues_Nil :: T_PValues
sem_PValues_Nil =
    (let _lhsOpp :: Doc
         _lhsOself :: PValues
         _lhsOpp =
             ({-# LINE 23 "src/Language/LLVMIR/Printer/Module.ag" #-}
              P.empty
              {-# LINE 6930 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Parameter ---------------------------------------------------
-- cata
sem_Parameter :: Parameter ->
                 T_Parameter
sem_Parameter (Parameter _var _ty) =
    (sem_Parameter_Parameter (sem_Id _var) (sem_Type _ty))
-- semantic domain
type T_Parameter = ( Doc,Parameter)
data Inh_Parameter = Inh_Parameter {}
data Syn_Parameter = Syn_Parameter {pp_Syn_Parameter :: Doc,self_Syn_Parameter :: Parameter}
wrap_Parameter :: T_Parameter ->
                  Inh_Parameter ->
                  Syn_Parameter
wrap_Parameter sem (Inh_Parameter) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Parameter _lhsOpp _lhsOself))
sem_Parameter_Parameter :: T_Id ->
                           T_Type ->
                           T_Parameter
sem_Parameter_Parameter var_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Parameter
         _varIself :: Id
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 69 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <> ppPName _varIself
              {-# LINE 6965 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Parameter _varIself _tyIself
         _lhsOself =
             _self
         ( _varIself) =
             var_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
-- Parameters --------------------------------------------------
-- cata
sem_Parameters :: Parameters ->
                  T_Parameters
sem_Parameters list =
    (Prelude.foldr sem_Parameters_Cons sem_Parameters_Nil (Prelude.map sem_Parameter list))
-- semantic domain
type T_Parameters = ( Doc,Parameters)
data Inh_Parameters = Inh_Parameters {}
data Syn_Parameters = Syn_Parameters {pp_Syn_Parameters :: Doc,self_Syn_Parameters :: Parameters}
wrap_Parameters :: T_Parameters ->
                   Inh_Parameters ->
                   Syn_Parameters
wrap_Parameters sem (Inh_Parameters) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Parameters _lhsOpp _lhsOself))
sem_Parameters_Cons :: T_Parameter ->
                       T_Parameters ->
                       T_Parameters
sem_Parameters_Cons hd_ tl_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Parameters
         _hdIpp :: Doc
         _hdIself :: Parameter
         _tlIpp :: Doc
         _tlIself :: Parameters
         _lhsOpp =
             ({-# LINE 24 "src/Language/LLVMIR/Printer/Module.ag" #-}
              if (length _tlIself == 0)
              then _hdIpp
              else _hdIpp <> char ',' <+> _tlIpp
              {-# LINE 7007 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIpp,_hdIself) =
             hd_
         ( _tlIpp,_tlIself) =
             tl_
     in  ( _lhsOpp,_lhsOself))
sem_Parameters_Nil :: T_Parameters
sem_Parameters_Nil =
    (let _lhsOpp :: Doc
         _lhsOself :: Parameters
         _lhsOpp =
             ({-# LINE 23 "src/Language/LLVMIR/Printer/Module.ag" #-}
              P.empty
              {-# LINE 7025 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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
type T_RealPredicate = ( Doc,RealPredicate)
data Inh_RealPredicate = Inh_RealPredicate {}
data Syn_RealPredicate = Syn_RealPredicate {pp_Syn_RealPredicate :: Doc,self_Syn_RealPredicate :: RealPredicate}
wrap_RealPredicate :: T_RealPredicate ->
                      Inh_RealPredicate ->
                      Syn_RealPredicate
wrap_RealPredicate sem (Inh_RealPredicate) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_RealPredicate _lhsOpp _lhsOself))
sem_RealPredicate_LLVMRealOEQ :: T_RealPredicate
sem_RealPredicate_LLVMRealOEQ =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 194 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "oeq"
              {-# LINE 7085 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealOEQ
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealOGE :: T_RealPredicate
sem_RealPredicate_LLVMRealOGE =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 196 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "oge"
              {-# LINE 7099 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealOGE
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealOGT :: T_RealPredicate
sem_RealPredicate_LLVMRealOGT =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 195 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ogt"
              {-# LINE 7113 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealOGT
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealOLE :: T_RealPredicate
sem_RealPredicate_LLVMRealOLE =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 198 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ole"
              {-# LINE 7127 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealOLE
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealOLT :: T_RealPredicate
sem_RealPredicate_LLVMRealOLT =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 197 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "olt"
              {-# LINE 7141 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealOLT
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealONE :: T_RealPredicate
sem_RealPredicate_LLVMRealONE =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 199 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "one"
              {-# LINE 7155 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealONE
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealORD :: T_RealPredicate
sem_RealPredicate_LLVMRealORD =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 200 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ord"
              {-# LINE 7169 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealORD
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealPredicateFalse :: T_RealPredicate
sem_RealPredicate_LLVMRealPredicateFalse =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 193 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "false"
              {-# LINE 7183 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealPredicateFalse
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealPredicateTrue :: T_RealPredicate
sem_RealPredicate_LLVMRealPredicateTrue =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 208 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "true"
              {-# LINE 7197 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealPredicateTrue
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealUEQ :: T_RealPredicate
sem_RealPredicate_LLVMRealUEQ =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 202 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ueq"
              {-# LINE 7211 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealUEQ
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealUGE :: T_RealPredicate
sem_RealPredicate_LLVMRealUGE =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 204 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "uge"
              {-# LINE 7225 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealUGE
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealUGT :: T_RealPredicate
sem_RealPredicate_LLVMRealUGT =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 203 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ugt"
              {-# LINE 7239 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealUGT
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealULE :: T_RealPredicate
sem_RealPredicate_LLVMRealULE =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 206 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ule"
              {-# LINE 7253 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealULE
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealULT :: T_RealPredicate
sem_RealPredicate_LLVMRealULT =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 205 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ult"
              {-# LINE 7267 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealULT
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealUNE :: T_RealPredicate
sem_RealPredicate_LLVMRealUNE =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 207 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "une"
              {-# LINE 7281 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealUNE
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_RealPredicate_LLVMRealUNO :: T_RealPredicate
sem_RealPredicate_LLVMRealUNO =
    (let _lhsOpp :: Doc
         _lhsOself :: RealPredicate
         _lhsOpp =
             ({-# LINE 201 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "uno"
              {-# LINE 7295 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             LLVMRealUNO
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- RetInst -----------------------------------------------------
-- cata
sem_RetInst :: RetInst ->
               T_RetInst
sem_RetInst (ValueRet _v) =
    (sem_RetInst_ValueRet (sem_Value _v))
sem_RetInst (VoidRet) =
    (sem_RetInst_VoidRet)
-- semantic domain
type T_RetInst = ( Doc,RetInst)
data Inh_RetInst = Inh_RetInst {}
data Syn_RetInst = Syn_RetInst {pp_Syn_RetInst :: Doc,self_Syn_RetInst :: RetInst}
wrap_RetInst :: T_RetInst ->
                Inh_RetInst ->
                Syn_RetInst
wrap_RetInst sem (Inh_RetInst) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_RetInst _lhsOpp _lhsOself))
sem_RetInst_ValueRet :: T_Value ->
                        T_RetInst
sem_RetInst_ValueRet v_ =
    (let _lhsOpp :: Doc
         _lhsOself :: RetInst
         _vIpp :: Doc
         _vIself :: Value
         _lhsOpp =
             ({-# LINE 75 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _vIpp
              {-# LINE 7330 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             ValueRet _vIself
         _lhsOself =
             _self
         ( _vIpp,_vIself) =
             v_
     in  ( _lhsOpp,_lhsOself))
sem_RetInst_VoidRet :: T_RetInst
sem_RetInst_VoidRet =
    (let _lhsOpp :: Doc
         _lhsOself :: RetInst
         _lhsOpp =
             ({-# LINE 76 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "void"
              {-# LINE 7346 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             VoidRet
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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
type T_Target = ( Doc,Target)
data Inh_Target = Inh_Target {}
data Syn_Target = Syn_Target {pp_Syn_Target :: Doc,self_Syn_Target :: Target}
wrap_Target :: T_Target ->
               Inh_Target ->
               Syn_Target
wrap_Target sem (Inh_Target) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Target _lhsOpp _lhsOself))
sem_Target_Linux :: T_Target
sem_Target_Linux =
    (let _lhsOpp :: Doc
         _lhsOself :: Target
         _lhsOpp =
             ({-# LINE 56 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "Linux"
              {-# LINE 7403 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Linux
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Target_MacOs :: T_Target
sem_Target_MacOs =
    (let _lhsOpp :: Doc
         _lhsOself :: Target
         _lhsOpp =
             ({-# LINE 55 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "MacOs"
              {-# LINE 7417 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             MacOs
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- TargetData --------------------------------------------------
-- cata
sem_TargetData :: TargetData ->
                  T_TargetData
sem_TargetData (TargetData _s _t) =
    (sem_TargetData_TargetData _s (sem_Target _t))
-- semantic domain
type T_TargetData = ( Doc,TargetData)
data Inh_TargetData = Inh_TargetData {}
data Syn_TargetData = Syn_TargetData {pp_Syn_TargetData :: Doc,self_Syn_TargetData :: TargetData}
wrap_TargetData :: T_TargetData ->
                   Inh_TargetData ->
                   Syn_TargetData
wrap_TargetData sem (Inh_TargetData) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_TargetData _lhsOpp _lhsOself))
sem_TargetData_TargetData :: String ->
                             T_Target ->
                             T_TargetData
sem_TargetData_TargetData s_ t_ =
    (let _lhsOpp :: Doc
         _lhsOself :: TargetData
         _tIpp :: Doc
         _tIself :: Target
         _lhsOpp =
             ({-# LINE 52 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "target triple =" <+> dquotes (text s_)
              {-# LINE 7451 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TargetData s_ _tIself
         _lhsOself =
             _self
         ( _tIpp,_tIself) =
             t_
     in  ( _lhsOpp,_lhsOself))
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
type T_TyFloatPoint = ( Doc,TyFloatPoint)
data Inh_TyFloatPoint = Inh_TyFloatPoint {}
data Syn_TyFloatPoint = Syn_TyFloatPoint {pp_Syn_TyFloatPoint :: Doc,self_Syn_TyFloatPoint :: TyFloatPoint}
wrap_TyFloatPoint :: T_TyFloatPoint ->
                     Inh_TyFloatPoint ->
                     Syn_TyFloatPoint
wrap_TyFloatPoint sem (Inh_TyFloatPoint) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_TyFloatPoint _lhsOpp _lhsOself))
sem_TyFloatPoint_TyDouble :: T_TyFloatPoint
sem_TyFloatPoint_TyDouble =
    (let _lhsOpp :: Doc
         _lhsOself :: TyFloatPoint
         _lhsOpp =
             ({-# LINE 301 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "double"
              {-# LINE 7520 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyDouble
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_TyFloatPoint_TyFP128 :: T_TyFloatPoint
sem_TyFloatPoint_TyFP128 =
    (let _lhsOpp :: Doc
         _lhsOself :: TyFloatPoint
         _lhsOpp =
             ({-# LINE 302 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "fp128"
              {-# LINE 7534 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyFP128
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_TyFloatPoint_TyFloat :: T_TyFloatPoint
sem_TyFloatPoint_TyFloat =
    (let _lhsOpp :: Doc
         _lhsOself :: TyFloatPoint
         _lhsOpp =
             ({-# LINE 300 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "float"
              {-# LINE 7548 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyFloat
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_TyFloatPoint_TyHalf :: T_TyFloatPoint
sem_TyFloatPoint_TyHalf =
    (let _lhsOpp :: Doc
         _lhsOself :: TyFloatPoint
         _lhsOpp =
             ({-# LINE 299 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "half"
              {-# LINE 7562 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyHalf
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_TyFloatPoint_TyPPCFP128 :: T_TyFloatPoint
sem_TyFloatPoint_TyPPCFP128 =
    (let _lhsOpp :: Doc
         _lhsOself :: TyFloatPoint
         _lhsOpp =
             ({-# LINE 304 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "ppc_fp128"
              {-# LINE 7576 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyPPCFP128
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_TyFloatPoint_Tyx86FP80 :: T_TyFloatPoint
sem_TyFloatPoint_Tyx86FP80 =
    (let _lhsOpp :: Doc
         _lhsOself :: TyFloatPoint
         _lhsOpp =
             ({-# LINE 303 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "x86_fp80"
              {-# LINE 7590 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Tyx86FP80
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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
type T_Type = ( Doc,Type)
data Inh_Type = Inh_Type {}
data Syn_Type = Syn_Type {pp_Syn_Type :: Doc,self_Syn_Type :: Type}
wrap_Type :: T_Type ->
             Inh_Type ->
             Syn_Type
wrap_Type sem (Inh_Type) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Type _lhsOpp _lhsOself))
sem_Type_TyArray :: Int ->
                    T_Type ->
                    T_Type
sem_Type_TyArray numEl_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 291 "src/Language/LLVMIR/Printer/Module.ag" #-}
              brackets $ int numEl_ <+> char 'x' <+> _tyIpp
              {-# LINE 7648 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyArray numEl_ _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyFloatPoint :: T_TyFloatPoint ->
                         T_Type
sem_Type_TyFloatPoint p_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _pIpp :: Doc
         _pIself :: TyFloatPoint
         _lhsOpp =
             ({-# LINE 290 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _pIpp
              {-# LINE 7667 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyFloatPoint _pIself
         _lhsOself =
             _self
         ( _pIpp,_pIself) =
             p_
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyFunction :: T_Types ->
                       T_Type ->
                       T_Type
sem_Type_TyFunction party_ retty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _partyIpp :: Doc
         _partyIself :: Types
         _rettyIpp :: Doc
         _rettyIself :: Type
         _lhsOpp =
             ({-# LINE 292 "src/Language/LLVMIR/Printer/Module.ag" #-}
              parens $ _partyIpp <+> text "->" <+> _rettyIpp
              {-# LINE 7689 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyFunction _partyIself _rettyIself
         _lhsOself =
             _self
         ( _partyIpp,_partyIself) =
             party_
         ( _rettyIpp,_rettyIself) =
             retty_
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyInt :: Int ->
                  T_Type
sem_Type_TyInt p_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _lhsOpp =
             ({-# LINE 289 "src/Language/LLVMIR/Printer/Module.ag" #-}
              char 'i' <> int p_
              {-# LINE 7708 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyInt p_
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyLabel :: T_Type
sem_Type_TyLabel =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _lhsOpp =
             ({-# LINE 286 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "label"
              {-# LINE 7722 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyLabel
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyMetadata :: T_Type
sem_Type_TyMetadata =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _lhsOpp =
             ({-# LINE 287 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "metadata"
              {-# LINE 7736 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyMetadata
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyOpaque :: T_Type
sem_Type_TyOpaque =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _lhsOpp =
             ({-# LINE 288 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "opaque"
              {-# LINE 7750 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyOpaque
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyPointer :: T_Type ->
                      T_Type
sem_Type_TyPointer ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 294 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <> char '*'
              {-# LINE 7767 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyPointer _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyStruct :: String ->
                     Int ->
                     T_Types ->
                     T_Type
sem_Type_TyStruct name_ numEl_ tys_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _tysIpp :: Doc
         _tysIself :: Types
         _lhsOpp =
             ({-# LINE 293 "src/Language/LLVMIR/Printer/Module.ag" #-}
              char '%' <> text name_ <+> int numEl_ <+> braces _tysIpp
              {-# LINE 7788 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyStruct name_ numEl_ _tysIself
         _lhsOself =
             _self
         ( _tysIpp,_tysIself) =
             tys_
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyUndefined :: T_Type
sem_Type_TyUndefined =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _lhsOpp =
             ({-# LINE 296 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "TODO TYPE UNDEFINED"
              {-# LINE 7804 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyUndefined
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyVector :: Int ->
                     T_Type ->
                     T_Type
sem_Type_TyVector numEl_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 295 "src/Language/LLVMIR/Printer/Module.ag" #-}
              char '<' <> int numEl_ <+> char 'x' <+> _tyIpp <> char '>'
              {-# LINE 7822 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyVector numEl_ _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyVoid :: T_Type
sem_Type_TyVoid =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _lhsOpp =
             ({-# LINE 284 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "void"
              {-# LINE 7838 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             TyVoid
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Type_Tyx86MMX :: T_Type
sem_Type_Tyx86MMX =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _lhsOpp =
             ({-# LINE 285 "src/Language/LLVMIR/Printer/Module.ag" #-}
              text "x86mmx"
              {-# LINE 7852 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Tyx86MMX
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Types -------------------------------------------------------
-- cata
sem_Types :: Types ->
             T_Types
sem_Types list =
    (Prelude.foldr sem_Types_Cons sem_Types_Nil (Prelude.map sem_Type list))
-- semantic domain
type T_Types = ( Doc,Types)
data Inh_Types = Inh_Types {}
data Syn_Types = Syn_Types {pp_Syn_Types :: Doc,self_Syn_Types :: Types}
wrap_Types :: T_Types ->
              Inh_Types ->
              Syn_Types
wrap_Types sem (Inh_Types) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Types _lhsOpp _lhsOself))
sem_Types_Cons :: T_Type ->
                  T_Types ->
                  T_Types
sem_Types_Cons hd_ tl_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Types
         _hdIpp :: Doc
         _hdIself :: Type
         _tlIpp :: Doc
         _tlIself :: Types
         _lhsOpp =
             ({-# LINE 24 "src/Language/LLVMIR/Printer/Module.ag" #-}
              if (length _tlIself == 0)
              then _hdIpp
              else _hdIpp <> char ',' <+> _tlIpp
              {-# LINE 7890 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIpp,_hdIself) =
             hd_
         ( _tlIpp,_tlIself) =
             tl_
     in  ( _lhsOpp,_lhsOself))
sem_Types_Nil :: T_Types
sem_Types_Nil =
    (let _lhsOpp :: Doc
         _lhsOself :: Types
         _lhsOpp =
             ({-# LINE 23 "src/Language/LLVMIR/Printer/Module.ag" #-}
              P.empty
              {-# LINE 7908 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Value -------------------------------------------------------
-- cata
sem_Value :: Value ->
             T_Value
sem_Value (Constant _c) =
    (sem_Value_Constant (sem_Constant _c))
sem_Value (Id _v _ty) =
    (sem_Value_Id (sem_Identifier _v) (sem_Type _ty))
-- semantic domain
type T_Value = ( Doc,Value)
data Inh_Value = Inh_Value {}
data Syn_Value = Syn_Value {pp_Syn_Value :: Doc,self_Syn_Value :: Value}
wrap_Value :: T_Value ->
              Inh_Value ->
              Syn_Value
wrap_Value sem (Inh_Value) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Value _lhsOpp _lhsOself))
sem_Value_Constant :: T_Constant ->
                      T_Value
sem_Value_Constant c_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Value
         _cIpp :: Doc
         _cIself :: Constant
         _lhsOpp =
             ({-# LINE 238 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _cIpp
              {-# LINE 7943 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Constant _cIself
         _lhsOself =
             _self
         ( _cIpp,_cIself) =
             c_
     in  ( _lhsOpp,_lhsOself))
sem_Value_Id :: T_Identifier ->
                T_Type ->
                T_Value
sem_Value_Id v_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Value
         _vIpp :: Doc
         _vIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 237 "src/Language/LLVMIR/Printer/Module.ag" #-}
              _tyIpp <+> _vIpp
              {-# LINE 7965 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             Id _vIself _tyIself
         _lhsOself =
             _self
         ( _vIpp,_vIself) =
             v_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
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
type T_Values = ( Doc,Values)
data Inh_Values = Inh_Values {}
data Syn_Values = Syn_Values {pp_Syn_Values :: Doc,self_Syn_Values :: Values}
wrap_Values :: T_Values ->
               Inh_Values ->
               Syn_Values
wrap_Values sem (Inh_Values) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Values _lhsOpp _lhsOself))
sem_Values_Cons :: T_Value ->
                   T_Values ->
                   T_Values
sem_Values_Cons hd_ tl_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Values
         _hdIpp :: Doc
         _hdIself :: Value
         _tlIpp :: Doc
         _tlIself :: Values
         _lhsOpp =
             ({-# LINE 24 "src/Language/LLVMIR/Printer/Module.ag" #-}
              if (length _tlIself == 0)
              then _hdIpp
              else _hdIpp <> char ',' <+> _tlIpp
              {-# LINE 8047 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIpp,_hdIself) =
             hd_
         ( _tlIpp,_tlIself) =
             tl_
     in  ( _lhsOpp,_lhsOself))
sem_Values_Nil :: T_Values
sem_Values_Nil =
    (let _lhsOpp :: Doc
         _lhsOself :: Values
         _lhsOpp =
             ({-# LINE 23 "src/Language/LLVMIR/Printer/Module.ag" #-}
              P.empty
              {-# LINE 8065 "src/Concurrent/Model/Visualizer.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
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