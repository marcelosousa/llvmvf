

-- UUAGC 0.9.40.3 (src/Language/LLVMIR.ag)
module Language.LLVMIR where

{-# LINE 11 "src/Language/LLVMIR/Base.ag" #-}

import Prelude              hiding (sequence)
import Data.Char            (chr)
import qualified Data.Map as Data.Map
import qualified Data.Map as Map
import Data.Map
{-# LINE 14 "src/Language/LLVMIR.hs" #-}

{-# LINE 11 "src/Language/LLVMIR/Printer.ag" #-}

import UU.PPrint as P
{-# LINE 19 "src/Language/LLVMIR.hs" #-}
{-# LINE 1 "src/Language/LLVMIR.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR
-- Copyright :  (c) 2012 Marcelo Sousa
-- This AG module imports all AG files used in the project and defines
-- the main wrap used. 
-------------------------------------------------------------------------------
{-# LINE 28 "src/Language/LLVMIR.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Base.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Base
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 36 "src/Language/LLVMIR.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Type.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Type
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 44 "src/Language/LLVMIR.hs" #-}

{-# LINE 1 "src/Language/LLVMIR/Printer.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Printer
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 52 "src/Language/LLVMIR.hs" #-}

{-# LINE 114 "src/Language/LLVMIR/Printer.ag" #-}

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

class MPretty a where
  pretty' :: a -> Doc

--instance Pretty 

instance Pretty Module where
    pretty mdl = pp_Syn_Module $ wrap_Module (sem_Module mdl) $ Inh_Module {}
    
instance Pretty DataLayout where
    pretty d = pp_Syn_DataLayout $ wrap_DataLayout (sem_DataLayout d) $ Inh_DataLayout {}

{-# LINE 81 "src/Language/LLVMIR.hs" #-}
-- Alias -------------------------------------------------------
data Alias = Alias (Id)
           deriving ( Eq,Ord,Show)
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
             ({-# LINE 56 "src/Language/LLVMIR/Printer.ag" #-}
              text "%" <> text _nameIself
              {-# LINE 109 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             Alias _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
     in  ( _lhsOpp,_lhsOself))
-- Aliases -----------------------------------------------------
type Aliases = [Alias]
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
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              _hdIpp <$> _tlIpp
              {-# LINE 148 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              P.empty
              {-# LINE 166 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Align -------------------------------------------------------
data Align = Align (Int)
           deriving ( Eq,Ord,Show)
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
             ({-# LINE 78 "src/Language/LLVMIR/Printer.ag" #-}
              text "align" <+> text (show n_)
              {-# LINE 199 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             Align n_
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Argument ----------------------------------------------------
data Argument = Argument
              deriving ( Eq,Ord,Show)
-- cata
sem_Argument :: Argument ->
                T_Argument
sem_Argument (Argument) =
    (sem_Argument_Argument)
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
sem_Argument_Argument :: T_Argument
sem_Argument_Argument =
    (let _lhsOself :: Argument
         _self =
             Argument
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Arguments ---------------------------------------------------
type Arguments = [Argument]
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
-- Attribute ---------------------------------------------------
data Attribute = AlwaysInlineAttribute
               | ByValAttribute
               | InRegAttribute
               | NakedAttribute
               | NestAttribute
               | NoAliasAttribute
               | NoCaptureAttribute
               | NoImplicitFloatAttribute
               | NoInlineAttribute
               | NoRedZoneAttribute
               | NoReturnAttribute
               | NoUnwindAttribute
               | OptimizeForSizeAttribute
               | ReadNoneAttribute
               | ReadOnlyAttribute
               | SExtAttribute
               | StackProtectAttribute
               | StackProtectReqAttribute
               | StructRetAttribute
               | ZExtAttribute
               deriving ( Eq,Ord,Show)
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
type Attributes = [Attribute]
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
data BasicBlock = BasicBlock (Label) (Instructions)
                deriving ( Eq,Ord,Show)
-- cata
sem_BasicBlock :: BasicBlock ->
                  T_BasicBlock
sem_BasicBlock (BasicBlock _label _instrs) =
    (sem_BasicBlock_BasicBlock (sem_Label _label) (sem_Instructions _instrs))
-- semantic domain
type T_BasicBlock = ( BasicBlock)
data Inh_BasicBlock = Inh_BasicBlock {}
data Syn_BasicBlock = Syn_BasicBlock {self_Syn_BasicBlock :: BasicBlock}
wrap_BasicBlock :: T_BasicBlock ->
                   Inh_BasicBlock ->
                   Syn_BasicBlock
wrap_BasicBlock sem (Inh_BasicBlock) =
    (let ( _lhsOself) = sem
     in  (Syn_BasicBlock _lhsOself))
sem_BasicBlock_BasicBlock :: T_Label ->
                             T_Instructions ->
                             T_BasicBlock
sem_BasicBlock_BasicBlock label_ instrs_ =
    (let _lhsOself :: BasicBlock
         _labelIself :: Label
         _instrsIself :: Instructions
         _self =
             BasicBlock _labelIself _instrsIself
         _lhsOself =
             _self
         ( _labelIself) =
             label_
         ( _instrsIself) =
             instrs_
     in  ( _lhsOself))
-- BasicBlocks -------------------------------------------------
type BasicBlocks = [BasicBlock]
-- cata
sem_BasicBlocks :: BasicBlocks ->
                   T_BasicBlocks
sem_BasicBlocks list =
    (Prelude.foldr sem_BasicBlocks_Cons sem_BasicBlocks_Nil (Prelude.map sem_BasicBlock list))
-- semantic domain
type T_BasicBlocks = ( BasicBlocks)
data Inh_BasicBlocks = Inh_BasicBlocks {}
data Syn_BasicBlocks = Syn_BasicBlocks {self_Syn_BasicBlocks :: BasicBlocks}
wrap_BasicBlocks :: T_BasicBlocks ->
                    Inh_BasicBlocks ->
                    Syn_BasicBlocks
wrap_BasicBlocks sem (Inh_BasicBlocks) =
    (let ( _lhsOself) = sem
     in  (Syn_BasicBlocks _lhsOself))
sem_BasicBlocks_Cons :: T_BasicBlock ->
                        T_BasicBlocks ->
                        T_BasicBlocks
sem_BasicBlocks_Cons hd_ tl_ =
    (let _lhsOself :: BasicBlocks
         _hdIself :: BasicBlock
         _tlIself :: BasicBlocks
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_BasicBlocks_Nil :: T_BasicBlocks
sem_BasicBlocks_Nil =
    (let _lhsOself :: BasicBlocks
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- CConv -------------------------------------------------------
data CConv = Cc (Int)
           | Cc10
           | Ccc
           | Coldcc
           | Fastcc
           deriving ( Eq,Ord,Show)
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
-- Constant ----------------------------------------------------
data Constant = ArrayC (Type) (String)
              | BlockAddr (Identifier) (Identifier)
              | BoolC (Bool)
              | FloatC (Float) (Type)
              | IntC (Int) (Type)
              | NullC (Type)
              | StructC (PTyIntL)
              | UndefC
              | VectorC (PTyIntL)
              | ZeroInitC (Type)
              deriving ( Eq,Ord,Show)
-- cata
sem_Constant :: Constant ->
                T_Constant
sem_Constant (ArrayC _ty _val) =
    (sem_Constant_ArrayC (sem_Type _ty) _val)
sem_Constant (BlockAddr _fun _label) =
    (sem_Constant_BlockAddr (sem_Identifier _fun) (sem_Identifier _label))
sem_Constant (BoolC _v) =
    (sem_Constant_BoolC _v)
sem_Constant (FloatC _v _ty) =
    (sem_Constant_FloatC _v (sem_Type _ty))
sem_Constant (IntC _v _ty) =
    (sem_Constant_IntC _v (sem_Type _ty))
sem_Constant (NullC _ty) =
    (sem_Constant_NullC (sem_Type _ty))
sem_Constant (StructC _elems) =
    (sem_Constant_StructC (sem_PTyIntL _elems))
sem_Constant (UndefC) =
    (sem_Constant_UndefC)
sem_Constant (VectorC _elems) =
    (sem_Constant_VectorC (sem_PTyIntL _elems))
sem_Constant (ZeroInitC _ty) =
    (sem_Constant_ZeroInitC (sem_Type _ty))
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
sem_Constant_ArrayC :: T_Type ->
                       String ->
                       T_Constant
sem_Constant_ArrayC ty_ val_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 89 "src/Language/LLVMIR/Printer.ag" #-}
              _tyIpp <+> text "c" <> dquotes (escaped val_)
              {-# LINE 751 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             ArrayC _tyIself val_
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_BlockAddr :: T_Identifier ->
                          T_Identifier ->
                          T_Constant
sem_Constant_BlockAddr fun_ label_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _funIself :: Identifier
         _labelIself :: Identifier
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              P.empty
              {-# LINE 771 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             BlockAddr _funIself _labelIself
         _lhsOself =
             _self
         ( _funIself) =
             fun_
         ( _labelIself) =
             label_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_BoolC :: Bool ->
                      T_Constant
sem_Constant_BoolC v_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              P.empty
              {-# LINE 790 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             BoolC v_
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Constant_FloatC :: Float ->
                       T_Type ->
                       T_Constant
sem_Constant_FloatC v_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              _tyIpp
              {-# LINE 808 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             FloatC v_ _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_IntC :: Int ->
                     T_Type ->
                     T_Constant
sem_Constant_IntC v_ ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              _tyIpp
              {-# LINE 828 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             IntC v_ _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_NullC :: T_Type ->
                      T_Constant
sem_Constant_NullC ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              _tyIpp
              {-# LINE 847 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             NullC _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_StructC :: T_PTyIntL ->
                        T_Constant
sem_Constant_StructC elems_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _elemsIself :: PTyIntL
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              P.empty
              {-# LINE 865 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             StructC _elemsIself
         _lhsOself =
             _self
         ( _elemsIself) =
             elems_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_UndefC :: T_Constant
sem_Constant_UndefC =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              P.empty
              {-# LINE 881 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             UndefC
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Constant_VectorC :: T_PTyIntL ->
                        T_Constant
sem_Constant_VectorC elems_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _elemsIself :: PTyIntL
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              P.empty
              {-# LINE 897 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             VectorC _elemsIself
         _lhsOself =
             _self
         ( _elemsIself) =
             elems_
     in  ( _lhsOpp,_lhsOself))
sem_Constant_ZeroInitC :: T_Type ->
                          T_Constant
sem_Constant_ZeroInitC ty_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Constant
         _tyIpp :: Doc
         _tyIself :: Type
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              _tyIpp
              {-# LINE 916 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             ZeroInitC _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
-- DLayout -----------------------------------------------------
type DLayout = [(String)]
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
data DataLayout = DataLayout (DLayout)
                deriving ( Eq,Ord,Show)
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
             ({-# LINE 32 "src/Language/LLVMIR/Printer.ag" #-}
              text "target datalayout =" <+> dquotes (Prelude.foldr1 (\x y -> x <> char '-' <> y) (Prelude.map text _sIself))
              {-# LINE 990 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             DataLayout _sIself
         _lhsOself =
             _self
         ( _sIself) =
             s_
     in  ( _lhsOpp,_lhsOself))
-- DefinitionTy ------------------------------------------------
data DefinitionTy = Constant
                  | ThreadLocal
                  deriving ( Eq,Ord,Show)
-- cata
sem_DefinitionTy :: DefinitionTy ->
                    T_DefinitionTy
sem_DefinitionTy (Constant) =
    (sem_DefinitionTy_Constant)
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
sem_DefinitionTy_Constant :: T_DefinitionTy
sem_DefinitionTy_Constant =
    (let _lhsOself :: DefinitionTy
         _self =
             Constant
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
data FunAttr = AddressSafety
             | Alignstack (Int)
             | Alwaysinline
             | Inlinehint
             | Naked
             | Noimplicitfloat
             | Noinline
             | Nonlazybind
             | Noredzone
             | Noreturn
             | Nounwind
             | Optsize
             | Readnone
             | Readonly
             | ReturnsTwice
             | Ssp
             | Sspreq
             | Uwtable
             deriving ( Eq,Ord,Show)
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
type FuncAttrs = [FunAttr]
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
data Function = FunctionDecl (Id) (Linkage) (Type) (Parameters)
              | FunctionDef (Id) (Linkage) (Type) (Parameters)
              deriving ( Eq,Ord,Show)
-- cata
sem_Function :: Function ->
                T_Function
sem_Function (FunctionDecl _name _linkage _retty _params) =
    (sem_Function_FunctionDecl (sem_Id _name) (sem_Linkage _linkage) (sem_Type _retty) (sem_Parameters _params))
sem_Function (FunctionDef _name _linkage _retty _params) =
    (sem_Function_FunctionDef (sem_Id _name) (sem_Linkage _linkage) (sem_Type _retty) (sem_Parameters _params))
-- semantic domain
type T_Function = ( Doc,Function)
data Inh_Function = Inh_Function {}
data Syn_Function = Syn_Function {pp_Syn_Function :: Doc,self_Syn_Function :: Function}
wrap_Function :: T_Function ->
                 Inh_Function ->
                 Syn_Function
wrap_Function sem (Inh_Function) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Function _lhsOpp _lhsOself))
sem_Function_FunctionDecl :: T_Id ->
                             T_Linkage ->
                             T_Type ->
                             T_Parameters ->
                             T_Function
sem_Function_FunctionDecl name_ linkage_ retty_ params_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Function
         _nameIself :: Id
         _linkageIpp :: Doc
         _linkageIself :: Linkage
         _rettyIpp :: Doc
         _rettyIself :: Type
         _paramsIpp :: Doc
         _paramsIself :: Parameters
         _lhsOpp =
             ({-# LINE 50 "src/Language/LLVMIR/Printer.ag" #-}
              text "declare" <+> _linkageIpp <+> _rettyIpp <+> char '@' <> text _nameIself <> parens _paramsIpp
              {-# LINE 1330 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             FunctionDecl _nameIself _linkageIself _rettyIself _paramsIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
         ( _linkageIpp,_linkageIself) =
             linkage_
         ( _rettyIpp,_rettyIself) =
             retty_
         ( _paramsIpp,_paramsIself) =
             params_
     in  ( _lhsOpp,_lhsOself))
sem_Function_FunctionDef :: T_Id ->
                            T_Linkage ->
                            T_Type ->
                            T_Parameters ->
                            T_Function
sem_Function_FunctionDef name_ linkage_ retty_ params_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Function
         _nameIself :: Id
         _linkageIpp :: Doc
         _linkageIself :: Linkage
         _rettyIpp :: Doc
         _rettyIself :: Type
         _paramsIpp :: Doc
         _paramsIself :: Parameters
         _lhsOpp =
             ({-# LINE 49 "src/Language/LLVMIR/Printer.ag" #-}
              text "define"  <+> _linkageIpp <+> _rettyIpp <+> char '@' <> text _nameIself <> parens _paramsIpp
              {-# LINE 1363 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             FunctionDef _nameIself _linkageIself _rettyIself _paramsIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
         ( _linkageIpp,_linkageIself) =
             linkage_
         ( _rettyIpp,_rettyIself) =
             retty_
         ( _paramsIpp,_paramsIself) =
             params_
     in  ( _lhsOpp,_lhsOself))
-- Functions ---------------------------------------------------
type Functions = [Function]
-- cata
sem_Functions :: Functions ->
                 T_Functions
sem_Functions list =
    (Prelude.foldr sem_Functions_Cons sem_Functions_Nil (Prelude.map sem_Function list))
-- semantic domain
type T_Functions = ( Doc,Functions)
data Inh_Functions = Inh_Functions {}
data Syn_Functions = Syn_Functions {pp_Syn_Functions :: Doc,self_Syn_Functions :: Functions}
wrap_Functions :: T_Functions ->
                  Inh_Functions ->
                  Syn_Functions
wrap_Functions sem (Inh_Functions) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_Functions _lhsOpp _lhsOself))
sem_Functions_Cons :: T_Function ->
                      T_Functions ->
                      T_Functions
sem_Functions_Cons hd_ tl_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Functions
         _hdIpp :: Doc
         _hdIself :: Function
         _tlIpp :: Doc
         _tlIself :: Functions
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              _hdIpp <$> _tlIpp
              {-# LINE 1408 "src/Language/LLVMIR.hs" #-}
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
sem_Functions_Nil :: T_Functions
sem_Functions_Nil =
    (let _lhsOpp :: Doc
         _lhsOself :: Functions
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              P.empty
              {-# LINE 1426 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- GCName ------------------------------------------------------
data GCName = GCName (String)
            deriving ( Eq,Ord,Show)
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
-- GlobalVar ---------------------------------------------------
data GlobalVar = GlobalVar (Id) (Linkage) (Bool) (Bool) (Align)
               deriving ( Eq,Ord,Show)
-- cata
sem_GlobalVar :: GlobalVar ->
                 T_GlobalVar
sem_GlobalVar (GlobalVar _name _linkage _isConst _isUaddr _align) =
    (sem_GlobalVar_GlobalVar (sem_Id _name) (sem_Linkage _linkage) _isConst _isUaddr (sem_Align _align))
-- semantic domain
type T_GlobalVar = ( Doc,GlobalVar)
data Inh_GlobalVar = Inh_GlobalVar {}
data Syn_GlobalVar = Syn_GlobalVar {pp_Syn_GlobalVar :: Doc,self_Syn_GlobalVar :: GlobalVar}
wrap_GlobalVar :: T_GlobalVar ->
                  Inh_GlobalVar ->
                  Syn_GlobalVar
wrap_GlobalVar sem (Inh_GlobalVar) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_GlobalVar _lhsOpp _lhsOself))
sem_GlobalVar_GlobalVar :: T_Id ->
                           T_Linkage ->
                           Bool ->
                           Bool ->
                           T_Align ->
                           T_GlobalVar
sem_GlobalVar_GlobalVar name_ linkage_ isConst_ isUaddr_ align_ =
    (let _lhsOpp :: Doc
         _lhsOself :: GlobalVar
         _nameIself :: Id
         _linkageIpp :: Doc
         _linkageIself :: Linkage
         _alignIpp :: Doc
         _alignIself :: Align
         _lhsOpp =
             ({-# LINE 46 "src/Language/LLVMIR/Printer.ag" #-}
              text "@" <> text _nameIself <+> text "=" <+>  _linkageIpp <+> ppKeyword isUaddr_ "unnamed_addr" <+> ppKeyword isConst_ "constant" <+> text "," <+> _alignIpp
              {-# LINE 1495 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             GlobalVar _nameIself _linkageIself isConst_ isUaddr_ _alignIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
         ( _linkageIpp,_linkageIself) =
             linkage_
         ( _alignIpp,_alignIself) =
             align_
     in  ( _lhsOpp,_lhsOself))
-- GlobalVars --------------------------------------------------
type GlobalVars = [GlobalVar]
-- cata
sem_GlobalVars :: GlobalVars ->
                  T_GlobalVars
sem_GlobalVars list =
    (Prelude.foldr sem_GlobalVars_Cons sem_GlobalVars_Nil (Prelude.map sem_GlobalVar list))
-- semantic domain
type T_GlobalVars = ( Doc,GlobalVars)
data Inh_GlobalVars = Inh_GlobalVars {}
data Syn_GlobalVars = Syn_GlobalVars {pp_Syn_GlobalVars :: Doc,self_Syn_GlobalVars :: GlobalVars}
wrap_GlobalVars :: T_GlobalVars ->
                   Inh_GlobalVars ->
                   Syn_GlobalVars
wrap_GlobalVars sem (Inh_GlobalVars) =
    (let ( _lhsOpp,_lhsOself) = sem
     in  (Syn_GlobalVars _lhsOpp _lhsOself))
sem_GlobalVars_Cons :: T_GlobalVar ->
                       T_GlobalVars ->
                       T_GlobalVars
sem_GlobalVars_Cons hd_ tl_ =
    (let _lhsOpp :: Doc
         _lhsOself :: GlobalVars
         _hdIpp :: Doc
         _hdIself :: GlobalVar
         _tlIpp :: Doc
         _tlIself :: GlobalVars
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              _hdIpp <$> _tlIpp
              {-# LINE 1538 "src/Language/LLVMIR.hs" #-}
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
sem_GlobalVars_Nil :: T_GlobalVars
sem_GlobalVars_Nil =
    (let _lhsOpp :: Doc
         _lhsOself :: GlobalVars
         _lhsOpp =
             ({-# LINE 25 "src/Language/LLVMIR/Printer.ag" #-}
              P.empty
              {-# LINE 1556 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Id ----------------------------------------------------------
type Id = ( (String))
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
data Identifier = Global (Id)
                | Local (Id)
                deriving ( Eq,Ord,Show)
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
type Identifiers = [Identifier]
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
data Instruction = Alloca (Identifier) (Type) (Int)
                 | Instruction (String)
                 deriving ( Eq,Ord,Show)
-- cata
sem_Instruction :: Instruction ->
                   T_Instruction
sem_Instruction (Alloca _id _ty _align) =
    (sem_Instruction_Alloca (sem_Identifier _id) (sem_Type _ty) _align)
sem_Instruction (Instruction _s) =
    (sem_Instruction_Instruction _s)
-- semantic domain
type T_Instruction = ( Instruction)
data Inh_Instruction = Inh_Instruction {}
data Syn_Instruction = Syn_Instruction {self_Syn_Instruction :: Instruction}
wrap_Instruction :: T_Instruction ->
                    Inh_Instruction ->
                    Syn_Instruction
wrap_Instruction sem (Inh_Instruction) =
    (let ( _lhsOself) = sem
     in  (Syn_Instruction _lhsOself))
sem_Instruction_Alloca :: T_Identifier ->
                          T_Type ->
                          Int ->
                          T_Instruction
sem_Instruction_Alloca id_ ty_ align_ =
    (let _lhsOself :: Instruction
         _idIself :: Identifier
         _tyIpp :: Doc
         _tyIself :: Type
         _self =
             Alloca _idIself _tyIself align_
         _lhsOself =
             _self
         ( _idIself) =
             id_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOself))
sem_Instruction_Instruction :: String ->
                               T_Instruction
sem_Instruction_Instruction s_ =
    (let _lhsOself :: Instruction
         _self =
             Instruction s_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Instructions ------------------------------------------------
type Instructions = [Instruction]
-- cata
sem_Instructions :: Instructions ->
                    T_Instructions
sem_Instructions list =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list))
-- semantic domain
type T_Instructions = ( Instructions)
data Inh_Instructions = Inh_Instructions {}
data Syn_Instructions = Syn_Instructions {self_Syn_Instructions :: Instructions}
wrap_Instructions :: T_Instructions ->
                     Inh_Instructions ->
                     Syn_Instructions
wrap_Instructions sem (Inh_Instructions) =
    (let ( _lhsOself) = sem
     in  (Syn_Instructions _lhsOself))
sem_Instructions_Cons :: T_Instruction ->
                         T_Instructions ->
                         T_Instructions
sem_Instructions_Cons hd_ tl_ =
    (let _lhsOself :: Instructions
         _hdIself :: Instruction
         _tlIself :: Instructions
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Instructions_Nil :: T_Instructions
sem_Instructions_Nil =
    (let _lhsOself :: Instructions
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- IntTyValId --------------------------------------------------
type IntTyValId = ( Type,Value,Identifier)
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
         _x3Iself :: Identifier
         _self =
             (_x1Iself,_x2Iself,_x3Iself)
         _lhsOself =
             _self
         ( _x1Ipp,_x1Iself) =
             x1_
         ( _x2Ipp,_x2Iself) =
             x2_
         ( _x3Iself) =
             x3_
     in  ( _lhsOself))
-- IntTyValIdL -------------------------------------------------
type IntTyValIdL = [IntTyValId]
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
type Ints = [(Int)]
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
type Label = ( (String))
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
data Linkage = AppendingLinkage
             | AvailableExternallyLinkage
             | CommonLinkage
             | DLLExportLinkage
             | DLLImportLinkage
             | ExternalLinkage
             | ExternalWeakLinkage
             | GhostLinkage
             | InternalLinkage
             | LinkOnceAnyLinkage
             | LinkOnceODRLinkage
             | LinkerPrivateLinkage
             | LinkerPrivateWeakDefAutoLinkage
             | LinkerPrivateWeakLinkage
             | PrivateLinkage
             | WeakAnyLinkage
             | WeakODRLinkage
             deriving ( Enum,Eq,Ord,Show)
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
             ({-# LINE 65 "src/Language/LLVMIR/Printer.ag" #-}
              text "appending"
              {-# LINE 1981 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 60 "src/Language/LLVMIR/Printer.ag" #-}
              text "available_externally"
              {-# LINE 1995 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 72 "src/Language/LLVMIR/Printer.ag" #-}
              text "common"
              {-# LINE 2009 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 69 "src/Language/LLVMIR/Printer.ag" #-}
              text "dllexport"
              {-# LINE 2023 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 68 "src/Language/LLVMIR/Printer.ag" #-}
              text "dllimport"
              {-# LINE 2037 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 59 "src/Language/LLVMIR/Printer.ag" #-}
              text "external"
              {-# LINE 2051 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 70 "src/Language/LLVMIR/Printer.ag" #-}
              text "external"
              {-# LINE 2065 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 71 "src/Language/LLVMIR/Printer.ag" #-}
              text "ghost"
              {-# LINE 2079 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 66 "src/Language/LLVMIR/Printer.ag" #-}
              text "internal"
              {-# LINE 2093 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 61 "src/Language/LLVMIR/Printer.ag" #-}
              text "linkonce"
              {-# LINE 2107 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 62 "src/Language/LLVMIR/Printer.ag" #-}
              text "linkonce_odr"
              {-# LINE 2121 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 73 "src/Language/LLVMIR/Printer.ag" #-}
              text "linker_private"
              {-# LINE 2135 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 75 "src/Language/LLVMIR/Printer.ag" #-}
              text "linker_private_weak_def_auto"
              {-# LINE 2149 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 74 "src/Language/LLVMIR/Printer.ag" #-}
              text "linker_private_weak"
              {-# LINE 2163 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 67 "src/Language/LLVMIR/Printer.ag" #-}
              text "private"
              {-# LINE 2177 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 63 "src/Language/LLVMIR/Printer.ag" #-}
              text "weak"
              {-# LINE 2191 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 64 "src/Language/LLVMIR/Printer.ag" #-}
              text "weak_odr"
              {-# LINE 2205 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             WeakODRLinkage
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- MAlign ------------------------------------------------------
type MAlign = Maybe Align
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
type MAttributes = Maybe Attributes
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
type MCConv = Maybe CConv
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
type MConstant = Maybe (Bool)
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
type MDefinitionTy = Maybe DefinitionTy
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
type MGCName = Maybe GCName
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
type MId = Maybe Id
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
-- MLabel ------------------------------------------------------
type MLabel = Maybe Label
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
type MLinkageTy = Maybe Linkage
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
type MModuleAsms = Maybe ModuleAsms
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
-- MNamedTys ---------------------------------------------------
type MNamedTys = Maybe NamedTys
-- cata
sem_MNamedTys :: MNamedTys ->
                 T_MNamedTys
sem_MNamedTys (Prelude.Just x) =
    (sem_MNamedTys_Just (sem_NamedTys x))
sem_MNamedTys Prelude.Nothing =
    sem_MNamedTys_Nothing
-- semantic domain
type T_MNamedTys = ( MNamedTys)
data Inh_MNamedTys = Inh_MNamedTys {}
data Syn_MNamedTys = Syn_MNamedTys {self_Syn_MNamedTys :: MNamedTys}
wrap_MNamedTys :: T_MNamedTys ->
                  Inh_MNamedTys ->
                  Syn_MNamedTys
wrap_MNamedTys sem (Inh_MNamedTys) =
    (let ( _lhsOself) = sem
     in  (Syn_MNamedTys _lhsOself))
sem_MNamedTys_Just :: T_NamedTys ->
                      T_MNamedTys
sem_MNamedTys_Just just_ =
    (let _lhsOself :: MNamedTys
         _justIself :: NamedTys
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_
     in  ( _lhsOself))
sem_MNamedTys_Nothing :: T_MNamedTys
sem_MNamedTys_Nothing =
    (let _lhsOself :: MNamedTys
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MSection ----------------------------------------------------
type MSection = Maybe Section
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
type MUnnamedAddr = Maybe (Bool)
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
type MValue = Maybe Value
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
             ({-# LINE 82 "src/Language/LLVMIR/Printer.ag" #-}
              _justIpp
              {-# LINE 2744 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 81 "src/Language/LLVMIR/Printer.ag" #-}
              P.empty
              {-# LINE 2760 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- MVisibility -------------------------------------------------
type MVisibility = Maybe Visibility
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
type MapTyInt = Data.Map.Map ((Type)) (Triplet)
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
data Module = Module (String) (DataLayout) (TargetData) (GlobalVars) (Functions) (Aliases)
            deriving ( Eq,Ord,Show)
-- cata
sem_Module :: Module ->
              T_Module
sem_Module (Module _id _layout _target _gvars _funs _aliases) =
    (sem_Module_Module _id (sem_DataLayout _layout) (sem_TargetData _target) (sem_GlobalVars _gvars) (sem_Functions _funs) (sem_Aliases _aliases))
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
                     T_GlobalVars ->
                     T_Functions ->
                     T_Aliases ->
                     T_Module
sem_Module_Module id_ layout_ target_ gvars_ funs_ aliases_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Module
         _layoutIpp :: Doc
         _layoutIself :: DataLayout
         _targetIpp :: Doc
         _targetIself :: TargetData
         _gvarsIpp :: Doc
         _gvarsIself :: GlobalVars
         _funsIpp :: Doc
         _funsIself :: Functions
         _aliasesIpp :: Doc
         _aliasesIself :: Aliases
         _lhsOpp =
             ({-# LINE 43 "src/Language/LLVMIR/Printer.ag" #-}
              text ("; ModuleID ='" ++ id_ ++ "'") <$> _layoutIpp <$> _targetIpp <$> _aliasesIpp <$> _gvarsIpp <$> _funsIpp
              {-# LINE 2889 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             Module id_ _layoutIself _targetIself _gvarsIself _funsIself _aliasesIself
         _lhsOself =
             _self
         ( _layoutIpp,_layoutIself) =
             layout_
         ( _targetIpp,_targetIself) =
             target_
         ( _gvarsIpp,_gvarsIself) =
             gvars_
         ( _funsIpp,_funsIself) =
             funs_
         ( _aliasesIpp,_aliasesIself) =
             aliases_
     in  ( _lhsOpp,_lhsOself))
-- ModuleAsm ---------------------------------------------------
data ModuleAsm = ModuleAsm (String)
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
type ModuleAsms = [ModuleAsm]
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
-- NamedTy -----------------------------------------------------
data NamedTy = NamedTy (Id) (Type)
             deriving ( Eq,Ord,Show)
-- cata
sem_NamedTy :: NamedTy ->
               T_NamedTy
sem_NamedTy (NamedTy _name _ty) =
    (sem_NamedTy_NamedTy (sem_Id _name) (sem_Type _ty))
-- semantic domain
type T_NamedTy = ( NamedTy)
data Inh_NamedTy = Inh_NamedTy {}
data Syn_NamedTy = Syn_NamedTy {self_Syn_NamedTy :: NamedTy}
wrap_NamedTy :: T_NamedTy ->
                Inh_NamedTy ->
                Syn_NamedTy
wrap_NamedTy sem (Inh_NamedTy) =
    (let ( _lhsOself) = sem
     in  (Syn_NamedTy _lhsOself))
sem_NamedTy_NamedTy :: T_Id ->
                       T_Type ->
                       T_NamedTy
sem_NamedTy_NamedTy name_ ty_ =
    (let _lhsOself :: NamedTy
         _nameIself :: Id
         _tyIpp :: Doc
         _tyIself :: Type
         _self =
             NamedTy _nameIself _tyIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOself))
-- NamedTys ----------------------------------------------------
type NamedTys = [NamedTy]
-- cata
sem_NamedTys :: NamedTys ->
                T_NamedTys
sem_NamedTys list =
    (Prelude.foldr sem_NamedTys_Cons sem_NamedTys_Nil (Prelude.map sem_NamedTy list))
-- semantic domain
type T_NamedTys = ( NamedTys)
data Inh_NamedTys = Inh_NamedTys {}
data Syn_NamedTys = Syn_NamedTys {self_Syn_NamedTys :: NamedTys}
wrap_NamedTys :: T_NamedTys ->
                 Inh_NamedTys ->
                 Syn_NamedTys
wrap_NamedTys sem (Inh_NamedTys) =
    (let ( _lhsOself) = sem
     in  (Syn_NamedTys _lhsOself))
sem_NamedTys_Cons :: T_NamedTy ->
                     T_NamedTys ->
                     T_NamedTys
sem_NamedTys_Cons hd_ tl_ =
    (let _lhsOself :: NamedTys
         _hdIself :: NamedTy
         _tlIself :: NamedTys
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_NamedTys_Nil :: T_NamedTys
sem_NamedTys_Nil =
    (let _lhsOself :: NamedTys
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- PTyInt ------------------------------------------------------
type PTyInt = ( Type,(Int))
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
type PTyIntL = [PTyInt]
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
-- Parameter ---------------------------------------------------
data Parameter = Parameter (Id) (Type)
               deriving ( Eq,Ord,Show)
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
             ({-# LINE 53 "src/Language/LLVMIR/Printer.ag" #-}
              _tyIpp <> ppPName _varIself
              {-# LINE 3151 "src/Language/LLVMIR.hs" #-}
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
type Parameters = [Parameter]
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
             ({-# LINE 20 "src/Language/LLVMIR/Printer.ag" #-}
              if (length _tlIself == 0)
              then _hdIpp
              else _hdIpp <> char ',' <+> _tlIpp
              {-# LINE 3194 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 19 "src/Language/LLVMIR/Printer.ag" #-}
              P.empty
              {-# LINE 3212 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Section -----------------------------------------------------
data Section = Section (String)
             deriving ( Eq,Ord,Show)
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
data Target = Linux
            | MacOs
            deriving ( Eq,Ord,Show)
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
             ({-# LINE 40 "src/Language/LLVMIR/Printer.ag" #-}
              text "Linux"
              {-# LINE 3274 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 39 "src/Language/LLVMIR/Printer.ag" #-}
              text "MacOs"
              {-# LINE 3288 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             MacOs
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- TargetData --------------------------------------------------
data TargetData = TargetData (String) (Target)
                deriving ( Eq,Ord,Show)
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
             ({-# LINE 36 "src/Language/LLVMIR/Printer.ag" #-}
              text "target triple =" <+> dquotes (text s_)
              {-# LINE 3324 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             TargetData s_ _tIself
         _lhsOself =
             _self
         ( _tIpp,_tIself) =
             t_
     in  ( _lhsOpp,_lhsOself))
-- Terminator --------------------------------------------------
data Terminator = Br (Bool) (Identifier) (Identifier)
                | Ret (MValue)
                | Switch (IntTyValIdL)
                | UBr (Identifier)
                deriving ( Eq,Ord,Show)
-- cata
sem_Terminator :: Terminator ->
                  T_Terminator
sem_Terminator (Br _v _t _f) =
    (sem_Terminator_Br _v (sem_Identifier _t) (sem_Identifier _f))
sem_Terminator (Ret _v) =
    (sem_Terminator_Ret (sem_MValue _v))
sem_Terminator (Switch _elems) =
    (sem_Terminator_Switch (sem_IntTyValIdL _elems))
sem_Terminator (UBr _d) =
    (sem_Terminator_UBr (sem_Identifier _d))
-- semantic domain
type T_Terminator = ( Terminator)
data Inh_Terminator = Inh_Terminator {}
data Syn_Terminator = Syn_Terminator {self_Syn_Terminator :: Terminator}
wrap_Terminator :: T_Terminator ->
                   Inh_Terminator ->
                   Syn_Terminator
wrap_Terminator sem (Inh_Terminator) =
    (let ( _lhsOself) = sem
     in  (Syn_Terminator _lhsOself))
sem_Terminator_Br :: Bool ->
                     T_Identifier ->
                     T_Identifier ->
                     T_Terminator
sem_Terminator_Br v_ t_ f_ =
    (let _lhsOself :: Terminator
         _tIself :: Identifier
         _fIself :: Identifier
         _self =
             Br v_ _tIself _fIself
         _lhsOself =
             _self
         ( _tIself) =
             t_
         ( _fIself) =
             f_
     in  ( _lhsOself))
sem_Terminator_Ret :: T_MValue ->
                      T_Terminator
sem_Terminator_Ret v_ =
    (let _lhsOself :: Terminator
         _vIpp :: Doc
         _vIself :: MValue
         _self =
             Ret _vIself
         _lhsOself =
             _self
         ( _vIpp,_vIself) =
             v_
     in  ( _lhsOself))
sem_Terminator_Switch :: T_IntTyValIdL ->
                         T_Terminator
sem_Terminator_Switch elems_ =
    (let _lhsOself :: Terminator
         _elemsIself :: IntTyValIdL
         _self =
             Switch _elemsIself
         _lhsOself =
             _self
         ( _elemsIself) =
             elems_
     in  ( _lhsOself))
sem_Terminator_UBr :: T_Identifier ->
                      T_Terminator
sem_Terminator_UBr d_ =
    (let _lhsOself :: Terminator
         _dIself :: Identifier
         _self =
             UBr _dIself
         _lhsOself =
             _self
         ( _dIself) =
             d_
     in  ( _lhsOself))
-- Triplet -----------------------------------------------------
type Triplet = ( (Int),(Int),(Int))
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
data TyFloatPoint = TyDouble
                  | TyFP128
                  | TyFloat
                  | TyHalf
                  | TyPPCFP128
                  | Tyx86FP80
                  deriving ( Eq,Ord,Show)
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
             ({-# LINE 109 "src/Language/LLVMIR/Printer.ag" #-}
              text "double"
              {-# LINE 3482 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 110 "src/Language/LLVMIR/Printer.ag" #-}
              text "fp128"
              {-# LINE 3496 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 108 "src/Language/LLVMIR/Printer.ag" #-}
              text "float"
              {-# LINE 3510 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 107 "src/Language/LLVMIR/Printer.ag" #-}
              text "half"
              {-# LINE 3524 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 112 "src/Language/LLVMIR/Printer.ag" #-}
              text "ppc_fp128"
              {-# LINE 3538 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 111 "src/Language/LLVMIR/Printer.ag" #-}
              text "x86_fp80"
              {-# LINE 3552 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             Tyx86FP80
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Type --------------------------------------------------------
data Type = TyArray (Int) (Type)
          | TyFloatPoint (TyFloatPoint)
          | TyFunction (Types) (Type)
          | TyInt (Int)
          | TyLabel
          | TyMetadata
          | TyOpaque
          | TyPointer (Type)
          | TyStruct (String)
          | TyUnsupported
          | TyVector (Int) (Type)
          | TyVoid
          | Tyx86MMX
          deriving ( Eq,Ord,Show)
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
sem_Type (TyStruct _name) =
    (sem_Type_TyStruct _name)
sem_Type (TyUnsupported) =
    (sem_Type_TyUnsupported)
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
             ({-# LINE 99 "src/Language/LLVMIR/Printer.ag" #-}
              brackets $ int numEl_ <+> char 'x' <+> _tyIpp
              {-# LINE 3624 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 98 "src/Language/LLVMIR/Printer.ag" #-}
              _pIpp
              {-# LINE 3643 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 100 "src/Language/LLVMIR/Printer.ag" #-}
              text "pp TyFunction TODO"
              {-# LINE 3665 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 97 "src/Language/LLVMIR/Printer.ag" #-}
              char 'i' <> int p_
              {-# LINE 3684 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 94 "src/Language/LLVMIR/Printer.ag" #-}
              text "label"
              {-# LINE 3698 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 95 "src/Language/LLVMIR/Printer.ag" #-}
              text "metadata"
              {-# LINE 3712 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 96 "src/Language/LLVMIR/Printer.ag" #-}
              text "opaque"
              {-# LINE 3726 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 102 "src/Language/LLVMIR/Printer.ag" #-}
              _tyIpp <> char '*'
              {-# LINE 3743 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             TyPointer _tyIself
         _lhsOself =
             _self
         ( _tyIpp,_tyIself) =
             ty_
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyStruct :: String ->
                     T_Type
sem_Type_TyStruct name_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _lhsOpp =
             ({-# LINE 101 "src/Language/LLVMIR/Printer.ag" #-}
              char '%' <> dquotes (text name_)
              {-# LINE 3760 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             TyStruct name_
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Type_TyUnsupported :: T_Type
sem_Type_TyUnsupported =
    (let _lhsOpp :: Doc
         _lhsOself :: Type
         _lhsOpp =
             ({-# LINE 104 "src/Language/LLVMIR/Printer.ag" #-}
              text "TyUnsupported"
              {-# LINE 3774 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             TyUnsupported
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
             ({-# LINE 103 "src/Language/LLVMIR/Printer.ag" #-}
              char '<' <> int numEl_ <+> char 'x' <+> _tyIpp <> char '>'
              {-# LINE 3792 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 92 "src/Language/LLVMIR/Printer.ag" #-}
              text "void"
              {-# LINE 3808 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 93 "src/Language/LLVMIR/Printer.ag" #-}
              text "x86mmx"
              {-# LINE 3822 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             Tyx86MMX
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Types -------------------------------------------------------
type Types = [Type]
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
             ({-# LINE 20 "src/Language/LLVMIR/Printer.ag" #-}
              if (length _tlIself == 0)
              then _hdIpp
              else _hdIpp <> char ',' <+> _tlIpp
              {-# LINE 3861 "src/Language/LLVMIR.hs" #-}
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
             ({-# LINE 19 "src/Language/LLVMIR/Printer.ag" #-}
              P.empty
              {-# LINE 3879 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Value -------------------------------------------------------
data Value = Const (Constant)
           | Id (Identifier)
           deriving ( Eq,Ord,Show)
-- cata
sem_Value :: Value ->
             T_Value
sem_Value (Const _c) =
    (sem_Value_Const (sem_Constant _c))
sem_Value (Id _v) =
    (sem_Value_Id (sem_Identifier _v))
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
sem_Value_Const :: T_Constant ->
                   T_Value
sem_Value_Const c_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Value
         _cIpp :: Doc
         _cIself :: Constant
         _lhsOpp =
             ({-# LINE 86 "src/Language/LLVMIR/Printer.ag" #-}
              _cIpp
              {-# LINE 3917 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             Const _cIself
         _lhsOself =
             _self
         ( _cIpp,_cIself) =
             c_
     in  ( _lhsOpp,_lhsOself))
sem_Value_Id :: T_Identifier ->
                T_Value
sem_Value_Id v_ =
    (let _lhsOpp :: Doc
         _lhsOself :: Value
         _vIself :: Identifier
         _lhsOpp =
             ({-# LINE 85 "src/Language/LLVMIR/Printer.ag" #-}
              P.empty
              {-# LINE 3935 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             Id _vIself
         _lhsOself =
             _self
         ( _vIself) =
             v_
     in  ( _lhsOpp,_lhsOself))
-- Visibility --------------------------------------------------
data Visibility = Default
                | Hidden
                | Protected
                deriving ( Eq,Ord,Show)
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