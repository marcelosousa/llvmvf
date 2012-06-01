

-- UUAGC 0.9.38.6 (src/Language/LLVMIR.ag)
module Language.LLVMIR where

{-# LINE 11 "src/Language/LLVMIR/Base.ag" #-}

import Prelude              hiding (sequence)
import Data.Char            (chr)
import qualified Data.Map as Data.Map
import qualified Data.Map as Map
import Data.Map
{-# LINE 14 "src/Language/LLVMIR.hs" #-}

{-# LINE 11 "src/Language/LLVMIR/PPrinter.ag" #-}

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

{-# LINE 1 "src/Language/LLVMIR/PPrinter.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.PPrinter
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 44 "src/Language/LLVMIR.hs" #-}

{-# LINE 32 "src/Language/LLVMIR/PPrinter.ag" #-}

instance Pretty Module where
    pretty mdl = pp_Syn_Module $ wrap_Module (sem_Module mdl) $ Inh_Module {}
    
instance Pretty DataLayout where
    pretty d = pp_Syn_DataLayout $ wrap_DataLayout (sem_DataLayout d) $ Inh_DataLayout {}

{-# LINE 54 "src/Language/LLVMIR.hs" #-}
-- Alias -------------------------------------------------------
data Alias  = Alias (Id ) (MLinkageTy ) (MVisibility ) (Type ) (Id ) 
            deriving ( Eq,Ord,Show)
-- cata
sem_Alias :: Alias  ->
             T_Alias 
sem_Alias (Alias _name _linkage _visibility _aliaseeTy _aliasee )  =
    (sem_Alias_Alias (sem_Id _name ) (sem_MLinkageTy _linkage ) (sem_MVisibility _visibility ) (sem_Type _aliaseeTy ) (sem_Id _aliasee ) )
-- semantic domain
type T_Alias  = ( Alias )
data Inh_Alias  = Inh_Alias {}
data Syn_Alias  = Syn_Alias {self_Syn_Alias :: Alias }
wrap_Alias :: T_Alias  ->
              Inh_Alias  ->
              Syn_Alias 
wrap_Alias sem (Inh_Alias )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Alias _lhsOself ))
sem_Alias_Alias :: T_Id  ->
                   T_MLinkageTy  ->
                   T_MVisibility  ->
                   T_Type  ->
                   T_Id  ->
                   T_Alias 
sem_Alias_Alias name_ linkage_ visibility_ aliaseeTy_ aliasee_  =
    (let _lhsOself :: Alias 
         _nameIself :: Id 
         _linkageIself :: MLinkageTy 
         _visibilityIself :: MVisibility 
         _aliaseeTyIself :: Type 
         _aliaseeIself :: Id 
         _self =
             Alias _nameIself _linkageIself _visibilityIself _aliaseeTyIself _aliaseeIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_ 
         ( _linkageIself) =
             linkage_ 
         ( _visibilityIself) =
             visibility_ 
         ( _aliaseeTyIself) =
             aliaseeTy_ 
         ( _aliaseeIself) =
             aliasee_ 
     in  ( _lhsOself))
-- Aliases -----------------------------------------------------
type Aliases  = [Alias ]
-- cata
sem_Aliases :: Aliases  ->
               T_Aliases 
sem_Aliases list  =
    (Prelude.foldr sem_Aliases_Cons sem_Aliases_Nil (Prelude.map sem_Alias list) )
-- semantic domain
type T_Aliases  = ( Aliases )
data Inh_Aliases  = Inh_Aliases {}
data Syn_Aliases  = Syn_Aliases {self_Syn_Aliases :: Aliases }
wrap_Aliases :: T_Aliases  ->
                Inh_Aliases  ->
                Syn_Aliases 
wrap_Aliases sem (Inh_Aliases )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Aliases _lhsOself ))
sem_Aliases_Cons :: T_Alias  ->
                    T_Aliases  ->
                    T_Aliases 
sem_Aliases_Cons hd_ tl_  =
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
sem_Aliases_Nil  =
    (let _lhsOself :: Aliases 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Align -------------------------------------------------------
data Align  = Align (Int) 
            deriving ( Eq,Ord,Show)
-- cata
sem_Align :: Align  ->
             T_Align 
sem_Align (Align _n )  =
    (sem_Align_Align _n )
-- semantic domain
type T_Align  = ( Align )
data Inh_Align  = Inh_Align {}
data Syn_Align  = Syn_Align {self_Syn_Align :: Align }
wrap_Align :: T_Align  ->
              Inh_Align  ->
              Syn_Align 
wrap_Align sem (Inh_Align )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Align _lhsOself ))
sem_Align_Align :: Int ->
                   T_Align 
sem_Align_Align n_  =
    (let _lhsOself :: Align 
         _self =
             Align n_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Argument ----------------------------------------------------
data Argument  = Argument 
               deriving ( Eq,Ord,Show)
-- cata
sem_Argument :: Argument  ->
                T_Argument 
sem_Argument (Argument )  =
    (sem_Argument_Argument )
-- semantic domain
type T_Argument  = ( Argument )
data Inh_Argument  = Inh_Argument {}
data Syn_Argument  = Syn_Argument {self_Syn_Argument :: Argument }
wrap_Argument :: T_Argument  ->
                 Inh_Argument  ->
                 Syn_Argument 
wrap_Argument sem (Inh_Argument )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Argument _lhsOself ))
sem_Argument_Argument :: T_Argument 
sem_Argument_Argument  =
    (let _lhsOself :: Argument 
         _self =
             Argument
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Arguments ---------------------------------------------------
type Arguments  = [Argument ]
-- cata
sem_Arguments :: Arguments  ->
                 T_Arguments 
sem_Arguments list  =
    (Prelude.foldr sem_Arguments_Cons sem_Arguments_Nil (Prelude.map sem_Argument list) )
-- semantic domain
type T_Arguments  = ( Arguments )
data Inh_Arguments  = Inh_Arguments {}
data Syn_Arguments  = Syn_Arguments {self_Syn_Arguments :: Arguments }
wrap_Arguments :: T_Arguments  ->
                  Inh_Arguments  ->
                  Syn_Arguments 
wrap_Arguments sem (Inh_Arguments )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Arguments _lhsOself ))
sem_Arguments_Cons :: T_Argument  ->
                      T_Arguments  ->
                      T_Arguments 
sem_Arguments_Cons hd_ tl_  =
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
sem_Arguments_Nil  =
    (let _lhsOself :: Arguments 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- BasicBlock --------------------------------------------------
data BasicBlock  = BasicBlock (Label ) (Instructions ) 
                 deriving ( Eq,Ord,Show)
-- cata
sem_BasicBlock :: BasicBlock  ->
                  T_BasicBlock 
sem_BasicBlock (BasicBlock _label _instrs )  =
    (sem_BasicBlock_BasicBlock (sem_Label _label ) (sem_Instructions _instrs ) )
-- semantic domain
type T_BasicBlock  = ( BasicBlock )
data Inh_BasicBlock  = Inh_BasicBlock {}
data Syn_BasicBlock  = Syn_BasicBlock {self_Syn_BasicBlock :: BasicBlock }
wrap_BasicBlock :: T_BasicBlock  ->
                   Inh_BasicBlock  ->
                   Syn_BasicBlock 
wrap_BasicBlock sem (Inh_BasicBlock )  =
    (let ( _lhsOself) = sem 
     in  (Syn_BasicBlock _lhsOself ))
sem_BasicBlock_BasicBlock :: T_Label  ->
                             T_Instructions  ->
                             T_BasicBlock 
sem_BasicBlock_BasicBlock label_ instrs_  =
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
type BasicBlocks  = [BasicBlock ]
-- cata
sem_BasicBlocks :: BasicBlocks  ->
                   T_BasicBlocks 
sem_BasicBlocks list  =
    (Prelude.foldr sem_BasicBlocks_Cons sem_BasicBlocks_Nil (Prelude.map sem_BasicBlock list) )
-- semantic domain
type T_BasicBlocks  = ( BasicBlocks )
data Inh_BasicBlocks  = Inh_BasicBlocks {}
data Syn_BasicBlocks  = Syn_BasicBlocks {self_Syn_BasicBlocks :: BasicBlocks }
wrap_BasicBlocks :: T_BasicBlocks  ->
                    Inh_BasicBlocks  ->
                    Syn_BasicBlocks 
wrap_BasicBlocks sem (Inh_BasicBlocks )  =
    (let ( _lhsOself) = sem 
     in  (Syn_BasicBlocks _lhsOself ))
sem_BasicBlocks_Cons :: T_BasicBlock  ->
                        T_BasicBlocks  ->
                        T_BasicBlocks 
sem_BasicBlocks_Cons hd_ tl_  =
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
sem_BasicBlocks_Nil  =
    (let _lhsOself :: BasicBlocks 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- CConv -------------------------------------------------------
data CConv  = Cc (Int) 
            | Cc10 
            | Ccc 
            | Coldcc 
            | Fastcc 
            deriving ( Eq,Ord,Show)
-- cata
sem_CConv :: CConv  ->
             T_CConv 
sem_CConv (Cc _n )  =
    (sem_CConv_Cc _n )
sem_CConv (Cc10 )  =
    (sem_CConv_Cc10 )
sem_CConv (Ccc )  =
    (sem_CConv_Ccc )
sem_CConv (Coldcc )  =
    (sem_CConv_Coldcc )
sem_CConv (Fastcc )  =
    (sem_CConv_Fastcc )
-- semantic domain
type T_CConv  = ( CConv )
data Inh_CConv  = Inh_CConv {}
data Syn_CConv  = Syn_CConv {self_Syn_CConv :: CConv }
wrap_CConv :: T_CConv  ->
              Inh_CConv  ->
              Syn_CConv 
wrap_CConv sem (Inh_CConv )  =
    (let ( _lhsOself) = sem 
     in  (Syn_CConv _lhsOself ))
sem_CConv_Cc :: Int ->
                T_CConv 
sem_CConv_Cc n_  =
    (let _lhsOself :: CConv 
         _self =
             Cc n_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_CConv_Cc10 :: T_CConv 
sem_CConv_Cc10  =
    (let _lhsOself :: CConv 
         _self =
             Cc10
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_CConv_Ccc :: T_CConv 
sem_CConv_Ccc  =
    (let _lhsOself :: CConv 
         _self =
             Ccc
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_CConv_Coldcc :: T_CConv 
sem_CConv_Coldcc  =
    (let _lhsOself :: CConv 
         _self =
             Coldcc
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_CConv_Fastcc :: T_CConv 
sem_CConv_Fastcc  =
    (let _lhsOself :: CConv 
         _self =
             Fastcc
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Constant ----------------------------------------------------
data Constant  = ArrayC (PTyIntL ) 
               | BlockAddr (Identifier ) (Identifier ) 
               | BoolC (Bool) 
               | FloatC (Float) (Type ) 
               | IntC (Int) (Type ) 
               | NullC (Type ) 
               | StructC (PTyIntL ) 
               | UndefC 
               | VectorC (PTyIntL ) 
               | ZeroInitC (Type ) 
               deriving ( Eq,Ord,Show)
-- cata
sem_Constant :: Constant  ->
                T_Constant 
sem_Constant (ArrayC _elems )  =
    (sem_Constant_ArrayC (sem_PTyIntL _elems ) )
sem_Constant (BlockAddr _fun _label )  =
    (sem_Constant_BlockAddr (sem_Identifier _fun ) (sem_Identifier _label ) )
sem_Constant (BoolC _v )  =
    (sem_Constant_BoolC _v )
sem_Constant (FloatC _v _ty )  =
    (sem_Constant_FloatC _v (sem_Type _ty ) )
sem_Constant (IntC _v _ty )  =
    (sem_Constant_IntC _v (sem_Type _ty ) )
sem_Constant (NullC _ty )  =
    (sem_Constant_NullC (sem_Type _ty ) )
sem_Constant (StructC _elems )  =
    (sem_Constant_StructC (sem_PTyIntL _elems ) )
sem_Constant (UndefC )  =
    (sem_Constant_UndefC )
sem_Constant (VectorC _elems )  =
    (sem_Constant_VectorC (sem_PTyIntL _elems ) )
sem_Constant (ZeroInitC _ty )  =
    (sem_Constant_ZeroInitC (sem_Type _ty ) )
-- semantic domain
type T_Constant  = ( Constant )
data Inh_Constant  = Inh_Constant {}
data Syn_Constant  = Syn_Constant {self_Syn_Constant :: Constant }
wrap_Constant :: T_Constant  ->
                 Inh_Constant  ->
                 Syn_Constant 
wrap_Constant sem (Inh_Constant )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Constant _lhsOself ))
sem_Constant_ArrayC :: T_PTyIntL  ->
                       T_Constant 
sem_Constant_ArrayC elems_  =
    (let _lhsOself :: Constant 
         _elemsIself :: PTyIntL 
         _self =
             ArrayC _elemsIself
         _lhsOself =
             _self
         ( _elemsIself) =
             elems_ 
     in  ( _lhsOself))
sem_Constant_BlockAddr :: T_Identifier  ->
                          T_Identifier  ->
                          T_Constant 
sem_Constant_BlockAddr fun_ label_  =
    (let _lhsOself :: Constant 
         _funIself :: Identifier 
         _labelIself :: Identifier 
         _self =
             BlockAddr _funIself _labelIself
         _lhsOself =
             _self
         ( _funIself) =
             fun_ 
         ( _labelIself) =
             label_ 
     in  ( _lhsOself))
sem_Constant_BoolC :: Bool ->
                      T_Constant 
sem_Constant_BoolC v_  =
    (let _lhsOself :: Constant 
         _self =
             BoolC v_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Constant_FloatC :: Float ->
                       T_Type  ->
                       T_Constant 
sem_Constant_FloatC v_ ty_  =
    (let _lhsOself :: Constant 
         _tyIself :: Type 
         _self =
             FloatC v_ _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_ 
     in  ( _lhsOself))
sem_Constant_IntC :: Int ->
                     T_Type  ->
                     T_Constant 
sem_Constant_IntC v_ ty_  =
    (let _lhsOself :: Constant 
         _tyIself :: Type 
         _self =
             IntC v_ _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_ 
     in  ( _lhsOself))
sem_Constant_NullC :: T_Type  ->
                      T_Constant 
sem_Constant_NullC ty_  =
    (let _lhsOself :: Constant 
         _tyIself :: Type 
         _self =
             NullC _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_ 
     in  ( _lhsOself))
sem_Constant_StructC :: T_PTyIntL  ->
                        T_Constant 
sem_Constant_StructC elems_  =
    (let _lhsOself :: Constant 
         _elemsIself :: PTyIntL 
         _self =
             StructC _elemsIself
         _lhsOself =
             _self
         ( _elemsIself) =
             elems_ 
     in  ( _lhsOself))
sem_Constant_UndefC :: T_Constant 
sem_Constant_UndefC  =
    (let _lhsOself :: Constant 
         _self =
             UndefC
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Constant_VectorC :: T_PTyIntL  ->
                        T_Constant 
sem_Constant_VectorC elems_  =
    (let _lhsOself :: Constant 
         _elemsIself :: PTyIntL 
         _self =
             VectorC _elemsIself
         _lhsOself =
             _self
         ( _elemsIself) =
             elems_ 
     in  ( _lhsOself))
sem_Constant_ZeroInitC :: T_Type  ->
                          T_Constant 
sem_Constant_ZeroInitC ty_  =
    (let _lhsOself :: Constant 
         _tyIself :: Type 
         _self =
             ZeroInitC _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_ 
     in  ( _lhsOself))
-- DLayout -----------------------------------------------------
type DLayout  = [(String)]
-- cata
sem_DLayout :: DLayout  ->
               T_DLayout 
sem_DLayout list  =
    (Prelude.foldr sem_DLayout_Cons sem_DLayout_Nil list )
-- semantic domain
type T_DLayout  = ( DLayout )
data Inh_DLayout  = Inh_DLayout {}
data Syn_DLayout  = Syn_DLayout {self_Syn_DLayout :: DLayout }
wrap_DLayout :: T_DLayout  ->
                Inh_DLayout  ->
                Syn_DLayout 
wrap_DLayout sem (Inh_DLayout )  =
    (let ( _lhsOself) = sem 
     in  (Syn_DLayout _lhsOself ))
sem_DLayout_Cons :: String ->
                    T_DLayout  ->
                    T_DLayout 
sem_DLayout_Cons hd_ tl_  =
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
sem_DLayout_Nil  =
    (let _lhsOself :: DLayout 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- DataLayout --------------------------------------------------
data DataLayout  = DataLayout (DLayout ) 
                 deriving ( Eq,Ord,Show)
-- cata
sem_DataLayout :: DataLayout  ->
                  T_DataLayout 
sem_DataLayout (DataLayout _s )  =
    (sem_DataLayout_DataLayout (sem_DLayout _s ) )
-- semantic domain
type T_DataLayout  = ( Doc,DataLayout )
data Inh_DataLayout  = Inh_DataLayout {}
data Syn_DataLayout  = Syn_DataLayout {pp_Syn_DataLayout :: Doc,self_Syn_DataLayout :: DataLayout }
wrap_DataLayout :: T_DataLayout  ->
                   Inh_DataLayout  ->
                   Syn_DataLayout 
wrap_DataLayout sem (Inh_DataLayout )  =
    (let ( _lhsOpp,_lhsOself) = sem 
     in  (Syn_DataLayout _lhsOpp _lhsOself ))
sem_DataLayout_DataLayout :: T_DLayout  ->
                             T_DataLayout 
sem_DataLayout_DataLayout s_  =
    (let _lhsOpp :: Doc
         _lhsOself :: DataLayout 
         _sIself :: DLayout 
         _lhsOpp =
             ({-# LINE 23 "src/Language/LLVMIR/PPrinter.ag" #-}
              text "target datalayout =" <+> Prelude.foldr1 (\x y -> x <> char '-' <> y) (Prelude.map text _sIself)
              {-# LINE 611 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             DataLayout _sIself
         _lhsOself =
             _self
         ( _sIself) =
             s_ 
     in  ( _lhsOpp,_lhsOself))
-- DefinitionTy ------------------------------------------------
data DefinitionTy  = Constant 
                   | ThreadLocal 
                   deriving ( Eq,Ord,Show)
-- cata
sem_DefinitionTy :: DefinitionTy  ->
                    T_DefinitionTy 
sem_DefinitionTy (Constant )  =
    (sem_DefinitionTy_Constant )
sem_DefinitionTy (ThreadLocal )  =
    (sem_DefinitionTy_ThreadLocal )
-- semantic domain
type T_DefinitionTy  = ( DefinitionTy )
data Inh_DefinitionTy  = Inh_DefinitionTy {}
data Syn_DefinitionTy  = Syn_DefinitionTy {self_Syn_DefinitionTy :: DefinitionTy }
wrap_DefinitionTy :: T_DefinitionTy  ->
                     Inh_DefinitionTy  ->
                     Syn_DefinitionTy 
wrap_DefinitionTy sem (Inh_DefinitionTy )  =
    (let ( _lhsOself) = sem 
     in  (Syn_DefinitionTy _lhsOself ))
sem_DefinitionTy_Constant :: T_DefinitionTy 
sem_DefinitionTy_Constant  =
    (let _lhsOself :: DefinitionTy 
         _self =
             Constant
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_DefinitionTy_ThreadLocal :: T_DefinitionTy 
sem_DefinitionTy_ThreadLocal  =
    (let _lhsOself :: DefinitionTy 
         _self =
             ThreadLocal
         _lhsOself =
             _self
     in  ( _lhsOself))
-- FpTy --------------------------------------------------------
data FpTy  = DoubleTy 
           | FloatTy 
           | Fp128Ty 
           | HalfTy 
           | Ppcfp128ty 
           | X86fp80Ty 
           deriving ( Eq,Ord,Show)
-- cata
sem_FpTy :: FpTy  ->
            T_FpTy 
sem_FpTy (DoubleTy )  =
    (sem_FpTy_DoubleTy )
sem_FpTy (FloatTy )  =
    (sem_FpTy_FloatTy )
sem_FpTy (Fp128Ty )  =
    (sem_FpTy_Fp128Ty )
sem_FpTy (HalfTy )  =
    (sem_FpTy_HalfTy )
sem_FpTy (Ppcfp128ty )  =
    (sem_FpTy_Ppcfp128ty )
sem_FpTy (X86fp80Ty )  =
    (sem_FpTy_X86fp80Ty )
-- semantic domain
type T_FpTy  = ( FpTy )
data Inh_FpTy  = Inh_FpTy {}
data Syn_FpTy  = Syn_FpTy {self_Syn_FpTy :: FpTy }
wrap_FpTy :: T_FpTy  ->
             Inh_FpTy  ->
             Syn_FpTy 
wrap_FpTy sem (Inh_FpTy )  =
    (let ( _lhsOself) = sem 
     in  (Syn_FpTy _lhsOself ))
sem_FpTy_DoubleTy :: T_FpTy 
sem_FpTy_DoubleTy  =
    (let _lhsOself :: FpTy 
         _self =
             DoubleTy
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FpTy_FloatTy :: T_FpTy 
sem_FpTy_FloatTy  =
    (let _lhsOself :: FpTy 
         _self =
             FloatTy
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FpTy_Fp128Ty :: T_FpTy 
sem_FpTy_Fp128Ty  =
    (let _lhsOself :: FpTy 
         _self =
             Fp128Ty
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FpTy_HalfTy :: T_FpTy 
sem_FpTy_HalfTy  =
    (let _lhsOself :: FpTy 
         _self =
             HalfTy
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FpTy_Ppcfp128ty :: T_FpTy 
sem_FpTy_Ppcfp128ty  =
    (let _lhsOself :: FpTy 
         _self =
             Ppcfp128ty
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FpTy_X86fp80Ty :: T_FpTy 
sem_FpTy_X86fp80Ty  =
    (let _lhsOself :: FpTy 
         _self =
             X86fp80Ty
         _lhsOself =
             _self
     in  ( _lhsOself))
-- FunAttr -----------------------------------------------------
data FunAttr  = AddressSafety 
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
sem_FunAttr :: FunAttr  ->
               T_FunAttr 
sem_FunAttr (AddressSafety )  =
    (sem_FunAttr_AddressSafety )
sem_FunAttr (Alignstack _n )  =
    (sem_FunAttr_Alignstack _n )
sem_FunAttr (Alwaysinline )  =
    (sem_FunAttr_Alwaysinline )
sem_FunAttr (Inlinehint )  =
    (sem_FunAttr_Inlinehint )
sem_FunAttr (Naked )  =
    (sem_FunAttr_Naked )
sem_FunAttr (Noimplicitfloat )  =
    (sem_FunAttr_Noimplicitfloat )
sem_FunAttr (Noinline )  =
    (sem_FunAttr_Noinline )
sem_FunAttr (Nonlazybind )  =
    (sem_FunAttr_Nonlazybind )
sem_FunAttr (Noredzone )  =
    (sem_FunAttr_Noredzone )
sem_FunAttr (Noreturn )  =
    (sem_FunAttr_Noreturn )
sem_FunAttr (Nounwind )  =
    (sem_FunAttr_Nounwind )
sem_FunAttr (Optsize )  =
    (sem_FunAttr_Optsize )
sem_FunAttr (Readnone )  =
    (sem_FunAttr_Readnone )
sem_FunAttr (Readonly )  =
    (sem_FunAttr_Readonly )
sem_FunAttr (ReturnsTwice )  =
    (sem_FunAttr_ReturnsTwice )
sem_FunAttr (Ssp )  =
    (sem_FunAttr_Ssp )
sem_FunAttr (Sspreq )  =
    (sem_FunAttr_Sspreq )
sem_FunAttr (Uwtable )  =
    (sem_FunAttr_Uwtable )
-- semantic domain
type T_FunAttr  = ( FunAttr )
data Inh_FunAttr  = Inh_FunAttr {}
data Syn_FunAttr  = Syn_FunAttr {self_Syn_FunAttr :: FunAttr }
wrap_FunAttr :: T_FunAttr  ->
                Inh_FunAttr  ->
                Syn_FunAttr 
wrap_FunAttr sem (Inh_FunAttr )  =
    (let ( _lhsOself) = sem 
     in  (Syn_FunAttr _lhsOself ))
sem_FunAttr_AddressSafety :: T_FunAttr 
sem_FunAttr_AddressSafety  =
    (let _lhsOself :: FunAttr 
         _self =
             AddressSafety
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Alignstack :: Int ->
                          T_FunAttr 
sem_FunAttr_Alignstack n_  =
    (let _lhsOself :: FunAttr 
         _self =
             Alignstack n_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Alwaysinline :: T_FunAttr 
sem_FunAttr_Alwaysinline  =
    (let _lhsOself :: FunAttr 
         _self =
             Alwaysinline
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Inlinehint :: T_FunAttr 
sem_FunAttr_Inlinehint  =
    (let _lhsOself :: FunAttr 
         _self =
             Inlinehint
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Naked :: T_FunAttr 
sem_FunAttr_Naked  =
    (let _lhsOself :: FunAttr 
         _self =
             Naked
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Noimplicitfloat :: T_FunAttr 
sem_FunAttr_Noimplicitfloat  =
    (let _lhsOself :: FunAttr 
         _self =
             Noimplicitfloat
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Noinline :: T_FunAttr 
sem_FunAttr_Noinline  =
    (let _lhsOself :: FunAttr 
         _self =
             Noinline
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Nonlazybind :: T_FunAttr 
sem_FunAttr_Nonlazybind  =
    (let _lhsOself :: FunAttr 
         _self =
             Nonlazybind
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Noredzone :: T_FunAttr 
sem_FunAttr_Noredzone  =
    (let _lhsOself :: FunAttr 
         _self =
             Noredzone
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Noreturn :: T_FunAttr 
sem_FunAttr_Noreturn  =
    (let _lhsOself :: FunAttr 
         _self =
             Noreturn
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Nounwind :: T_FunAttr 
sem_FunAttr_Nounwind  =
    (let _lhsOself :: FunAttr 
         _self =
             Nounwind
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Optsize :: T_FunAttr 
sem_FunAttr_Optsize  =
    (let _lhsOself :: FunAttr 
         _self =
             Optsize
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Readnone :: T_FunAttr 
sem_FunAttr_Readnone  =
    (let _lhsOself :: FunAttr 
         _self =
             Readnone
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Readonly :: T_FunAttr 
sem_FunAttr_Readonly  =
    (let _lhsOself :: FunAttr 
         _self =
             Readonly
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_ReturnsTwice :: T_FunAttr 
sem_FunAttr_ReturnsTwice  =
    (let _lhsOself :: FunAttr 
         _self =
             ReturnsTwice
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Ssp :: T_FunAttr 
sem_FunAttr_Ssp  =
    (let _lhsOself :: FunAttr 
         _self =
             Ssp
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Sspreq :: T_FunAttr 
sem_FunAttr_Sspreq  =
    (let _lhsOself :: FunAttr 
         _self =
             Sspreq
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_FunAttr_Uwtable :: T_FunAttr 
sem_FunAttr_Uwtable  =
    (let _lhsOself :: FunAttr 
         _self =
             Uwtable
         _lhsOself =
             _self
     in  ( _lhsOself))
-- FuncAttrs ---------------------------------------------------
type FuncAttrs  = [FunAttr ]
-- cata
sem_FuncAttrs :: FuncAttrs  ->
                 T_FuncAttrs 
sem_FuncAttrs list  =
    (Prelude.foldr sem_FuncAttrs_Cons sem_FuncAttrs_Nil (Prelude.map sem_FunAttr list) )
-- semantic domain
type T_FuncAttrs  = ( FuncAttrs )
data Inh_FuncAttrs  = Inh_FuncAttrs {}
data Syn_FuncAttrs  = Syn_FuncAttrs {self_Syn_FuncAttrs :: FuncAttrs }
wrap_FuncAttrs :: T_FuncAttrs  ->
                  Inh_FuncAttrs  ->
                  Syn_FuncAttrs 
wrap_FuncAttrs sem (Inh_FuncAttrs )  =
    (let ( _lhsOself) = sem 
     in  (Syn_FuncAttrs _lhsOself ))
sem_FuncAttrs_Cons :: T_FunAttr  ->
                      T_FuncAttrs  ->
                      T_FuncAttrs 
sem_FuncAttrs_Cons hd_ tl_  =
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
sem_FuncAttrs_Nil  =
    (let _lhsOself :: FuncAttrs 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Function ----------------------------------------------------
data Function  = FunctionDecl (Id ) 
               | FunctionDef (Id ) (BasicBlocks ) 
               deriving ( Eq,Ord,Show)
-- cata
sem_Function :: Function  ->
                T_Function 
sem_Function (FunctionDecl _name )  =
    (sem_Function_FunctionDecl (sem_Id _name ) )
sem_Function (FunctionDef _name _bb )  =
    (sem_Function_FunctionDef (sem_Id _name ) (sem_BasicBlocks _bb ) )
-- semantic domain
type T_Function  = ( Function )
data Inh_Function  = Inh_Function {}
data Syn_Function  = Syn_Function {self_Syn_Function :: Function }
wrap_Function :: T_Function  ->
                 Inh_Function  ->
                 Syn_Function 
wrap_Function sem (Inh_Function )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Function _lhsOself ))
sem_Function_FunctionDecl :: T_Id  ->
                             T_Function 
sem_Function_FunctionDecl name_  =
    (let _lhsOself :: Function 
         _nameIself :: Id 
         _self =
             FunctionDecl _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_ 
     in  ( _lhsOself))
sem_Function_FunctionDef :: T_Id  ->
                            T_BasicBlocks  ->
                            T_Function 
sem_Function_FunctionDef name_ bb_  =
    (let _lhsOself :: Function 
         _nameIself :: Id 
         _bbIself :: BasicBlocks 
         _self =
             FunctionDef _nameIself _bbIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_ 
         ( _bbIself) =
             bb_ 
     in  ( _lhsOself))
-- Functions ---------------------------------------------------
type Functions  = [Function ]
-- cata
sem_Functions :: Functions  ->
                 T_Functions 
sem_Functions list  =
    (Prelude.foldr sem_Functions_Cons sem_Functions_Nil (Prelude.map sem_Function list) )
-- semantic domain
type T_Functions  = ( Doc,Functions )
data Inh_Functions  = Inh_Functions {}
data Syn_Functions  = Syn_Functions {pp_Syn_Functions :: Doc,self_Syn_Functions :: Functions }
wrap_Functions :: T_Functions  ->
                  Inh_Functions  ->
                  Syn_Functions 
wrap_Functions sem (Inh_Functions )  =
    (let ( _lhsOpp,_lhsOself) = sem 
     in  (Syn_Functions _lhsOpp _lhsOself ))
sem_Functions_Cons :: T_Function  ->
                      T_Functions  ->
                      T_Functions 
sem_Functions_Cons hd_ tl_  =
    (let _lhsOpp :: Doc
         _lhsOself :: Functions 
         _hdIself :: Function 
         _tlIpp :: Doc
         _tlIself :: Functions 
         _lhsOpp =
             ({-# LINE 16 "src/Language/LLVMIR/PPrinter.ag" #-}
              _tlIpp
              {-# LINE 1071 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_ 
         ( _tlIpp,_tlIself) =
             tl_ 
     in  ( _lhsOpp,_lhsOself))
sem_Functions_Nil :: T_Functions 
sem_Functions_Nil  =
    (let _lhsOpp :: Doc
         _lhsOself :: Functions 
         _lhsOpp =
             ({-# LINE 16 "src/Language/LLVMIR/PPrinter.ag" #-}
              P.empty
              {-# LINE 1089 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- GCName ------------------------------------------------------
data GCName  = GCName (String) 
             deriving ( Eq,Ord,Show)
-- cata
sem_GCName :: GCName  ->
              T_GCName 
sem_GCName (GCName _name )  =
    (sem_GCName_GCName _name )
-- semantic domain
type T_GCName  = ( GCName )
data Inh_GCName  = Inh_GCName {}
data Syn_GCName  = Syn_GCName {self_Syn_GCName :: GCName }
wrap_GCName :: T_GCName  ->
               Inh_GCName  ->
               Syn_GCName 
wrap_GCName sem (Inh_GCName )  =
    (let ( _lhsOself) = sem 
     in  (Syn_GCName _lhsOself ))
sem_GCName_GCName :: String ->
                     T_GCName 
sem_GCName_GCName name_  =
    (let _lhsOself :: GCName 
         _self =
             GCName name_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- GlobalVar ---------------------------------------------------
data GlobalVar  = GlobalVar (Id ) 
                deriving ( Eq,Ord,Show)
-- cata
sem_GlobalVar :: GlobalVar  ->
                 T_GlobalVar 
sem_GlobalVar (GlobalVar _name )  =
    (sem_GlobalVar_GlobalVar (sem_Id _name ) )
-- semantic domain
type T_GlobalVar  = ( GlobalVar )
data Inh_GlobalVar  = Inh_GlobalVar {}
data Syn_GlobalVar  = Syn_GlobalVar {self_Syn_GlobalVar :: GlobalVar }
wrap_GlobalVar :: T_GlobalVar  ->
                  Inh_GlobalVar  ->
                  Syn_GlobalVar 
wrap_GlobalVar sem (Inh_GlobalVar )  =
    (let ( _lhsOself) = sem 
     in  (Syn_GlobalVar _lhsOself ))
sem_GlobalVar_GlobalVar :: T_Id  ->
                           T_GlobalVar 
sem_GlobalVar_GlobalVar name_  =
    (let _lhsOself :: GlobalVar 
         _nameIself :: Id 
         _self =
             GlobalVar _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_ 
     in  ( _lhsOself))
-- GlobalVars --------------------------------------------------
type GlobalVars  = [GlobalVar ]
-- cata
sem_GlobalVars :: GlobalVars  ->
                  T_GlobalVars 
sem_GlobalVars list  =
    (Prelude.foldr sem_GlobalVars_Cons sem_GlobalVars_Nil (Prelude.map sem_GlobalVar list) )
-- semantic domain
type T_GlobalVars  = ( Doc,GlobalVars )
data Inh_GlobalVars  = Inh_GlobalVars {}
data Syn_GlobalVars  = Syn_GlobalVars {pp_Syn_GlobalVars :: Doc,self_Syn_GlobalVars :: GlobalVars }
wrap_GlobalVars :: T_GlobalVars  ->
                   Inh_GlobalVars  ->
                   Syn_GlobalVars 
wrap_GlobalVars sem (Inh_GlobalVars )  =
    (let ( _lhsOpp,_lhsOself) = sem 
     in  (Syn_GlobalVars _lhsOpp _lhsOself ))
sem_GlobalVars_Cons :: T_GlobalVar  ->
                       T_GlobalVars  ->
                       T_GlobalVars 
sem_GlobalVars_Cons hd_ tl_  =
    (let _lhsOpp :: Doc
         _lhsOself :: GlobalVars 
         _hdIself :: GlobalVar 
         _tlIpp :: Doc
         _tlIself :: GlobalVars 
         _lhsOpp =
             ({-# LINE 16 "src/Language/LLVMIR/PPrinter.ag" #-}
              _tlIpp
              {-# LINE 1182 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_ 
         ( _tlIpp,_tlIself) =
             tl_ 
     in  ( _lhsOpp,_lhsOself))
sem_GlobalVars_Nil :: T_GlobalVars 
sem_GlobalVars_Nil  =
    (let _lhsOpp :: Doc
         _lhsOself :: GlobalVars 
         _lhsOpp =
             ({-# LINE 16 "src/Language/LLVMIR/PPrinter.ag" #-}
              P.empty
              {-# LINE 1200 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- Id ----------------------------------------------------------
type Id  = ( (String))
-- cata
sem_Id :: Id  ->
          T_Id 
sem_Id ( x1)  =
    (sem_Id_Tuple x1 )
-- semantic domain
type T_Id  = ( Id )
data Inh_Id  = Inh_Id {}
data Syn_Id  = Syn_Id {self_Syn_Id :: Id }
wrap_Id :: T_Id  ->
           Inh_Id  ->
           Syn_Id 
wrap_Id sem (Inh_Id )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Id _lhsOself ))
sem_Id_Tuple :: String ->
                T_Id 
sem_Id_Tuple x1_  =
    (let _lhsOself :: Id 
         _self =
             (x1_)
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Identifier --------------------------------------------------
data Identifier  = Global (Id ) 
                 | Local (Id ) 
                 deriving ( Eq,Ord,Show)
-- cata
sem_Identifier :: Identifier  ->
                  T_Identifier 
sem_Identifier (Global _name )  =
    (sem_Identifier_Global (sem_Id _name ) )
sem_Identifier (Local _name )  =
    (sem_Identifier_Local (sem_Id _name ) )
-- semantic domain
type T_Identifier  = ( Identifier )
data Inh_Identifier  = Inh_Identifier {}
data Syn_Identifier  = Syn_Identifier {self_Syn_Identifier :: Identifier }
wrap_Identifier :: T_Identifier  ->
                   Inh_Identifier  ->
                   Syn_Identifier 
wrap_Identifier sem (Inh_Identifier )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Identifier _lhsOself ))
sem_Identifier_Global :: T_Id  ->
                         T_Identifier 
sem_Identifier_Global name_  =
    (let _lhsOself :: Identifier 
         _nameIself :: Id 
         _self =
             Global _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_ 
     in  ( _lhsOself))
sem_Identifier_Local :: T_Id  ->
                        T_Identifier 
sem_Identifier_Local name_  =
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
type Identifiers  = [Identifier ]
-- cata
sem_Identifiers :: Identifiers  ->
                   T_Identifiers 
sem_Identifiers list  =
    (Prelude.foldr sem_Identifiers_Cons sem_Identifiers_Nil (Prelude.map sem_Identifier list) )
-- semantic domain
type T_Identifiers  = ( Identifiers )
data Inh_Identifiers  = Inh_Identifiers {}
data Syn_Identifiers  = Syn_Identifiers {self_Syn_Identifiers :: Identifiers }
wrap_Identifiers :: T_Identifiers  ->
                    Inh_Identifiers  ->
                    Syn_Identifiers 
wrap_Identifiers sem (Inh_Identifiers )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Identifiers _lhsOself ))
sem_Identifiers_Cons :: T_Identifier  ->
                        T_Identifiers  ->
                        T_Identifiers 
sem_Identifiers_Cons hd_ tl_  =
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
sem_Identifiers_Nil  =
    (let _lhsOself :: Identifiers 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Instruction -------------------------------------------------
data Instruction  = Instruction (String) 
                  deriving ( Eq,Ord,Show)
-- cata
sem_Instruction :: Instruction  ->
                   T_Instruction 
sem_Instruction (Instruction _s )  =
    (sem_Instruction_Instruction _s )
-- semantic domain
type T_Instruction  = ( Instruction )
data Inh_Instruction  = Inh_Instruction {}
data Syn_Instruction  = Syn_Instruction {self_Syn_Instruction :: Instruction }
wrap_Instruction :: T_Instruction  ->
                    Inh_Instruction  ->
                    Syn_Instruction 
wrap_Instruction sem (Inh_Instruction )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Instruction _lhsOself ))
sem_Instruction_Instruction :: String ->
                               T_Instruction 
sem_Instruction_Instruction s_  =
    (let _lhsOself :: Instruction 
         _self =
             Instruction s_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Instructions ------------------------------------------------
type Instructions  = [Instruction ]
-- cata
sem_Instructions :: Instructions  ->
                    T_Instructions 
sem_Instructions list  =
    (Prelude.foldr sem_Instructions_Cons sem_Instructions_Nil (Prelude.map sem_Instruction list) )
-- semantic domain
type T_Instructions  = ( Instructions )
data Inh_Instructions  = Inh_Instructions {}
data Syn_Instructions  = Syn_Instructions {self_Syn_Instructions :: Instructions }
wrap_Instructions :: T_Instructions  ->
                     Inh_Instructions  ->
                     Syn_Instructions 
wrap_Instructions sem (Inh_Instructions )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Instructions _lhsOself ))
sem_Instructions_Cons :: T_Instruction  ->
                         T_Instructions  ->
                         T_Instructions 
sem_Instructions_Cons hd_ tl_  =
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
sem_Instructions_Nil  =
    (let _lhsOself :: Instructions 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- IntTyValId --------------------------------------------------
type IntTyValId  = ( Type ,Value ,Identifier )
-- cata
sem_IntTyValId :: IntTyValId  ->
                  T_IntTyValId 
sem_IntTyValId ( x1,x2,x3)  =
    (sem_IntTyValId_Tuple (sem_Type x1 ) (sem_Value x2 ) (sem_Identifier x3 ) )
-- semantic domain
type T_IntTyValId  = ( IntTyValId )
data Inh_IntTyValId  = Inh_IntTyValId {}
data Syn_IntTyValId  = Syn_IntTyValId {self_Syn_IntTyValId :: IntTyValId }
wrap_IntTyValId :: T_IntTyValId  ->
                   Inh_IntTyValId  ->
                   Syn_IntTyValId 
wrap_IntTyValId sem (Inh_IntTyValId )  =
    (let ( _lhsOself) = sem 
     in  (Syn_IntTyValId _lhsOself ))
sem_IntTyValId_Tuple :: T_Type  ->
                        T_Value  ->
                        T_Identifier  ->
                        T_IntTyValId 
sem_IntTyValId_Tuple x1_ x2_ x3_  =
    (let _lhsOself :: IntTyValId 
         _x1Iself :: Type 
         _x2Iself :: Value 
         _x3Iself :: Identifier 
         _self =
             (_x1Iself,_x2Iself,_x3Iself)
         _lhsOself =
             _self
         ( _x1Iself) =
             x1_ 
         ( _x2Iself) =
             x2_ 
         ( _x3Iself) =
             x3_ 
     in  ( _lhsOself))
-- IntTyValIdL -------------------------------------------------
type IntTyValIdL  = [IntTyValId ]
-- cata
sem_IntTyValIdL :: IntTyValIdL  ->
                   T_IntTyValIdL 
sem_IntTyValIdL list  =
    (Prelude.foldr sem_IntTyValIdL_Cons sem_IntTyValIdL_Nil (Prelude.map sem_IntTyValId list) )
-- semantic domain
type T_IntTyValIdL  = ( IntTyValIdL )
data Inh_IntTyValIdL  = Inh_IntTyValIdL {}
data Syn_IntTyValIdL  = Syn_IntTyValIdL {self_Syn_IntTyValIdL :: IntTyValIdL }
wrap_IntTyValIdL :: T_IntTyValIdL  ->
                    Inh_IntTyValIdL  ->
                    Syn_IntTyValIdL 
wrap_IntTyValIdL sem (Inh_IntTyValIdL )  =
    (let ( _lhsOself) = sem 
     in  (Syn_IntTyValIdL _lhsOself ))
sem_IntTyValIdL_Cons :: T_IntTyValId  ->
                        T_IntTyValIdL  ->
                        T_IntTyValIdL 
sem_IntTyValIdL_Cons hd_ tl_  =
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
sem_IntTyValIdL_Nil  =
    (let _lhsOself :: IntTyValIdL 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Ints --------------------------------------------------------
type Ints  = [(Int)]
-- cata
sem_Ints :: Ints  ->
            T_Ints 
sem_Ints list  =
    (Prelude.foldr sem_Ints_Cons sem_Ints_Nil list )
-- semantic domain
type T_Ints  = ( Ints )
data Inh_Ints  = Inh_Ints {}
data Syn_Ints  = Syn_Ints {self_Syn_Ints :: Ints }
wrap_Ints :: T_Ints  ->
             Inh_Ints  ->
             Syn_Ints 
wrap_Ints sem (Inh_Ints )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Ints _lhsOself ))
sem_Ints_Cons :: Int ->
                 T_Ints  ->
                 T_Ints 
sem_Ints_Cons hd_ tl_  =
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
sem_Ints_Nil  =
    (let _lhsOself :: Ints 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Label -------------------------------------------------------
type Label  = ( (String))
-- cata
sem_Label :: Label  ->
             T_Label 
sem_Label ( x1)  =
    (sem_Label_Tuple x1 )
-- semantic domain
type T_Label  = ( Label )
data Inh_Label  = Inh_Label {}
data Syn_Label  = Syn_Label {self_Syn_Label :: Label }
wrap_Label :: T_Label  ->
              Inh_Label  ->
              Syn_Label 
wrap_Label sem (Inh_Label )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Label _lhsOself ))
sem_Label_Tuple :: String ->
                   T_Label 
sem_Label_Tuple x1_  =
    (let _lhsOself :: Label 
         _self =
             (x1_)
         _lhsOself =
             _self
     in  ( _lhsOself))
-- LinkageTy ---------------------------------------------------
data LinkageTy  = Appending 
                | AvailableExternally 
                | Common 
                | Dllexport 
                | Dllimport 
                | ExternWeak 
                | External 
                | Internal 
                | LinkerPrivate 
                | LinkerPrivateWeak 
                | LinkerPrivateWeakDefAuto 
                | Linkonce 
                | LinkonceOdr 
                | Private 
                | Weak 
                | WeakOdr 
                deriving ( Eq,Ord,Show)
-- cata
sem_LinkageTy :: LinkageTy  ->
                 T_LinkageTy 
sem_LinkageTy (Appending )  =
    (sem_LinkageTy_Appending )
sem_LinkageTy (AvailableExternally )  =
    (sem_LinkageTy_AvailableExternally )
sem_LinkageTy (Common )  =
    (sem_LinkageTy_Common )
sem_LinkageTy (Dllexport )  =
    (sem_LinkageTy_Dllexport )
sem_LinkageTy (Dllimport )  =
    (sem_LinkageTy_Dllimport )
sem_LinkageTy (ExternWeak )  =
    (sem_LinkageTy_ExternWeak )
sem_LinkageTy (External )  =
    (sem_LinkageTy_External )
sem_LinkageTy (Internal )  =
    (sem_LinkageTy_Internal )
sem_LinkageTy (LinkerPrivate )  =
    (sem_LinkageTy_LinkerPrivate )
sem_LinkageTy (LinkerPrivateWeak )  =
    (sem_LinkageTy_LinkerPrivateWeak )
sem_LinkageTy (LinkerPrivateWeakDefAuto )  =
    (sem_LinkageTy_LinkerPrivateWeakDefAuto )
sem_LinkageTy (Linkonce )  =
    (sem_LinkageTy_Linkonce )
sem_LinkageTy (LinkonceOdr )  =
    (sem_LinkageTy_LinkonceOdr )
sem_LinkageTy (Private )  =
    (sem_LinkageTy_Private )
sem_LinkageTy (Weak )  =
    (sem_LinkageTy_Weak )
sem_LinkageTy (WeakOdr )  =
    (sem_LinkageTy_WeakOdr )
-- semantic domain
type T_LinkageTy  = ( LinkageTy )
data Inh_LinkageTy  = Inh_LinkageTy {}
data Syn_LinkageTy  = Syn_LinkageTy {self_Syn_LinkageTy :: LinkageTy }
wrap_LinkageTy :: T_LinkageTy  ->
                  Inh_LinkageTy  ->
                  Syn_LinkageTy 
wrap_LinkageTy sem (Inh_LinkageTy )  =
    (let ( _lhsOself) = sem 
     in  (Syn_LinkageTy _lhsOself ))
sem_LinkageTy_Appending :: T_LinkageTy 
sem_LinkageTy_Appending  =
    (let _lhsOself :: LinkageTy 
         _self =
             Appending
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_AvailableExternally :: T_LinkageTy 
sem_LinkageTy_AvailableExternally  =
    (let _lhsOself :: LinkageTy 
         _self =
             AvailableExternally
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_Common :: T_LinkageTy 
sem_LinkageTy_Common  =
    (let _lhsOself :: LinkageTy 
         _self =
             Common
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_Dllexport :: T_LinkageTy 
sem_LinkageTy_Dllexport  =
    (let _lhsOself :: LinkageTy 
         _self =
             Dllexport
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_Dllimport :: T_LinkageTy 
sem_LinkageTy_Dllimport  =
    (let _lhsOself :: LinkageTy 
         _self =
             Dllimport
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_ExternWeak :: T_LinkageTy 
sem_LinkageTy_ExternWeak  =
    (let _lhsOself :: LinkageTy 
         _self =
             ExternWeak
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_External :: T_LinkageTy 
sem_LinkageTy_External  =
    (let _lhsOself :: LinkageTy 
         _self =
             External
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_Internal :: T_LinkageTy 
sem_LinkageTy_Internal  =
    (let _lhsOself :: LinkageTy 
         _self =
             Internal
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_LinkerPrivate :: T_LinkageTy 
sem_LinkageTy_LinkerPrivate  =
    (let _lhsOself :: LinkageTy 
         _self =
             LinkerPrivate
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_LinkerPrivateWeak :: T_LinkageTy 
sem_LinkageTy_LinkerPrivateWeak  =
    (let _lhsOself :: LinkageTy 
         _self =
             LinkerPrivateWeak
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_LinkerPrivateWeakDefAuto :: T_LinkageTy 
sem_LinkageTy_LinkerPrivateWeakDefAuto  =
    (let _lhsOself :: LinkageTy 
         _self =
             LinkerPrivateWeakDefAuto
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_Linkonce :: T_LinkageTy 
sem_LinkageTy_Linkonce  =
    (let _lhsOself :: LinkageTy 
         _self =
             Linkonce
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_LinkonceOdr :: T_LinkageTy 
sem_LinkageTy_LinkonceOdr  =
    (let _lhsOself :: LinkageTy 
         _self =
             LinkonceOdr
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_Private :: T_LinkageTy 
sem_LinkageTy_Private  =
    (let _lhsOself :: LinkageTy 
         _self =
             Private
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_Weak :: T_LinkageTy 
sem_LinkageTy_Weak  =
    (let _lhsOself :: LinkageTy 
         _self =
             Weak
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LinkageTy_WeakOdr :: T_LinkageTy 
sem_LinkageTy_WeakOdr  =
    (let _lhsOself :: LinkageTy 
         _self =
             WeakOdr
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MAliases ----------------------------------------------------
type MAliases  = Maybe Aliases 
-- cata
sem_MAliases :: MAliases  ->
                T_MAliases 
sem_MAliases (Prelude.Just x )  =
    (sem_MAliases_Just (sem_Aliases x ) )
sem_MAliases Prelude.Nothing  =
    sem_MAliases_Nothing
-- semantic domain
type T_MAliases  = ( MAliases )
data Inh_MAliases  = Inh_MAliases {}
data Syn_MAliases  = Syn_MAliases {self_Syn_MAliases :: MAliases }
wrap_MAliases :: T_MAliases  ->
                 Inh_MAliases  ->
                 Syn_MAliases 
wrap_MAliases sem (Inh_MAliases )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MAliases _lhsOself ))
sem_MAliases_Just :: T_Aliases  ->
                     T_MAliases 
sem_MAliases_Just just_  =
    (let _lhsOself :: MAliases 
         _justIself :: Aliases 
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_ 
     in  ( _lhsOself))
sem_MAliases_Nothing :: T_MAliases 
sem_MAliases_Nothing  =
    (let _lhsOself :: MAliases 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MAlign ------------------------------------------------------
type MAlign  = Maybe Align 
-- cata
sem_MAlign :: MAlign  ->
              T_MAlign 
sem_MAlign (Prelude.Just x )  =
    (sem_MAlign_Just (sem_Align x ) )
sem_MAlign Prelude.Nothing  =
    sem_MAlign_Nothing
-- semantic domain
type T_MAlign  = ( MAlign )
data Inh_MAlign  = Inh_MAlign {}
data Syn_MAlign  = Syn_MAlign {self_Syn_MAlign :: MAlign }
wrap_MAlign :: T_MAlign  ->
               Inh_MAlign  ->
               Syn_MAlign 
wrap_MAlign sem (Inh_MAlign )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MAlign _lhsOself ))
sem_MAlign_Just :: T_Align  ->
                   T_MAlign 
sem_MAlign_Just just_  =
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
sem_MAlign_Nothing  =
    (let _lhsOself :: MAlign 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MCConv ------------------------------------------------------
type MCConv  = Maybe CConv 
-- cata
sem_MCConv :: MCConv  ->
              T_MCConv 
sem_MCConv (Prelude.Just x )  =
    (sem_MCConv_Just (sem_CConv x ) )
sem_MCConv Prelude.Nothing  =
    sem_MCConv_Nothing
-- semantic domain
type T_MCConv  = ( MCConv )
data Inh_MCConv  = Inh_MCConv {}
data Syn_MCConv  = Syn_MCConv {self_Syn_MCConv :: MCConv }
wrap_MCConv :: T_MCConv  ->
               Inh_MCConv  ->
               Syn_MCConv 
wrap_MCConv sem (Inh_MCConv )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MCConv _lhsOself ))
sem_MCConv_Just :: T_CConv  ->
                   T_MCConv 
sem_MCConv_Just just_  =
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
sem_MCConv_Nothing  =
    (let _lhsOself :: MCConv 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MConstant ---------------------------------------------------
type MConstant  = Maybe (Bool)
-- cata
sem_MConstant :: MConstant  ->
                 T_MConstant 
sem_MConstant (Prelude.Just x )  =
    (sem_MConstant_Just x )
sem_MConstant Prelude.Nothing  =
    sem_MConstant_Nothing
-- semantic domain
type T_MConstant  = ( MConstant )
data Inh_MConstant  = Inh_MConstant {}
data Syn_MConstant  = Syn_MConstant {self_Syn_MConstant :: MConstant }
wrap_MConstant :: T_MConstant  ->
                  Inh_MConstant  ->
                  Syn_MConstant 
wrap_MConstant sem (Inh_MConstant )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MConstant _lhsOself ))
sem_MConstant_Just :: Bool ->
                      T_MConstant 
sem_MConstant_Just just_  =
    (let _lhsOself :: MConstant 
         _self =
             Just just_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_MConstant_Nothing :: T_MConstant 
sem_MConstant_Nothing  =
    (let _lhsOself :: MConstant 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MDefinitionTy -----------------------------------------------
type MDefinitionTy  = Maybe DefinitionTy 
-- cata
sem_MDefinitionTy :: MDefinitionTy  ->
                     T_MDefinitionTy 
sem_MDefinitionTy (Prelude.Just x )  =
    (sem_MDefinitionTy_Just (sem_DefinitionTy x ) )
sem_MDefinitionTy Prelude.Nothing  =
    sem_MDefinitionTy_Nothing
-- semantic domain
type T_MDefinitionTy  = ( MDefinitionTy )
data Inh_MDefinitionTy  = Inh_MDefinitionTy {}
data Syn_MDefinitionTy  = Syn_MDefinitionTy {self_Syn_MDefinitionTy :: MDefinitionTy }
wrap_MDefinitionTy :: T_MDefinitionTy  ->
                      Inh_MDefinitionTy  ->
                      Syn_MDefinitionTy 
wrap_MDefinitionTy sem (Inh_MDefinitionTy )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MDefinitionTy _lhsOself ))
sem_MDefinitionTy_Just :: T_DefinitionTy  ->
                          T_MDefinitionTy 
sem_MDefinitionTy_Just just_  =
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
sem_MDefinitionTy_Nothing  =
    (let _lhsOself :: MDefinitionTy 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MGCName -----------------------------------------------------
type MGCName  = Maybe GCName 
-- cata
sem_MGCName :: MGCName  ->
               T_MGCName 
sem_MGCName (Prelude.Just x )  =
    (sem_MGCName_Just (sem_GCName x ) )
sem_MGCName Prelude.Nothing  =
    sem_MGCName_Nothing
-- semantic domain
type T_MGCName  = ( MGCName )
data Inh_MGCName  = Inh_MGCName {}
data Syn_MGCName  = Syn_MGCName {self_Syn_MGCName :: MGCName }
wrap_MGCName :: T_MGCName  ->
                Inh_MGCName  ->
                Syn_MGCName 
wrap_MGCName sem (Inh_MGCName )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MGCName _lhsOself ))
sem_MGCName_Just :: T_GCName  ->
                    T_MGCName 
sem_MGCName_Just just_  =
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
sem_MGCName_Nothing  =
    (let _lhsOself :: MGCName 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MLabel ------------------------------------------------------
type MLabel  = Maybe Label 
-- cata
sem_MLabel :: MLabel  ->
              T_MLabel 
sem_MLabel (Prelude.Just x )  =
    (sem_MLabel_Just (sem_Label x ) )
sem_MLabel Prelude.Nothing  =
    sem_MLabel_Nothing
-- semantic domain
type T_MLabel  = ( MLabel )
data Inh_MLabel  = Inh_MLabel {}
data Syn_MLabel  = Syn_MLabel {self_Syn_MLabel :: MLabel }
wrap_MLabel :: T_MLabel  ->
               Inh_MLabel  ->
               Syn_MLabel 
wrap_MLabel sem (Inh_MLabel )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MLabel _lhsOself ))
sem_MLabel_Just :: T_Label  ->
                   T_MLabel 
sem_MLabel_Just just_  =
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
sem_MLabel_Nothing  =
    (let _lhsOself :: MLabel 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MLinkageTy --------------------------------------------------
type MLinkageTy  = Maybe LinkageTy 
-- cata
sem_MLinkageTy :: MLinkageTy  ->
                  T_MLinkageTy 
sem_MLinkageTy (Prelude.Just x )  =
    (sem_MLinkageTy_Just (sem_LinkageTy x ) )
sem_MLinkageTy Prelude.Nothing  =
    sem_MLinkageTy_Nothing
-- semantic domain
type T_MLinkageTy  = ( MLinkageTy )
data Inh_MLinkageTy  = Inh_MLinkageTy {}
data Syn_MLinkageTy  = Syn_MLinkageTy {self_Syn_MLinkageTy :: MLinkageTy }
wrap_MLinkageTy :: T_MLinkageTy  ->
                   Inh_MLinkageTy  ->
                   Syn_MLinkageTy 
wrap_MLinkageTy sem (Inh_MLinkageTy )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MLinkageTy _lhsOself ))
sem_MLinkageTy_Just :: T_LinkageTy  ->
                       T_MLinkageTy 
sem_MLinkageTy_Just just_  =
    (let _lhsOself :: MLinkageTy 
         _justIself :: LinkageTy 
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_ 
     in  ( _lhsOself))
sem_MLinkageTy_Nothing :: T_MLinkageTy 
sem_MLinkageTy_Nothing  =
    (let _lhsOself :: MLinkageTy 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MModuleAsms -------------------------------------------------
type MModuleAsms  = Maybe ModuleAsms 
-- cata
sem_MModuleAsms :: MModuleAsms  ->
                   T_MModuleAsms 
sem_MModuleAsms (Prelude.Just x )  =
    (sem_MModuleAsms_Just (sem_ModuleAsms x ) )
sem_MModuleAsms Prelude.Nothing  =
    sem_MModuleAsms_Nothing
-- semantic domain
type T_MModuleAsms  = ( MModuleAsms )
data Inh_MModuleAsms  = Inh_MModuleAsms {}
data Syn_MModuleAsms  = Syn_MModuleAsms {self_Syn_MModuleAsms :: MModuleAsms }
wrap_MModuleAsms :: T_MModuleAsms  ->
                    Inh_MModuleAsms  ->
                    Syn_MModuleAsms 
wrap_MModuleAsms sem (Inh_MModuleAsms )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MModuleAsms _lhsOself ))
sem_MModuleAsms_Just :: T_ModuleAsms  ->
                        T_MModuleAsms 
sem_MModuleAsms_Just just_  =
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
sem_MModuleAsms_Nothing  =
    (let _lhsOself :: MModuleAsms 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MNamedTys ---------------------------------------------------
type MNamedTys  = Maybe NamedTys 
-- cata
sem_MNamedTys :: MNamedTys  ->
                 T_MNamedTys 
sem_MNamedTys (Prelude.Just x )  =
    (sem_MNamedTys_Just (sem_NamedTys x ) )
sem_MNamedTys Prelude.Nothing  =
    sem_MNamedTys_Nothing
-- semantic domain
type T_MNamedTys  = ( MNamedTys )
data Inh_MNamedTys  = Inh_MNamedTys {}
data Syn_MNamedTys  = Syn_MNamedTys {self_Syn_MNamedTys :: MNamedTys }
wrap_MNamedTys :: T_MNamedTys  ->
                  Inh_MNamedTys  ->
                  Syn_MNamedTys 
wrap_MNamedTys sem (Inh_MNamedTys )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MNamedTys _lhsOself ))
sem_MNamedTys_Just :: T_NamedTys  ->
                      T_MNamedTys 
sem_MNamedTys_Just just_  =
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
sem_MNamedTys_Nothing  =
    (let _lhsOself :: MNamedTys 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MParamAttrs -------------------------------------------------
type MParamAttrs  = Maybe ParamAttrs 
-- cata
sem_MParamAttrs :: MParamAttrs  ->
                   T_MParamAttrs 
sem_MParamAttrs (Prelude.Just x )  =
    (sem_MParamAttrs_Just (sem_ParamAttrs x ) )
sem_MParamAttrs Prelude.Nothing  =
    sem_MParamAttrs_Nothing
-- semantic domain
type T_MParamAttrs  = ( MParamAttrs )
data Inh_MParamAttrs  = Inh_MParamAttrs {}
data Syn_MParamAttrs  = Syn_MParamAttrs {self_Syn_MParamAttrs :: MParamAttrs }
wrap_MParamAttrs :: T_MParamAttrs  ->
                    Inh_MParamAttrs  ->
                    Syn_MParamAttrs 
wrap_MParamAttrs sem (Inh_MParamAttrs )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MParamAttrs _lhsOself ))
sem_MParamAttrs_Just :: T_ParamAttrs  ->
                        T_MParamAttrs 
sem_MParamAttrs_Just just_  =
    (let _lhsOself :: MParamAttrs 
         _justIself :: ParamAttrs 
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_ 
     in  ( _lhsOself))
sem_MParamAttrs_Nothing :: T_MParamAttrs 
sem_MParamAttrs_Nothing  =
    (let _lhsOself :: MParamAttrs 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MSection ----------------------------------------------------
type MSection  = Maybe Section 
-- cata
sem_MSection :: MSection  ->
                T_MSection 
sem_MSection (Prelude.Just x )  =
    (sem_MSection_Just (sem_Section x ) )
sem_MSection Prelude.Nothing  =
    sem_MSection_Nothing
-- semantic domain
type T_MSection  = ( MSection )
data Inh_MSection  = Inh_MSection {}
data Syn_MSection  = Syn_MSection {self_Syn_MSection :: MSection }
wrap_MSection :: T_MSection  ->
                 Inh_MSection  ->
                 Syn_MSection 
wrap_MSection sem (Inh_MSection )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MSection _lhsOself ))
sem_MSection_Just :: T_Section  ->
                     T_MSection 
sem_MSection_Just just_  =
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
sem_MSection_Nothing  =
    (let _lhsOself :: MSection 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MUnnamedAddr ------------------------------------------------
type MUnnamedAddr  = Maybe (Bool)
-- cata
sem_MUnnamedAddr :: MUnnamedAddr  ->
                    T_MUnnamedAddr 
sem_MUnnamedAddr (Prelude.Just x )  =
    (sem_MUnnamedAddr_Just x )
sem_MUnnamedAddr Prelude.Nothing  =
    sem_MUnnamedAddr_Nothing
-- semantic domain
type T_MUnnamedAddr  = ( MUnnamedAddr )
data Inh_MUnnamedAddr  = Inh_MUnnamedAddr {}
data Syn_MUnnamedAddr  = Syn_MUnnamedAddr {self_Syn_MUnnamedAddr :: MUnnamedAddr }
wrap_MUnnamedAddr :: T_MUnnamedAddr  ->
                     Inh_MUnnamedAddr  ->
                     Syn_MUnnamedAddr 
wrap_MUnnamedAddr sem (Inh_MUnnamedAddr )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MUnnamedAddr _lhsOself ))
sem_MUnnamedAddr_Just :: Bool ->
                         T_MUnnamedAddr 
sem_MUnnamedAddr_Just just_  =
    (let _lhsOself :: MUnnamedAddr 
         _self =
             Just just_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_MUnnamedAddr_Nothing :: T_MUnnamedAddr 
sem_MUnnamedAddr_Nothing  =
    (let _lhsOself :: MUnnamedAddr 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MValue ------------------------------------------------------
type MValue  = Maybe Value 
-- cata
sem_MValue :: MValue  ->
              T_MValue 
sem_MValue (Prelude.Just x )  =
    (sem_MValue_Just (sem_Value x ) )
sem_MValue Prelude.Nothing  =
    sem_MValue_Nothing
-- semantic domain
type T_MValue  = ( MValue )
data Inh_MValue  = Inh_MValue {}
data Syn_MValue  = Syn_MValue {self_Syn_MValue :: MValue }
wrap_MValue :: T_MValue  ->
               Inh_MValue  ->
               Syn_MValue 
wrap_MValue sem (Inh_MValue )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MValue _lhsOself ))
sem_MValue_Just :: T_Value  ->
                   T_MValue 
sem_MValue_Just just_  =
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
sem_MValue_Nothing  =
    (let _lhsOself :: MValue 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MVisibility -------------------------------------------------
type MVisibility  = Maybe Visibility 
-- cata
sem_MVisibility :: MVisibility  ->
                   T_MVisibility 
sem_MVisibility (Prelude.Just x )  =
    (sem_MVisibility_Just (sem_Visibility x ) )
sem_MVisibility Prelude.Nothing  =
    sem_MVisibility_Nothing
-- semantic domain
type T_MVisibility  = ( MVisibility )
data Inh_MVisibility  = Inh_MVisibility {}
data Syn_MVisibility  = Syn_MVisibility {self_Syn_MVisibility :: MVisibility }
wrap_MVisibility :: T_MVisibility  ->
                    Inh_MVisibility  ->
                    Syn_MVisibility 
wrap_MVisibility sem (Inh_MVisibility )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MVisibility _lhsOself ))
sem_MVisibility_Just :: T_Visibility  ->
                        T_MVisibility 
sem_MVisibility_Just just_  =
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
sem_MVisibility_Nothing  =
    (let _lhsOself :: MVisibility 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MapTyInt ----------------------------------------------------
type MapTyInt  = Map ((Type)) (Triplet )
-- cata
sem_MapTyInt :: MapTyInt  ->
                T_MapTyInt 
sem_MapTyInt m  =
    (Data.Map.foldrWithKey sem_MapTyInt_Entry sem_MapTyInt_Nil (Data.Map.map sem_Triplet m ) )
-- semantic domain
type T_MapTyInt  = ( MapTyInt )
data Inh_MapTyInt  = Inh_MapTyInt {}
data Syn_MapTyInt  = Syn_MapTyInt {self_Syn_MapTyInt :: MapTyInt }
wrap_MapTyInt :: T_MapTyInt  ->
                 Inh_MapTyInt  ->
                 Syn_MapTyInt 
wrap_MapTyInt sem (Inh_MapTyInt )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MapTyInt _lhsOself ))
sem_MapTyInt_Entry :: Type ->
                      T_Triplet  ->
                      T_MapTyInt  ->
                      T_MapTyInt 
sem_MapTyInt_Entry key_ val_ tl_  =
    (let _lhsOself :: MapTyInt 
         _valIself :: Triplet 
         _tlIself :: MapTyInt 
         _self =
             Map.insert key_ _valIself _tlIself
         _lhsOself =
             _self
         ( _valIself) =
             val_ 
         ( _tlIself) =
             tl_ 
     in  ( _lhsOself))
sem_MapTyInt_Nil :: T_MapTyInt 
sem_MapTyInt_Nil  =
    (let _lhsOself :: MapTyInt 
         _self =
             Map.empty
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Module ------------------------------------------------------
data Module  = Module (DataLayout ) (TargetData ) (Functions ) (GlobalVars ) 
             deriving ( Eq,Ord,Show)
-- cata
sem_Module :: Module  ->
              T_Module 
sem_Module (Module _layout _target _funs _gvars )  =
    (sem_Module_Module (sem_DataLayout _layout ) (sem_TargetData _target ) (sem_Functions _funs ) (sem_GlobalVars _gvars ) )
-- semantic domain
type T_Module  = ( Doc,Module )
data Inh_Module  = Inh_Module {}
data Syn_Module  = Syn_Module {pp_Syn_Module :: Doc,self_Syn_Module :: Module }
wrap_Module :: T_Module  ->
               Inh_Module  ->
               Syn_Module 
wrap_Module sem (Inh_Module )  =
    (let ( _lhsOpp,_lhsOself) = sem 
     in  (Syn_Module _lhsOpp _lhsOself ))
sem_Module_Module :: T_DataLayout  ->
                     T_TargetData  ->
                     T_Functions  ->
                     T_GlobalVars  ->
                     T_Module 
sem_Module_Module layout_ target_ funs_ gvars_  =
    (let _lhsOpp :: Doc
         _lhsOself :: Module 
         _layoutIpp :: Doc
         _layoutIself :: DataLayout 
         _targetIpp :: Doc
         _targetIself :: TargetData 
         _funsIpp :: Doc
         _funsIself :: Functions 
         _gvarsIpp :: Doc
         _gvarsIself :: GlobalVars 
         _lhsOpp =
             ({-# LINE 30 "src/Language/LLVMIR/PPrinter.ag" #-}
              _layoutIpp <$> _targetIpp <$> _gvarsIpp <$> _funsIpp
              {-# LINE 2378 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             Module _layoutIself _targetIself _funsIself _gvarsIself
         _lhsOself =
             _self
         ( _layoutIpp,_layoutIself) =
             layout_ 
         ( _targetIpp,_targetIself) =
             target_ 
         ( _funsIpp,_funsIself) =
             funs_ 
         ( _gvarsIpp,_gvarsIself) =
             gvars_ 
     in  ( _lhsOpp,_lhsOself))
-- ModuleAsm ---------------------------------------------------
data ModuleAsm  = ModuleAsm (String) 
-- cata
sem_ModuleAsm :: ModuleAsm  ->
                 T_ModuleAsm 
sem_ModuleAsm (ModuleAsm _asm )  =
    (sem_ModuleAsm_ModuleAsm _asm )
-- semantic domain
type T_ModuleAsm  = ( ModuleAsm )
data Inh_ModuleAsm  = Inh_ModuleAsm {}
data Syn_ModuleAsm  = Syn_ModuleAsm {self_Syn_ModuleAsm :: ModuleAsm }
wrap_ModuleAsm :: T_ModuleAsm  ->
                  Inh_ModuleAsm  ->
                  Syn_ModuleAsm 
wrap_ModuleAsm sem (Inh_ModuleAsm )  =
    (let ( _lhsOself) = sem 
     in  (Syn_ModuleAsm _lhsOself ))
sem_ModuleAsm_ModuleAsm :: String ->
                           T_ModuleAsm 
sem_ModuleAsm_ModuleAsm asm_  =
    (let _lhsOself :: ModuleAsm 
         _self =
             ModuleAsm asm_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- ModuleAsms --------------------------------------------------
type ModuleAsms  = [ModuleAsm ]
-- cata
sem_ModuleAsms :: ModuleAsms  ->
                  T_ModuleAsms 
sem_ModuleAsms list  =
    (Prelude.foldr sem_ModuleAsms_Cons sem_ModuleAsms_Nil (Prelude.map sem_ModuleAsm list) )
-- semantic domain
type T_ModuleAsms  = ( ModuleAsms )
data Inh_ModuleAsms  = Inh_ModuleAsms {}
data Syn_ModuleAsms  = Syn_ModuleAsms {self_Syn_ModuleAsms :: ModuleAsms }
wrap_ModuleAsms :: T_ModuleAsms  ->
                   Inh_ModuleAsms  ->
                   Syn_ModuleAsms 
wrap_ModuleAsms sem (Inh_ModuleAsms )  =
    (let ( _lhsOself) = sem 
     in  (Syn_ModuleAsms _lhsOself ))
sem_ModuleAsms_Cons :: T_ModuleAsm  ->
                       T_ModuleAsms  ->
                       T_ModuleAsms 
sem_ModuleAsms_Cons hd_ tl_  =
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
sem_ModuleAsms_Nil  =
    (let _lhsOself :: ModuleAsms 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- NamedTy -----------------------------------------------------
data NamedTy  = NamedTy (Id ) (Type ) 
              deriving ( Eq,Ord,Show)
-- cata
sem_NamedTy :: NamedTy  ->
               T_NamedTy 
sem_NamedTy (NamedTy _name _ty )  =
    (sem_NamedTy_NamedTy (sem_Id _name ) (sem_Type _ty ) )
-- semantic domain
type T_NamedTy  = ( NamedTy )
data Inh_NamedTy  = Inh_NamedTy {}
data Syn_NamedTy  = Syn_NamedTy {self_Syn_NamedTy :: NamedTy }
wrap_NamedTy :: T_NamedTy  ->
                Inh_NamedTy  ->
                Syn_NamedTy 
wrap_NamedTy sem (Inh_NamedTy )  =
    (let ( _lhsOself) = sem 
     in  (Syn_NamedTy _lhsOself ))
sem_NamedTy_NamedTy :: T_Id  ->
                       T_Type  ->
                       T_NamedTy 
sem_NamedTy_NamedTy name_ ty_  =
    (let _lhsOself :: NamedTy 
         _nameIself :: Id 
         _tyIself :: Type 
         _self =
             NamedTy _nameIself _tyIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_ 
         ( _tyIself) =
             ty_ 
     in  ( _lhsOself))
-- NamedTys ----------------------------------------------------
type NamedTys  = [NamedTy ]
-- cata
sem_NamedTys :: NamedTys  ->
                T_NamedTys 
sem_NamedTys list  =
    (Prelude.foldr sem_NamedTys_Cons sem_NamedTys_Nil (Prelude.map sem_NamedTy list) )
-- semantic domain
type T_NamedTys  = ( NamedTys )
data Inh_NamedTys  = Inh_NamedTys {}
data Syn_NamedTys  = Syn_NamedTys {self_Syn_NamedTys :: NamedTys }
wrap_NamedTys :: T_NamedTys  ->
                 Inh_NamedTys  ->
                 Syn_NamedTys 
wrap_NamedTys sem (Inh_NamedTys )  =
    (let ( _lhsOself) = sem 
     in  (Syn_NamedTys _lhsOself ))
sem_NamedTys_Cons :: T_NamedTy  ->
                     T_NamedTys  ->
                     T_NamedTys 
sem_NamedTys_Cons hd_ tl_  =
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
sem_NamedTys_Nil  =
    (let _lhsOself :: NamedTys 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- PTyInt ------------------------------------------------------
type PTyInt  = ( Type ,(Int))
-- cata
sem_PTyInt :: PTyInt  ->
              T_PTyInt 
sem_PTyInt ( x1,x2)  =
    (sem_PTyInt_Tuple (sem_Type x1 ) x2 )
-- semantic domain
type T_PTyInt  = ( PTyInt )
data Inh_PTyInt  = Inh_PTyInt {}
data Syn_PTyInt  = Syn_PTyInt {self_Syn_PTyInt :: PTyInt }
wrap_PTyInt :: T_PTyInt  ->
               Inh_PTyInt  ->
               Syn_PTyInt 
wrap_PTyInt sem (Inh_PTyInt )  =
    (let ( _lhsOself) = sem 
     in  (Syn_PTyInt _lhsOself ))
sem_PTyInt_Tuple :: T_Type  ->
                    Int ->
                    T_PTyInt 
sem_PTyInt_Tuple x1_ x2_  =
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
type PTyIntL  = [PTyInt ]
-- cata
sem_PTyIntL :: PTyIntL  ->
               T_PTyIntL 
sem_PTyIntL list  =
    (Prelude.foldr sem_PTyIntL_Cons sem_PTyIntL_Nil (Prelude.map sem_PTyInt list) )
-- semantic domain
type T_PTyIntL  = ( PTyIntL )
data Inh_PTyIntL  = Inh_PTyIntL {}
data Syn_PTyIntL  = Syn_PTyIntL {self_Syn_PTyIntL :: PTyIntL }
wrap_PTyIntL :: T_PTyIntL  ->
                Inh_PTyIntL  ->
                Syn_PTyIntL 
wrap_PTyIntL sem (Inh_PTyIntL )  =
    (let ( _lhsOself) = sem 
     in  (Syn_PTyIntL _lhsOself ))
sem_PTyIntL_Cons :: T_PTyInt  ->
                    T_PTyIntL  ->
                    T_PTyIntL 
sem_PTyIntL_Cons hd_ tl_  =
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
sem_PTyIntL_Nil  =
    (let _lhsOself :: PTyIntL 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- ParamAttr ---------------------------------------------------
data ParamAttr  = Byval 
                | Inreg 
                | Nest 
                | Noalias 
                | Nocapture 
                | Signext 
                | Sret 
                | Zeroext 
                deriving ( Eq,Ord,Show)
-- cata
sem_ParamAttr :: ParamAttr  ->
                 T_ParamAttr 
sem_ParamAttr (Byval )  =
    (sem_ParamAttr_Byval )
sem_ParamAttr (Inreg )  =
    (sem_ParamAttr_Inreg )
sem_ParamAttr (Nest )  =
    (sem_ParamAttr_Nest )
sem_ParamAttr (Noalias )  =
    (sem_ParamAttr_Noalias )
sem_ParamAttr (Nocapture )  =
    (sem_ParamAttr_Nocapture )
sem_ParamAttr (Signext )  =
    (sem_ParamAttr_Signext )
sem_ParamAttr (Sret )  =
    (sem_ParamAttr_Sret )
sem_ParamAttr (Zeroext )  =
    (sem_ParamAttr_Zeroext )
-- semantic domain
type T_ParamAttr  = ( ParamAttr )
data Inh_ParamAttr  = Inh_ParamAttr {}
data Syn_ParamAttr  = Syn_ParamAttr {self_Syn_ParamAttr :: ParamAttr }
wrap_ParamAttr :: T_ParamAttr  ->
                  Inh_ParamAttr  ->
                  Syn_ParamAttr 
wrap_ParamAttr sem (Inh_ParamAttr )  =
    (let ( _lhsOself) = sem 
     in  (Syn_ParamAttr _lhsOself ))
sem_ParamAttr_Byval :: T_ParamAttr 
sem_ParamAttr_Byval  =
    (let _lhsOself :: ParamAttr 
         _self =
             Byval
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ParamAttr_Inreg :: T_ParamAttr 
sem_ParamAttr_Inreg  =
    (let _lhsOself :: ParamAttr 
         _self =
             Inreg
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ParamAttr_Nest :: T_ParamAttr 
sem_ParamAttr_Nest  =
    (let _lhsOself :: ParamAttr 
         _self =
             Nest
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ParamAttr_Noalias :: T_ParamAttr 
sem_ParamAttr_Noalias  =
    (let _lhsOself :: ParamAttr 
         _self =
             Noalias
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ParamAttr_Nocapture :: T_ParamAttr 
sem_ParamAttr_Nocapture  =
    (let _lhsOself :: ParamAttr 
         _self =
             Nocapture
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ParamAttr_Signext :: T_ParamAttr 
sem_ParamAttr_Signext  =
    (let _lhsOself :: ParamAttr 
         _self =
             Signext
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ParamAttr_Sret :: T_ParamAttr 
sem_ParamAttr_Sret  =
    (let _lhsOself :: ParamAttr 
         _self =
             Sret
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_ParamAttr_Zeroext :: T_ParamAttr 
sem_ParamAttr_Zeroext  =
    (let _lhsOself :: ParamAttr 
         _self =
             Zeroext
         _lhsOself =
             _self
     in  ( _lhsOself))
-- ParamAttrs --------------------------------------------------
type ParamAttrs  = [ParamAttr ]
-- cata
sem_ParamAttrs :: ParamAttrs  ->
                  T_ParamAttrs 
sem_ParamAttrs list  =
    (Prelude.foldr sem_ParamAttrs_Cons sem_ParamAttrs_Nil (Prelude.map sem_ParamAttr list) )
-- semantic domain
type T_ParamAttrs  = ( ParamAttrs )
data Inh_ParamAttrs  = Inh_ParamAttrs {}
data Syn_ParamAttrs  = Syn_ParamAttrs {self_Syn_ParamAttrs :: ParamAttrs }
wrap_ParamAttrs :: T_ParamAttrs  ->
                   Inh_ParamAttrs  ->
                   Syn_ParamAttrs 
wrap_ParamAttrs sem (Inh_ParamAttrs )  =
    (let ( _lhsOself) = sem 
     in  (Syn_ParamAttrs _lhsOself ))
sem_ParamAttrs_Cons :: T_ParamAttr  ->
                       T_ParamAttrs  ->
                       T_ParamAttrs 
sem_ParamAttrs_Cons hd_ tl_  =
    (let _lhsOself :: ParamAttrs 
         _hdIself :: ParamAttr 
         _tlIself :: ParamAttrs 
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_ 
         ( _tlIself) =
             tl_ 
     in  ( _lhsOself))
sem_ParamAttrs_Nil :: T_ParamAttrs 
sem_ParamAttrs_Nil  =
    (let _lhsOself :: ParamAttrs 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Section -----------------------------------------------------
data Section  = Section (String) 
              deriving ( Eq,Ord,Show)
-- cata
sem_Section :: Section  ->
               T_Section 
sem_Section (Section _s )  =
    (sem_Section_Section _s )
-- semantic domain
type T_Section  = ( Section )
data Inh_Section  = Inh_Section {}
data Syn_Section  = Syn_Section {self_Syn_Section :: Section }
wrap_Section :: T_Section  ->
                Inh_Section  ->
                Syn_Section 
wrap_Section sem (Inh_Section )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Section _lhsOself ))
sem_Section_Section :: String ->
                       T_Section 
sem_Section_Section s_  =
    (let _lhsOself :: Section 
         _self =
             Section s_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Target ------------------------------------------------------
data Target  = Linux 
             | MacOs 
             deriving ( Eq,Ord,Show)
-- cata
sem_Target :: Target  ->
              T_Target 
sem_Target (Linux )  =
    (sem_Target_Linux )
sem_Target (MacOs )  =
    (sem_Target_MacOs )
-- semantic domain
type T_Target  = ( Doc,Target )
data Inh_Target  = Inh_Target {}
data Syn_Target  = Syn_Target {pp_Syn_Target :: Doc,self_Syn_Target :: Target }
wrap_Target :: T_Target  ->
               Inh_Target  ->
               Syn_Target 
wrap_Target sem (Inh_Target )  =
    (let ( _lhsOpp,_lhsOself) = sem 
     in  (Syn_Target _lhsOpp _lhsOself ))
sem_Target_Linux :: T_Target 
sem_Target_Linux  =
    (let _lhsOpp :: Doc
         _lhsOself :: Target 
         _lhsOpp =
             ({-# LINE 27 "src/Language/LLVMIR/PPrinter.ag" #-}
              text "Linux"
              {-# LINE 2805 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             Linux
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
sem_Target_MacOs :: T_Target 
sem_Target_MacOs  =
    (let _lhsOpp :: Doc
         _lhsOself :: Target 
         _lhsOpp =
             ({-# LINE 26 "src/Language/LLVMIR/PPrinter.ag" #-}
              text "MacOs"
              {-# LINE 2819 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             MacOs
         _lhsOself =
             _self
     in  ( _lhsOpp,_lhsOself))
-- TargetData --------------------------------------------------
data TargetData  = TargetData (String) (Target ) 
                 deriving ( Eq,Ord,Show)
-- cata
sem_TargetData :: TargetData  ->
                  T_TargetData 
sem_TargetData (TargetData _s _t )  =
    (sem_TargetData_TargetData _s (sem_Target _t ) )
-- semantic domain
type T_TargetData  = ( Doc,TargetData )
data Inh_TargetData  = Inh_TargetData {}
data Syn_TargetData  = Syn_TargetData {pp_Syn_TargetData :: Doc,self_Syn_TargetData :: TargetData }
wrap_TargetData :: T_TargetData  ->
                   Inh_TargetData  ->
                   Syn_TargetData 
wrap_TargetData sem (Inh_TargetData )  =
    (let ( _lhsOpp,_lhsOself) = sem 
     in  (Syn_TargetData _lhsOpp _lhsOself ))
sem_TargetData_TargetData :: String ->
                             T_Target  ->
                             T_TargetData 
sem_TargetData_TargetData s_ t_  =
    (let _lhsOpp :: Doc
         _lhsOself :: TargetData 
         _tIpp :: Doc
         _tIself :: Target 
         _lhsOpp =
             ({-# LINE 16 "src/Language/LLVMIR/PPrinter.ag" #-}
              _tIpp
              {-# LINE 2855 "src/Language/LLVMIR.hs" #-}
              )
         _self =
             TargetData s_ _tIself
         _lhsOself =
             _self
         ( _tIpp,_tIself) =
             t_ 
     in  ( _lhsOpp,_lhsOself))
-- Terminator --------------------------------------------------
data Terminator  = Br (Bool) (Identifier ) (Identifier ) 
                 | Ret (MValue ) 
                 | Switch (IntTyValIdL ) 
                 | UBr (Identifier ) 
                 deriving ( Eq,Ord,Show)
-- cata
sem_Terminator :: Terminator  ->
                  T_Terminator 
sem_Terminator (Br _v _t _f )  =
    (sem_Terminator_Br _v (sem_Identifier _t ) (sem_Identifier _f ) )
sem_Terminator (Ret _v )  =
    (sem_Terminator_Ret (sem_MValue _v ) )
sem_Terminator (Switch _elems )  =
    (sem_Terminator_Switch (sem_IntTyValIdL _elems ) )
sem_Terminator (UBr _d )  =
    (sem_Terminator_UBr (sem_Identifier _d ) )
-- semantic domain
type T_Terminator  = ( Terminator )
data Inh_Terminator  = Inh_Terminator {}
data Syn_Terminator  = Syn_Terminator {self_Syn_Terminator :: Terminator }
wrap_Terminator :: T_Terminator  ->
                   Inh_Terminator  ->
                   Syn_Terminator 
wrap_Terminator sem (Inh_Terminator )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Terminator _lhsOself ))
sem_Terminator_Br :: Bool ->
                     T_Identifier  ->
                     T_Identifier  ->
                     T_Terminator 
sem_Terminator_Br v_ t_ f_  =
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
sem_Terminator_Ret :: T_MValue  ->
                      T_Terminator 
sem_Terminator_Ret v_  =
    (let _lhsOself :: Terminator 
         _vIself :: MValue 
         _self =
             Ret _vIself
         _lhsOself =
             _self
         ( _vIself) =
             v_ 
     in  ( _lhsOself))
sem_Terminator_Switch :: T_IntTyValIdL  ->
                         T_Terminator 
sem_Terminator_Switch elems_  =
    (let _lhsOself :: Terminator 
         _elemsIself :: IntTyValIdL 
         _self =
             Switch _elemsIself
         _lhsOself =
             _self
         ( _elemsIself) =
             elems_ 
     in  ( _lhsOself))
sem_Terminator_UBr :: T_Identifier  ->
                      T_Terminator 
sem_Terminator_UBr d_  =
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
type Triplet  = ( (Int),(Int),(Int))
-- cata
sem_Triplet :: Triplet  ->
               T_Triplet 
sem_Triplet ( x1,x2,x3)  =
    (sem_Triplet_Tuple x1 x2 x3 )
-- semantic domain
type T_Triplet  = ( Triplet )
data Inh_Triplet  = Inh_Triplet {}
data Syn_Triplet  = Syn_Triplet {self_Syn_Triplet :: Triplet }
wrap_Triplet :: T_Triplet  ->
                Inh_Triplet  ->
                Syn_Triplet 
wrap_Triplet sem (Inh_Triplet )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Triplet _lhsOself ))
sem_Triplet_Tuple :: Int ->
                     Int ->
                     Int ->
                     T_Triplet 
sem_Triplet_Tuple x1_ x2_ x3_  =
    (let _lhsOself :: Triplet 
         _self =
             (x1_,x2_,x3_)
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Type --------------------------------------------------------
data Type  = ArrayTy (Ints ) (Type ) 
           | FpTy (FpTy ) 
           | FunctionTy (Types ) (Type ) 
           | IntTy (Int) 
           | LabelTy 
           | MetadataTy 
           | OpaqueTy 
           | PointerTy (Type ) 
           | StructureTy (Types ) 
           | VectorTy (Ints ) (Type ) 
           | VoidTy 
           | X86mmxTy 
           deriving ( Eq,Ord,Show)
-- cata
sem_Type :: Type  ->
            T_Type 
sem_Type (ArrayTy _elems _ty )  =
    (sem_Type_ArrayTy (sem_Ints _elems ) (sem_Type _ty ) )
sem_Type (FpTy _fp )  =
    (sem_Type_FpTy (sem_FpTy _fp ) )
sem_Type (FunctionTy _party _retty )  =
    (sem_Type_FunctionTy (sem_Types _party ) (sem_Type _retty ) )
sem_Type (IntTy _i )  =
    (sem_Type_IntTy _i )
sem_Type (LabelTy )  =
    (sem_Type_LabelTy )
sem_Type (MetadataTy )  =
    (sem_Type_MetadataTy )
sem_Type (OpaqueTy )  =
    (sem_Type_OpaqueTy )
sem_Type (PointerTy _ty )  =
    (sem_Type_PointerTy (sem_Type _ty ) )
sem_Type (StructureTy _tys )  =
    (sem_Type_StructureTy (sem_Types _tys ) )
sem_Type (VectorTy _elems _ty )  =
    (sem_Type_VectorTy (sem_Ints _elems ) (sem_Type _ty ) )
sem_Type (VoidTy )  =
    (sem_Type_VoidTy )
sem_Type (X86mmxTy )  =
    (sem_Type_X86mmxTy )
-- semantic domain
type T_Type  = ( Type )
data Inh_Type  = Inh_Type {}
data Syn_Type  = Syn_Type {self_Syn_Type :: Type }
wrap_Type :: T_Type  ->
             Inh_Type  ->
             Syn_Type 
wrap_Type sem (Inh_Type )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Type _lhsOself ))
sem_Type_ArrayTy :: T_Ints  ->
                    T_Type  ->
                    T_Type 
sem_Type_ArrayTy elems_ ty_  =
    (let _lhsOself :: Type 
         _elemsIself :: Ints 
         _tyIself :: Type 
         _self =
             ArrayTy _elemsIself _tyIself
         _lhsOself =
             _self
         ( _elemsIself) =
             elems_ 
         ( _tyIself) =
             ty_ 
     in  ( _lhsOself))
sem_Type_FpTy :: T_FpTy  ->
                 T_Type 
sem_Type_FpTy fp_  =
    (let _lhsOself :: Type 
         _fpIself :: FpTy 
         _self =
             FpTy _fpIself
         _lhsOself =
             _self
         ( _fpIself) =
             fp_ 
     in  ( _lhsOself))
sem_Type_FunctionTy :: T_Types  ->
                       T_Type  ->
                       T_Type 
sem_Type_FunctionTy party_ retty_  =
    (let _lhsOself :: Type 
         _partyIself :: Types 
         _rettyIself :: Type 
         _self =
             FunctionTy _partyIself _rettyIself
         _lhsOself =
             _self
         ( _partyIself) =
             party_ 
         ( _rettyIself) =
             retty_ 
     in  ( _lhsOself))
sem_Type_IntTy :: Int ->
                  T_Type 
sem_Type_IntTy i_  =
    (let _lhsOself :: Type 
         _self =
             IntTy i_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Type_LabelTy :: T_Type 
sem_Type_LabelTy  =
    (let _lhsOself :: Type 
         _self =
             LabelTy
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Type_MetadataTy :: T_Type 
sem_Type_MetadataTy  =
    (let _lhsOself :: Type 
         _self =
             MetadataTy
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Type_OpaqueTy :: T_Type 
sem_Type_OpaqueTy  =
    (let _lhsOself :: Type 
         _self =
             OpaqueTy
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Type_PointerTy :: T_Type  ->
                      T_Type 
sem_Type_PointerTy ty_  =
    (let _lhsOself :: Type 
         _tyIself :: Type 
         _self =
             PointerTy _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_ 
     in  ( _lhsOself))
sem_Type_StructureTy :: T_Types  ->
                        T_Type 
sem_Type_StructureTy tys_  =
    (let _lhsOself :: Type 
         _tysIself :: Types 
         _self =
             StructureTy _tysIself
         _lhsOself =
             _self
         ( _tysIself) =
             tys_ 
     in  ( _lhsOself))
sem_Type_VectorTy :: T_Ints  ->
                     T_Type  ->
                     T_Type 
sem_Type_VectorTy elems_ ty_  =
    (let _lhsOself :: Type 
         _elemsIself :: Ints 
         _tyIself :: Type 
         _self =
             VectorTy _elemsIself _tyIself
         _lhsOself =
             _self
         ( _elemsIself) =
             elems_ 
         ( _tyIself) =
             ty_ 
     in  ( _lhsOself))
sem_Type_VoidTy :: T_Type 
sem_Type_VoidTy  =
    (let _lhsOself :: Type 
         _self =
             VoidTy
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Type_X86mmxTy :: T_Type 
sem_Type_X86mmxTy  =
    (let _lhsOself :: Type 
         _self =
             X86mmxTy
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Types -------------------------------------------------------
type Types  = [Type ]
-- cata
sem_Types :: Types  ->
             T_Types 
sem_Types list  =
    (Prelude.foldr sem_Types_Cons sem_Types_Nil (Prelude.map sem_Type list) )
-- semantic domain
type T_Types  = ( Types )
data Inh_Types  = Inh_Types {}
data Syn_Types  = Syn_Types {self_Syn_Types :: Types }
wrap_Types :: T_Types  ->
              Inh_Types  ->
              Syn_Types 
wrap_Types sem (Inh_Types )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Types _lhsOself ))
sem_Types_Cons :: T_Type  ->
                  T_Types  ->
                  T_Types 
sem_Types_Cons hd_ tl_  =
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
sem_Types_Nil  =
    (let _lhsOself :: Types 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Value -------------------------------------------------------
data Value  = Const (Constant ) 
            | Id (Identifier ) 
            deriving ( Eq,Ord,Show)
-- cata
sem_Value :: Value  ->
             T_Value 
sem_Value (Const _c )  =
    (sem_Value_Const (sem_Constant _c ) )
sem_Value (Id _v )  =
    (sem_Value_Id (sem_Identifier _v ) )
-- semantic domain
type T_Value  = ( Value )
data Inh_Value  = Inh_Value {}
data Syn_Value  = Syn_Value {self_Syn_Value :: Value }
wrap_Value :: T_Value  ->
              Inh_Value  ->
              Syn_Value 
wrap_Value sem (Inh_Value )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Value _lhsOself ))
sem_Value_Const :: T_Constant  ->
                   T_Value 
sem_Value_Const c_  =
    (let _lhsOself :: Value 
         _cIself :: Constant 
         _self =
             Const _cIself
         _lhsOself =
             _self
         ( _cIself) =
             c_ 
     in  ( _lhsOself))
sem_Value_Id :: T_Identifier  ->
                T_Value 
sem_Value_Id v_  =
    (let _lhsOself :: Value 
         _vIself :: Identifier 
         _self =
             Id _vIself
         _lhsOself =
             _self
         ( _vIself) =
             v_ 
     in  ( _lhsOself))
-- Visibility --------------------------------------------------
data Visibility  = Default 
                 | Hidden 
                 | Protected 
                 deriving ( Eq,Ord,Show)
-- cata
sem_Visibility :: Visibility  ->
                  T_Visibility 
sem_Visibility (Default )  =
    (sem_Visibility_Default )
sem_Visibility (Hidden )  =
    (sem_Visibility_Hidden )
sem_Visibility (Protected )  =
    (sem_Visibility_Protected )
-- semantic domain
type T_Visibility  = ( Visibility )
data Inh_Visibility  = Inh_Visibility {}
data Syn_Visibility  = Syn_Visibility {self_Syn_Visibility :: Visibility }
wrap_Visibility :: T_Visibility  ->
                   Inh_Visibility  ->
                   Syn_Visibility 
wrap_Visibility sem (Inh_Visibility )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Visibility _lhsOself ))
sem_Visibility_Default :: T_Visibility 
sem_Visibility_Default  =
    (let _lhsOself :: Visibility 
         _self =
             Default
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Visibility_Hidden :: T_Visibility 
sem_Visibility_Hidden  =
    (let _lhsOself :: Visibility 
         _self =
             Hidden
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Visibility_Protected :: T_Visibility 
sem_Visibility_Protected  =
    (let _lhsOself :: Visibility 
         _self =
             Protected
         _lhsOself =
             _self
     in  ( _lhsOself))