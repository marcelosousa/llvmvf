

-- UUAGC 0.9.38.6 (src/Language/LLVMIR/Base.ag)
module Language.LLVMIR.Base where

{-# LINE 11 "src/Language/LLVMIR/Base.ag" #-}

import Prelude              hiding (sequence)
import Data.Char            (chr)
{-# LINE 11 "src/Language/LLVMIR/Base.hs" #-}
{-# LINE 1 "src/Language/LLVMIR/Base.ag" #-}

-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Base
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------
{-# LINE 18 "src/Language/LLVMIR/Base.hs" #-}
-- Align -------------------------------------------------------
data Align  = Align (Int) 
            deriving ( Eq,Show)
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
               deriving ( Eq,Show)
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
data BasicBlock  = BasicBlock (MLabel ) (Instructions ) (Terminator ) 
                 deriving ( Eq,Show)
-- cata
sem_BasicBlock :: BasicBlock  ->
                  T_BasicBlock 
sem_BasicBlock (BasicBlock _label _instrs _tmn )  =
    (sem_BasicBlock_BasicBlock (sem_MLabel _label ) (sem_Instructions _instrs ) (sem_Terminator _tmn ) )
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
sem_BasicBlock_BasicBlock :: T_MLabel  ->
                             T_Instructions  ->
                             T_Terminator  ->
                             T_BasicBlock 
sem_BasicBlock_BasicBlock label_ instrs_ tmn_  =
    (let _lhsOself :: BasicBlock 
         _labelIself :: MLabel 
         _instrsIself :: Instructions 
         _tmnIself :: Terminator 
         _self =
             BasicBlock _labelIself _instrsIself _tmnIself
         _lhsOself =
             _self
         ( _labelIself) =
             label_ 
         ( _instrsIself) =
             instrs_ 
         ( _tmnIself) =
             tmn_ 
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
            deriving ( Eq,Show)
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
-- DefinitionTy ------------------------------------------------
data DefinitionTy  = Constant 
                   | ThreadLocal 
                   deriving ( Eq,Show)
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
              deriving ( Eq,Show)
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
data Function  = FunctionDecl (MLinkageTy ) (MVisibility ) (MCConv ) (MUnnamedAddr ) (Type ) (MParamAttribute ) (Id ) (Arguments ) (MAlign ) (MGCName ) 
               | FunctionDef (MLinkageTy ) (MVisibility ) (MCConv ) (MUnnamedAddr ) (Type ) (MParamAttribute ) (Id ) (Arguments ) (FuncAttrs ) (MAlign ) (MGCName ) (BasicBlocks ) 
               deriving ( Eq,Show)
-- cata
sem_Function :: Function  ->
                T_Function 
sem_Function (FunctionDecl _linkage _visibility _cconv _uaddr _retty _paramattr _name _args _optAlign _gcName )  =
    (sem_Function_FunctionDecl (sem_MLinkageTy _linkage ) (sem_MVisibility _visibility ) (sem_MCConv _cconv ) (sem_MUnnamedAddr _uaddr ) (sem_Type _retty ) (sem_MParamAttribute _paramattr ) (sem_Id _name ) (sem_Arguments _args ) (sem_MAlign _optAlign ) (sem_MGCName _gcName ) )
sem_Function (FunctionDef _linkage _visibility _cconv _uaddr _retty _paramattr _name _args _fnAttrs _optAlign _gcName _bb )  =
    (sem_Function_FunctionDef (sem_MLinkageTy _linkage ) (sem_MVisibility _visibility ) (sem_MCConv _cconv ) (sem_MUnnamedAddr _uaddr ) (sem_Type _retty ) (sem_MParamAttribute _paramattr ) (sem_Id _name ) (sem_Arguments _args ) (sem_FuncAttrs _fnAttrs ) (sem_MAlign _optAlign ) (sem_MGCName _gcName ) (sem_BasicBlocks _bb ) )
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
sem_Function_FunctionDecl :: T_MLinkageTy  ->
                             T_MVisibility  ->
                             T_MCConv  ->
                             T_MUnnamedAddr  ->
                             T_Type  ->
                             T_MParamAttribute  ->
                             T_Id  ->
                             T_Arguments  ->
                             T_MAlign  ->
                             T_MGCName  ->
                             T_Function 
sem_Function_FunctionDecl linkage_ visibility_ cconv_ uaddr_ retty_ paramattr_ name_ args_ optAlign_ gcName_  =
    (let _lhsOself :: Function 
         _linkageIself :: MLinkageTy 
         _visibilityIself :: MVisibility 
         _cconvIself :: MCConv 
         _uaddrIself :: MUnnamedAddr 
         _rettyIself :: Type 
         _paramattrIself :: MParamAttribute 
         _nameIself :: Id 
         _argsIself :: Arguments 
         _optAlignIself :: MAlign 
         _gcNameIself :: MGCName 
         _self =
             FunctionDecl _linkageIself _visibilityIself _cconvIself _uaddrIself _rettyIself _paramattrIself _nameIself _argsIself _optAlignIself _gcNameIself
         _lhsOself =
             _self
         ( _linkageIself) =
             linkage_ 
         ( _visibilityIself) =
             visibility_ 
         ( _cconvIself) =
             cconv_ 
         ( _uaddrIself) =
             uaddr_ 
         ( _rettyIself) =
             retty_ 
         ( _paramattrIself) =
             paramattr_ 
         ( _nameIself) =
             name_ 
         ( _argsIself) =
             args_ 
         ( _optAlignIself) =
             optAlign_ 
         ( _gcNameIself) =
             gcName_ 
     in  ( _lhsOself))
sem_Function_FunctionDef :: T_MLinkageTy  ->
                            T_MVisibility  ->
                            T_MCConv  ->
                            T_MUnnamedAddr  ->
                            T_Type  ->
                            T_MParamAttribute  ->
                            T_Id  ->
                            T_Arguments  ->
                            T_FuncAttrs  ->
                            T_MAlign  ->
                            T_MGCName  ->
                            T_BasicBlocks  ->
                            T_Function 
sem_Function_FunctionDef linkage_ visibility_ cconv_ uaddr_ retty_ paramattr_ name_ args_ fnAttrs_ optAlign_ gcName_ bb_  =
    (let _lhsOself :: Function 
         _linkageIself :: MLinkageTy 
         _visibilityIself :: MVisibility 
         _cconvIself :: MCConv 
         _uaddrIself :: MUnnamedAddr 
         _rettyIself :: Type 
         _paramattrIself :: MParamAttribute 
         _nameIself :: Id 
         _argsIself :: Arguments 
         _fnAttrsIself :: FuncAttrs 
         _optAlignIself :: MAlign 
         _gcNameIself :: MGCName 
         _bbIself :: BasicBlocks 
         _self =
             FunctionDef _linkageIself _visibilityIself _cconvIself _uaddrIself _rettyIself _paramattrIself _nameIself _argsIself _fnAttrsIself _optAlignIself _gcNameIself _bbIself
         _lhsOself =
             _self
         ( _linkageIself) =
             linkage_ 
         ( _visibilityIself) =
             visibility_ 
         ( _cconvIself) =
             cconv_ 
         ( _uaddrIself) =
             uaddr_ 
         ( _rettyIself) =
             retty_ 
         ( _paramattrIself) =
             paramattr_ 
         ( _nameIself) =
             name_ 
         ( _argsIself) =
             args_ 
         ( _fnAttrsIself) =
             fnAttrs_ 
         ( _optAlignIself) =
             optAlign_ 
         ( _gcNameIself) =
             gcName_ 
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
type T_Functions  = ( Functions )
data Inh_Functions  = Inh_Functions {}
data Syn_Functions  = Syn_Functions {self_Syn_Functions :: Functions }
wrap_Functions :: T_Functions  ->
                  Inh_Functions  ->
                  Syn_Functions 
wrap_Functions sem (Inh_Functions )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Functions _lhsOself ))
sem_Functions_Cons :: T_Function  ->
                      T_Functions  ->
                      T_Functions 
sem_Functions_Cons hd_ tl_  =
    (let _lhsOself :: Functions 
         _hdIself :: Function 
         _tlIself :: Functions 
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_ 
         ( _tlIself) =
             tl_ 
     in  ( _lhsOself))
sem_Functions_Nil :: T_Functions 
sem_Functions_Nil  =
    (let _lhsOself :: Functions 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- GCName ------------------------------------------------------
data GCName  = GCName (String) 
             deriving ( Eq,Show)
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
                deriving ( Eq,Show)
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
type T_GlobalVars  = ( GlobalVars )
data Inh_GlobalVars  = Inh_GlobalVars {}
data Syn_GlobalVars  = Syn_GlobalVars {self_Syn_GlobalVars :: GlobalVars }
wrap_GlobalVars :: T_GlobalVars  ->
                   Inh_GlobalVars  ->
                   Syn_GlobalVars 
wrap_GlobalVars sem (Inh_GlobalVars )  =
    (let ( _lhsOself) = sem 
     in  (Syn_GlobalVars _lhsOself ))
sem_GlobalVars_Cons :: T_GlobalVar  ->
                       T_GlobalVars  ->
                       T_GlobalVars 
sem_GlobalVars_Cons hd_ tl_  =
    (let _lhsOself :: GlobalVars 
         _hdIself :: GlobalVar 
         _tlIself :: GlobalVars 
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_ 
         ( _tlIself) =
             tl_ 
     in  ( _lhsOself))
sem_GlobalVars_Nil :: T_GlobalVars 
sem_GlobalVars_Nil  =
    (let _lhsOself :: GlobalVars 
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
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
data Identifier  = Global (String) 
                 | Local (String) 
-- cata
sem_Identifier :: Identifier  ->
                  T_Identifier 
sem_Identifier (Global _name )  =
    (sem_Identifier_Global _name )
sem_Identifier (Local _name )  =
    (sem_Identifier_Local _name )
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
sem_Identifier_Global :: String ->
                         T_Identifier 
sem_Identifier_Global name_  =
    (let _lhsOself :: Identifier 
         _self =
             Global name_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Identifier_Local :: String ->
                        T_Identifier 
sem_Identifier_Local name_  =
    (let _lhsOself :: Identifier 
         _self =
             Local name_
         _lhsOself =
             _self
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
data Instruction  = Instruction 
                  deriving ( Eq,Show)
-- cata
sem_Instruction :: Instruction  ->
                   T_Instruction 
sem_Instruction (Instruction )  =
    (sem_Instruction_Instruction )
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
sem_Instruction_Instruction :: T_Instruction 
sem_Instruction_Instruction  =
    (let _lhsOself :: Instruction 
         _self =
             Instruction
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
-- Label -------------------------------------------------------
type Label  = ( (Int))
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
sem_Label_Tuple :: Int ->
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
                deriving ( Eq,Show)
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
-- LocalVar ----------------------------------------------------
data LocalVar  = NamedTy 
               | Register (Id ) 
               deriving ( Eq,Show)
-- cata
sem_LocalVar :: LocalVar  ->
                T_LocalVar 
sem_LocalVar (NamedTy )  =
    (sem_LocalVar_NamedTy )
sem_LocalVar (Register _name )  =
    (sem_LocalVar_Register (sem_Id _name ) )
-- semantic domain
type T_LocalVar  = ( LocalVar )
data Inh_LocalVar  = Inh_LocalVar {}
data Syn_LocalVar  = Syn_LocalVar {self_Syn_LocalVar :: LocalVar }
wrap_LocalVar :: T_LocalVar  ->
                 Inh_LocalVar  ->
                 Syn_LocalVar 
wrap_LocalVar sem (Inh_LocalVar )  =
    (let ( _lhsOself) = sem 
     in  (Syn_LocalVar _lhsOself ))
sem_LocalVar_NamedTy :: T_LocalVar 
sem_LocalVar_NamedTy  =
    (let _lhsOself :: LocalVar 
         _self =
             NamedTy
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_LocalVar_Register :: T_Id  ->
                         T_LocalVar 
sem_LocalVar_Register name_  =
    (let _lhsOself :: LocalVar 
         _nameIself :: Id 
         _self =
             Register _nameIself
         _lhsOself =
             _self
         ( _nameIself) =
             name_ 
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
-- MParamAttribute ---------------------------------------------
type MParamAttribute  = Maybe ParamAttribute 
-- cata
sem_MParamAttribute :: MParamAttribute  ->
                       T_MParamAttribute 
sem_MParamAttribute (Prelude.Just x )  =
    (sem_MParamAttribute_Just (sem_ParamAttribute x ) )
sem_MParamAttribute Prelude.Nothing  =
    sem_MParamAttribute_Nothing
-- semantic domain
type T_MParamAttribute  = ( MParamAttribute )
data Inh_MParamAttribute  = Inh_MParamAttribute {}
data Syn_MParamAttribute  = Syn_MParamAttribute {self_Syn_MParamAttribute :: MParamAttribute }
wrap_MParamAttribute :: T_MParamAttribute  ->
                        Inh_MParamAttribute  ->
                        Syn_MParamAttribute 
wrap_MParamAttribute sem (Inh_MParamAttribute )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MParamAttribute _lhsOself ))
sem_MParamAttribute_Just :: T_ParamAttribute  ->
                            T_MParamAttribute 
sem_MParamAttribute_Just just_  =
    (let _lhsOself :: MParamAttribute 
         _justIself :: ParamAttribute 
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_ 
     in  ( _lhsOself))
sem_MParamAttribute_Nothing :: T_MParamAttribute 
sem_MParamAttribute_Nothing  =
    (let _lhsOself :: MParamAttribute 
         _self =
             Nothing
         _lhsOself =
             _self
     in  ( _lhsOself))
-- MUnnamedAddr ------------------------------------------------
type MUnnamedAddr  = Maybe UnnamedAddr 
-- cata
sem_MUnnamedAddr :: MUnnamedAddr  ->
                    T_MUnnamedAddr 
sem_MUnnamedAddr (Prelude.Just x )  =
    (sem_MUnnamedAddr_Just (sem_UnnamedAddr x ) )
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
sem_MUnnamedAddr_Just :: T_UnnamedAddr  ->
                         T_MUnnamedAddr 
sem_MUnnamedAddr_Just just_  =
    (let _lhsOself :: MUnnamedAddr 
         _justIself :: UnnamedAddr 
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_ 
     in  ( _lhsOself))
sem_MUnnamedAddr_Nothing :: T_MUnnamedAddr 
sem_MUnnamedAddr_Nothing  =
    (let _lhsOself :: MUnnamedAddr 
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
-- Module ------------------------------------------------------
data Module  = Module (Functions ) (GlobalVars ) 
             deriving ( Eq,Show)
-- cata
sem_Module :: Module  ->
              T_Module 
sem_Module (Module _funs _gvars )  =
    (sem_Module_Module (sem_Functions _funs ) (sem_GlobalVars _gvars ) )
-- semantic domain
type T_Module  = ( Module )
data Inh_Module  = Inh_Module {}
data Syn_Module  = Syn_Module {self_Syn_Module :: Module }
wrap_Module :: T_Module  ->
               Inh_Module  ->
               Syn_Module 
wrap_Module sem (Inh_Module )  =
    (let ( _lhsOself) = sem 
     in  (Syn_Module _lhsOself ))
sem_Module_Module :: T_Functions  ->
                     T_GlobalVars  ->
                     T_Module 
sem_Module_Module funs_ gvars_  =
    (let _lhsOself :: Module 
         _funsIself :: Functions 
         _gvarsIself :: GlobalVars 
         _self =
             Module _funsIself _gvarsIself
         _lhsOself =
             _self
         ( _funsIself) =
             funs_ 
         ( _gvarsIself) =
             gvars_ 
     in  ( _lhsOself))
-- ParamAttribute ----------------------------------------------
data ParamAttribute  = ParaAttribute 
                     deriving ( Eq,Show)
-- cata
sem_ParamAttribute :: ParamAttribute  ->
                      T_ParamAttribute 
sem_ParamAttribute (ParaAttribute )  =
    (sem_ParamAttribute_ParaAttribute )
-- semantic domain
type T_ParamAttribute  = ( ParamAttribute )
data Inh_ParamAttribute  = Inh_ParamAttribute {}
data Syn_ParamAttribute  = Syn_ParamAttribute {self_Syn_ParamAttribute :: ParamAttribute }
wrap_ParamAttribute :: T_ParamAttribute  ->
                       Inh_ParamAttribute  ->
                       Syn_ParamAttribute 
wrap_ParamAttribute sem (Inh_ParamAttribute )  =
    (let ( _lhsOself) = sem 
     in  (Syn_ParamAttribute _lhsOself ))
sem_ParamAttribute_ParaAttribute :: T_ParamAttribute 
sem_ParamAttribute_ParaAttribute  =
    (let _lhsOself :: ParamAttribute 
         _self =
             ParaAttribute
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Terminator --------------------------------------------------
data Terminator  = Terminator 
                 deriving ( Eq,Show)
-- cata
sem_Terminator :: Terminator  ->
                  T_Terminator 
sem_Terminator (Terminator )  =
    (sem_Terminator_Terminator )
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
sem_Terminator_Terminator :: T_Terminator 
sem_Terminator_Terminator  =
    (let _lhsOself :: Terminator 
         _self =
             Terminator
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Type --------------------------------------------------------
data Type  = Intz 
           deriving ( Eq,Show)
-- cata
sem_Type :: Type  ->
            T_Type 
sem_Type (Intz )  =
    (sem_Type_Intz )
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
sem_Type_Intz :: T_Type 
sem_Type_Intz  =
    (let _lhsOself :: Type 
         _self =
             Intz
         _lhsOself =
             _self
     in  ( _lhsOself))
-- UnnamedAddr -------------------------------------------------
data UnnamedAddr  = UnnamedAddr 
                  deriving ( Eq,Show)
-- cata
sem_UnnamedAddr :: UnnamedAddr  ->
                   T_UnnamedAddr 
sem_UnnamedAddr (UnnamedAddr )  =
    (sem_UnnamedAddr_UnnamedAddr )
-- semantic domain
type T_UnnamedAddr  = ( UnnamedAddr )
data Inh_UnnamedAddr  = Inh_UnnamedAddr {}
data Syn_UnnamedAddr  = Syn_UnnamedAddr {self_Syn_UnnamedAddr :: UnnamedAddr }
wrap_UnnamedAddr :: T_UnnamedAddr  ->
                    Inh_UnnamedAddr  ->
                    Syn_UnnamedAddr 
wrap_UnnamedAddr sem (Inh_UnnamedAddr )  =
    (let ( _lhsOself) = sem 
     in  (Syn_UnnamedAddr _lhsOself ))
sem_UnnamedAddr_UnnamedAddr :: T_UnnamedAddr 
sem_UnnamedAddr_UnnamedAddr  =
    (let _lhsOself :: UnnamedAddr 
         _self =
             UnnamedAddr
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Visibility --------------------------------------------------
data Visibility  = Default 
                 | Hidden 
                 | Protected 
                 deriving ( Eq,Show)
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