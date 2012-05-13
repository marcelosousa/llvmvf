

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
-- CallingConvention -------------------------------------------
data CallingConvention  = Cc (Int) 
                        | Cc10 
                        | Ccc 
                        | Coldcc 
                        | Fastcc 
-- cata
sem_CallingConvention :: CallingConvention  ->
                         T_CallingConvention 
sem_CallingConvention (Cc _int )  =
    (sem_CallingConvention_Cc _int )
sem_CallingConvention (Cc10 )  =
    (sem_CallingConvention_Cc10 )
sem_CallingConvention (Ccc )  =
    (sem_CallingConvention_Ccc )
sem_CallingConvention (Coldcc )  =
    (sem_CallingConvention_Coldcc )
sem_CallingConvention (Fastcc )  =
    (sem_CallingConvention_Fastcc )
-- semantic domain
type T_CallingConvention  = ( CallingConvention )
data Inh_CallingConvention  = Inh_CallingConvention {}
data Syn_CallingConvention  = Syn_CallingConvention {self_Syn_CallingConvention :: CallingConvention }
wrap_CallingConvention :: T_CallingConvention  ->
                          Inh_CallingConvention  ->
                          Syn_CallingConvention 
wrap_CallingConvention sem (Inh_CallingConvention )  =
    (let ( _lhsOself) = sem 
     in  (Syn_CallingConvention _lhsOself ))
sem_CallingConvention_Cc :: Int ->
                            T_CallingConvention 
sem_CallingConvention_Cc int_  =
    (let _lhsOself :: CallingConvention 
         _self =
             Cc int_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_CallingConvention_Cc10 :: T_CallingConvention 
sem_CallingConvention_Cc10  =
    (let _lhsOself :: CallingConvention 
         _self =
             Cc10
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_CallingConvention_Ccc :: T_CallingConvention 
sem_CallingConvention_Ccc  =
    (let _lhsOself :: CallingConvention 
         _self =
             Ccc
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_CallingConvention_Coldcc :: T_CallingConvention 
sem_CallingConvention_Coldcc  =
    (let _lhsOself :: CallingConvention 
         _self =
             Coldcc
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_CallingConvention_Fastcc :: T_CallingConvention 
sem_CallingConvention_Fastcc  =
    (let _lhsOself :: CallingConvention 
         _self =
             Fastcc
         _lhsOself =
             _self
     in  ( _lhsOself))
-- DefinitionTy ------------------------------------------------
data DefinitionTy  = Constant 
                   | ThreadLocal 
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
-- Function ----------------------------------------------------
data Function  = Function (MLinkageTy ) (MVisibility ) (MCallingConvention ) (MUnnamedAddr ) (Id ) 
-- cata
sem_Function :: Function  ->
                T_Function 
sem_Function (Function _linkage _visibility _callconv _uaddr _name )  =
    (sem_Function_Function (sem_MLinkageTy _linkage ) (sem_MVisibility _visibility ) (sem_MCallingConvention _callconv ) (sem_MUnnamedAddr _uaddr ) (sem_Id _name ) )
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
sem_Function_Function :: T_MLinkageTy  ->
                         T_MVisibility  ->
                         T_MCallingConvention  ->
                         T_MUnnamedAddr  ->
                         T_Id  ->
                         T_Function 
sem_Function_Function linkage_ visibility_ callconv_ uaddr_ name_  =
    (let _lhsOself :: Function 
         _linkageIself :: MLinkageTy 
         _visibilityIself :: MVisibility 
         _callconvIself :: MCallingConvention 
         _uaddrIself :: MUnnamedAddr 
         _nameIself :: Id 
         _self =
             Function _linkageIself _visibilityIself _callconvIself _uaddrIself _nameIself
         _lhsOself =
             _self
         ( _linkageIself) =
             linkage_ 
         ( _visibilityIself) =
             visibility_ 
         ( _callconvIself) =
             callconv_ 
         ( _uaddrIself) =
             uaddr_ 
         ( _nameIself) =
             name_ 
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
-- LLModule ----------------------------------------------------
data LLModule  = LLModule (Functions ) (Identifiers ) 
-- cata
sem_LLModule :: LLModule  ->
                T_LLModule 
sem_LLModule (LLModule _funs _idents )  =
    (sem_LLModule_LLModule (sem_Functions _funs ) (sem_Identifiers _idents ) )
-- semantic domain
type T_LLModule  = ( LLModule )
data Inh_LLModule  = Inh_LLModule {}
data Syn_LLModule  = Syn_LLModule {self_Syn_LLModule :: LLModule }
wrap_LLModule :: T_LLModule  ->
                 Inh_LLModule  ->
                 Syn_LLModule 
wrap_LLModule sem (Inh_LLModule )  =
    (let ( _lhsOself) = sem 
     in  (Syn_LLModule _lhsOself ))
sem_LLModule_LLModule :: T_Functions  ->
                         T_Identifiers  ->
                         T_LLModule 
sem_LLModule_LLModule funs_ idents_  =
    (let _lhsOself :: LLModule 
         _funsIself :: Functions 
         _identsIself :: Identifiers 
         _self =
             LLModule _funsIself _identsIself
         _lhsOself =
             _self
         ( _funsIself) =
             funs_ 
         ( _identsIself) =
             idents_ 
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
-- MCallingConvention ------------------------------------------
type MCallingConvention  = Maybe CallingConvention 
-- cata
sem_MCallingConvention :: MCallingConvention  ->
                          T_MCallingConvention 
sem_MCallingConvention (Prelude.Just x )  =
    (sem_MCallingConvention_Just (sem_CallingConvention x ) )
sem_MCallingConvention Prelude.Nothing  =
    sem_MCallingConvention_Nothing
-- semantic domain
type T_MCallingConvention  = ( MCallingConvention )
data Inh_MCallingConvention  = Inh_MCallingConvention {}
data Syn_MCallingConvention  = Syn_MCallingConvention {self_Syn_MCallingConvention :: MCallingConvention }
wrap_MCallingConvention :: T_MCallingConvention  ->
                           Inh_MCallingConvention  ->
                           Syn_MCallingConvention 
wrap_MCallingConvention sem (Inh_MCallingConvention )  =
    (let ( _lhsOself) = sem 
     in  (Syn_MCallingConvention _lhsOself ))
sem_MCallingConvention_Just :: T_CallingConvention  ->
                               T_MCallingConvention 
sem_MCallingConvention_Just just_  =
    (let _lhsOself :: MCallingConvention 
         _justIself :: CallingConvention 
         _self =
             Just _justIself
         _lhsOself =
             _self
         ( _justIself) =
             just_ 
     in  ( _lhsOself))
sem_MCallingConvention_Nothing :: T_MCallingConvention 
sem_MCallingConvention_Nothing  =
    (let _lhsOself :: MCallingConvention 
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
-- UnnamedAddr -------------------------------------------------
data UnnamedAddr  = UnnamedAddr 
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