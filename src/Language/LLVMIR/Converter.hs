-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Converter
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Converter where

--import Language.LLVMIR.Converter.Module 
import Language.LLVMIR
import Language.HTm.Base

llvmir2Htm :: Module -> HTm
llvmir2Htm mdl = undefined --htm_Syn_Module $ wrap_Module (sem_Module mdl) $ Inh_Module {}
