-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Printer
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Printer where

import Language.LLVMIR.Printer.Module 
import Language.LLVMIR
import UU.PPrint

class MPretty a where
  pretty' :: a -> Doc

--instance Pretty

instance Pretty Module where
    pretty mdl = pp_Syn_Module $ wrap_Module (sem_Module mdl) $ Inh_Module {}

instance Pretty DataLayout where
    pretty d = pp_Syn_DataLayout $ wrap_DataLayout (sem_DataLayout d) $ Inh_DataLayout {}

