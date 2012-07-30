-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Encoder
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Encoder(encode) where

import Language.LLVMIR.Encoder.Module 
import Language.LLVMIR
import Language.SMTLib2

encode :: Module -> SMod
encode mdl = enc_Syn_Module $ wrap_Module (sem_Module mdl) $ Inh_Module {}
