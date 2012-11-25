-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Extractor.Linkage
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Extractor.Linkage where
  
import qualified LLVM.FFI.Core as FFI

import qualified Language.LLVMIR as LL

convertLinkage :: FFI.Linkage -> LL.Linkage
convertLinkage FFI.ExternalLinkage                 = LL.ExternalLinkage
convertLinkage FFI.AvailableExternallyLinkage      = LL.AvailableExternallyLinkage
convertLinkage FFI.LinkOnceAnyLinkage              = LL.LinkOnceAnyLinkage
convertLinkage FFI.LinkOnceODRLinkage              = LL.LinkOnceODRLinkage
convertLinkage FFI.WeakAnyLinkage                  = LL.WeakAnyLinkage
convertLinkage FFI.WeakODRLinkage                  = LL.WeakODRLinkage
convertLinkage FFI.AppendingLinkage                = LL.AppendingLinkage
convertLinkage FFI.InternalLinkage                 = LL.InternalLinkage
convertLinkage FFI.PrivateLinkage                  = LL.PrivateLinkage
convertLinkage FFI.DLLImportLinkage                = LL.DLLImportLinkage
convertLinkage FFI.DLLExportLinkage                = LL.DLLExportLinkage
convertLinkage FFI.ExternalWeakLinkage             = LL.ExternalWeakLinkage
convertLinkage FFI.GhostLinkage                    = LL.GhostLinkage
convertLinkage FFI.CommonLinkage                   = LL.CommonLinkage
convertLinkage FFI.LinkerPrivateLinkage            = LL.LinkerPrivateLinkage
convertLinkage FFI.LinkerPrivateWeakLinkage        = LL.LinkerPrivateWeakLinkage
--convertLinkage FFI.LinkerPrivateWeakDefAutoLinkage = LL.LinkerPrivateWeakDefAutoLinkage