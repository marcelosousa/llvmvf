{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.LinuxKernel
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Qualifiers to Analyse the Linux Kernel
-------------------------------------------------------------------------------

data TyAnnot = TyIOAddr
             | TyRegAddr TyRegAddr
             | TyAny 
  deriving (Eq, Ord)

data TyRegAddr = 
    UserAddr
  | KernelAddr KernelAddr
  | AnyRegAddr
  deriving (Eq, Ord)

data UserAddr = UserVirtualAddr
  deriving (Eq, Ord)

data KernelAddr = 
    KernelLogicalAddr 
  | KernelVirtualAddr
--  | KernelPhysicalAddr
  deriving (Eq, Ord)

anyRegAddr ∷ TyAnnot
anyRegAddr = TyRegAddr AnyRegAddr

kLogAddr ∷ TyAnnot
kLogAddr = TyRegAddr $ KernelAddr $ KernelLogicalAddr

kVirAddr ∷ TyAnnot
kVirAddr = TyRegAddr $ KernelAddr $ KernelVirtualAddr

uVirAddr ∷ TyAnnot
uVirAddr = TyRegAddr UserAddr

instance Show KernelAddr where
  show KernelLogicalAddr = "KLogicalAddr"
  show KernelVirtualAddr = "KVirtualAddr"

instance PartialOrder TyAnnot where
  TyIOAddr ≌ (TyRegAddr α) = Nothing
  TyIOAddr ≌ _             = Just TyIOAddr
  (TyRegAddr α) ≌ TyIOAddr = Nothing
  (TyRegAddr α) ≌ TyAny     = Just $ TyRegAddr α
  (TyRegAddr α) ≌ (TyRegAddr β) = TyRegAddr <$> α ≌ β
  TyAny ≌ α     = Just α

instance PartialOrder TyRegAddr where
  UserAddr ≌ (KernelAddr β) = Nothing
  UserAddr ≌ _ = Just UserAddr
  (KernelAddr α) ≌ UserAddr = Nothing
  (KernelAddr α) ≌ AnyRegAddr = Just $ KernelAddr α
  (KernelAddr α) ≌ (KernelAddr β) = 
    if α == β
    then Just (KernelAddr α)
    else Nothing
  AnyRegAddr  ≌ α = Just α
