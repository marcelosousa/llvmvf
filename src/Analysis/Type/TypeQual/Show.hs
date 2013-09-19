{-# LANGUAGE UnicodeSyntax #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Type.TypeQual.Show
-- Copyright :  (c) 2013 Marcelo Sousa
-- Type Qualifiers 
-------------------------------------------------------------------------------

instance Show TyAnn where
  show TyBot = "⊥"
  show (TyPri t) = show t
  show (TyDer t) = show t
 
instance Show TyPri where
  show TyVoid     = "void"
  show TyFloat    = "float"
  show TyLabel    = "label"
  show TyMetadata = "metadata"
  show (TyInt n)  = "i"++show n

instance (Show α, Show (TypeQual α)) => Show (TyDer α) where
  show (TyAgg t) = show t
  show (TyVec n t) = "<" ++ show n ++ " x " ++ show t ++ ">"
  show (TyFun [] t _) = "fn :: " ++ show t
  show (TyFun tys t _) = "fn :: " ++ (foldr (\x s -> show x ++ " -> " ++ s) (show t) tys)
  show (TyPtr ty tya) = "*(" ++ show ty ++ ") " ++ show tya

instance (Show α, Show (TypeQual α)) => Show (TyAgg α) where
  show (TyArr n t) = "[" ++ show n ++ " x " ++ show t ++ "]"
  show (TyStr nm n []) = "{" ++ nm ++ "}"
  show (TyStr nm n t) = "{" ++ (foldr (\x s -> show x ++ ", " ++ s) (show $ last t) (init t)) ++ "}"

instance Show TyAnnot where
  show TyIOAddr = "IOAddr"
  show (TyRegAddr t) = show t
  show TyAny = "AnyAddr"

instance Show TyRegAddr where
  show UserAddr = "UVirtualAddr"
  show (KernelAddr ka) = show ka
  show AnyRegAddr = "AnyRegAddr"
