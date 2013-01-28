module Language.HTm.Base where

-- First Untyped
import UU.PPrint as P

type Ident = String

data HTm = HTm ETm
  deriving (Show,Eq)

data ETm = EBot
         | ENum Int
         | EVar Ident 
         | ELet Ident ETm ETm
         | EFun Ident [ETm] ETm
         | EApp ETm ETm
         | EAbs ETm ETm
         | EIf ETm ETm ETm
  deriving (Show,Eq)

instance Pretty HTm where
  pretty (HTm e) = pretty e 

instance Pretty ETm where
  pretty EBot = text "_|_"
  pretty (ENum i) = int i
  pretty (EVar x) = text x
  pretty (ELet a b t) = text "let" <+> pretty a <+> char '=' <+> pretty b <$> text "in" <+> pretty t
  pretty (EFun x a t) = text "fun" <+> pretty x <> char '(' <> text (show a) <> char ')' <> char '{' <$> indent 3 (pretty t) <$> char '}'
  pretty (EApp a b) = pretty a <> char '(' <> pretty b <> char ')'
  pretty (EAbs x t) = char '\\' <> pretty x <+> text "->" <+> pretty t
  pretty (EIf c t f) = text "if(" <> pretty c <> text ")" <$> indent 3 (text "then{" <$> indent 3 (pretty t) <$> char '}' <> text "else{" <$> indent 3 (pretty f) <$> char '}')
