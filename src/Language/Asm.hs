{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
-------------------------------------------------------------------------------
-- Module    :  Language.Asm
-- Copyright :  (c) 2013 Marcelo Sousa
-- Inline Asm 
-------------------------------------------------------------------------------

module Language.Asm where

import Text.ParserCombinators.UU hiding (parse)
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances 
import Text.ParserCombinators.UU.Demo.Examples hiding (Parser)

import Prelude.Unicode ((⧺),(≡))
import Data.Char 

import Language.LLVMIR hiding (Instruction(..))

infixl 5 **>

-- | '**>' is a version of '*>' with increased fixity.
(**>) :: Applicative f => f a -> f b -> f b
p **> q = p *> q


type Asm = ([Directive],[(Maybe String, [GAS])])

-- GAS instructions generally have the form mnemonic source, destination. 
data GAS = Nop
         | Add Type Operand Operand
         | Mov Type Operand Operand
         | Cmpxchg Type Operand Operand
         | Lock
  deriving Show

data Directive = Pushsection String String
			   | Balign Int
			   | Long String 
			   | Popsection
  deriving Show

data Operand = Lit Int 
             | Reg String
             | CReg String
  deriving Show

-- |'Char' ~=> @[a-zA-Z0-9%$]@
pChar :: Parser Char
pChar = pLower <|> pUpper <|> pDigit -- <|> pAnySym "%$*:()"

pAChar :: Parser Char
pAChar = pLower <|> pUpper <|> pDigit <|> pAnySym "._\"-"

pString ∷ Parser String
pString = pList1 pAChar

pType' ∷ Parser Type
pType' =  const (TyInt 8)  <$> pSym 'b'
      <|> const (TyInt 16) <$> pSym 's'
      <|> const (TyInt 16) <$> pSym 'w'
      <|> const (TyInt 32) <$> pSym 'l'
      <|> const (TyInt 64) <$> pSym 'q'
      <|> const (TyFloatPoint Tyx86FP80) <$> pSym 't'

pType ∷ Parser Type
pType = pType' `opt` (TyInt 32)

-- digit2Num converts a char to a num.
digit2Num :: Num a => Char -> a
digit2Num a = fromInteger $ toInteger $ ord a - ord '0'

-- | 'pSNumeral' converts a string form of a <numeral> into a Num @a@.
pSNumeral :: Num a => Parser a
pSNumeral = foldl (\a b -> a * 10 + (digit2Num b)) 0 <$> pList1 pDigit <?> "<numeral>"

pBinOp ∷ Parser (Operand → Operand → GAS)
pBinOp =  Add <$> pToken "add" **> pType
      <|> Mov <$> pToken "mov" **> pType
      <|> Cmpxchg <$> pToken "cmpxchg" **> pType


pOperand ∷ Parser Operand
pOperand =  Lit <$> pSym '$' **> pSym '$' **> pSNumeral
        <|> Reg <$> pSym '$' **> pList1 pChar
        <|> CReg <$> pSym '%' **> pList1 pChar

pCmd ∷ Parser GAS
pCmd =  (\op b c → op b c) <$> pBinOp <*> pSpaces **> pOperand <*> pSym ',' **> pSpaces **> pOperand
    <|> const Lock <$> pToken "lock"

pGAS ∷ Parser GAS
pGAS = pSpaces **> pCmd <* pSpaces **> pSym ';'

pSec ∷ Parser (Maybe String)
pSec = (\a b → Just a) <$> pList1 pChar <*> pToken ":\n\t"

pSection ∷ Parser (Maybe String, [GAS])
pSection =  (,) <$> (pSec `opt` Nothing) <*> pList1 pGAS

pDir ∷ Parser Directive 
pDir =  Pushsection <$> pToken ".pushsection" **> pSpaces **> pString <*> pSym ',' **> pSpaces **> pString
    <|> Balign      <$> pToken ".balign" **> pSpaces **> pSNumeral
    <|> Long        <$> pToken ".long" **> pSpaces **> (pList1 (pAChar <|> pSym ' '))
    <|> const Popsection  <$> pToken ".popsection"

pDirective ∷ Parser Directive
pDirective = pDir <* pSym '\n'

pAsm ∷ Parser Asm
pAsm = (,) <$> pList pDirective <*> pList pSection

parseAsm ∷ String → Asm
parseAsm s | last s ≡ ';' = runParser "Error Asm" pAsm s
           | otherwise    = runParser "Error Asm" pAsm (s ⧺ ";")

transform ∷ Asm → Function
transform = undefined