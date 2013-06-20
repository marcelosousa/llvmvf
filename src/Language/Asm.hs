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

--import Debug.Trace

data TyGas = I Int
           | Fp Int
   deriving (Eq,Ord,Show)

infixl 5 **>

-- | '**>' is a version of '*>' with increased fixity.
(**>) :: Applicative f => f a -> f b -> f b
p **> q = p *> q


type Asm = ([Directive],[(Maybe String, [GAS])])

-- GAS instructions generally have the form mnemonic source, destination. 
data GAS = Nop
         | Add TyGas Operand Operand
         | Sub TyGas Operand Operand
         | Mov TyGas Operand Operand
         | Cmpxchg TyGas Operand Operand
         | Xchg TyGas Operand Operand
         | Sete Operand
         | Bswap TyGas Operand
         | Lock
  deriving (Eq,Ord,Show)

data Directive = Pushsection String String
			   | Balign Int
			   | Long String 
			   | Popsection
  deriving (Eq,Ord,Show)

data Operand = Lit Int 
             | Reg String
             | CReg String
  deriving (Eq,Ord,Show)

-- |'Char' ~=> @[a-zA-Z0-9%$]@
pChar :: Parser Char
pChar = pLower <|> pUpper <|> pDigit -- <|> pAnySym "%$*:()"

pAChar :: Parser Char
pAChar = pLower <|> pUpper <|> pDigit <|> pAnySym "._\"-"

pString ∷ Parser String
pString = pList1 pAChar

pType' ∷ Parser TyGas
pType' =  const (I 8)  <$> pSym 'b'
      <|> const (I 16) <$> pSym 's'
      <|> const (I 16) <$> pSym 'w'
      <|> const (I 32) <$> pSym 'l'
      <|> const (I 64) <$> pSym 'q'
      <|> const (Fp 80) <$> pSym 't'

pType ∷ Parser TyGas
pType = pType' `opt` (I 32)

-- digit2Num converts a char to a num.
digit2Num :: Num a => Char -> a
digit2Num a = fromInteger $ toInteger $ ord a - ord '0'

-- | 'pSNumeral' converts a string form of a <numeral> into a Num @a@.
pSNumeral :: Num a => Parser a
pSNumeral = foldl (\a b -> a * 10 + (digit2Num b)) 0 <$> pList1 pDigit <?> "<numeral>"

pUnOp ∷ Parser (Operand → GAS)
pUnOp =   const Sete <$> pToken "sete"
      <|> Bswap <$> pToken "bswap" **> pType

pBinOp ∷ Parser (Operand → Operand → GAS)
pBinOp =  Add <$> pToken "add" **> pType
      <|> Mov <$> pToken "mov" **> pType
      <|> Sub <$> pToken "sub" **> pType
      <|> Cmpxchg <$> pToken "cmpxchg" **> pType
      <|> Xchg <$> pToken "xchg" **> pType

pOperand ∷ Parser Operand
pOperand =  Lit <$> pSym '$' **> pSym '$' **> pSNumeral
        <|> Reg <$> pSym '$' **> pList1 pChar
        <|> CReg <$> pSym '%' **> pList1 pChar

pCmd ∷ Parser GAS
pCmd =  (\op b c → op b c) <$> pBinOp <*> pSpaces **> pOperand <*> pSym ',' **> pSpaces **> pOperand
	<|> (\op a → op a)     <$> pUnOp <*> pSpaces **> pOperand
	<|> (\τ α → Add τ (Lit (-1)) α) <$> pToken "dec" **> pType <*> pSpaces **> pOperand
	<|> (\τ α → Add τ (Lit 1) α)    <$> pToken "inc" **> pType <*> pSpaces **> pOperand
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
parseAsm "" = ([],[])
parseAsm s | last s ≡ ';' = runParser "Error Asm2" pAsm s
           | otherwise    = runParser "Error Asm" pAsm (s ⧺ ";")

type AsmCs = [AsmC]

data AsmC = IC GasC | OC GasC | FC String
  deriving (Eq,Ord,Show)

data GasC = PosC Int
          | MemC
          | RegC
          | IRegC
          | CRegC String
  deriving (Eq,Ord,Show)

parseAsmC ∷ String → [AsmC]
parseAsmC = runParser "Error AsmC" (pComma `pListSep` pAsmC) 

pAsmC ∷ Parser AsmC
pAsmC =  OC <$> pSym '=' **> pGasC
     <|> IC <$> pGasC
     <|> FC <$> pToken "~{memory}"
     <|> FC <$> pToken "~{dirflag}"
     <|> FC <$> pToken "~{fpsr}"
     <|> FC <$> pToken "~{flags}"
     <|> FC <$> pToken "~{cc}"

pGasC ∷ Parser GasC
pGasC =  const (CRegC "eax") <$> pToken "{ax}"
     <|> const MemC <$> (pToken "*m" <|> pToken "*qm")
     <|> const RegC <$> pToken "r"
     <|> const IRegC <$> pToken "ir"
     <|> (PosC . digit2Num)  <$> pDigit

