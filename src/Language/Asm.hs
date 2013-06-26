{-# LANGUAGE UnicodeSyntax, FlexibleContexts, PatternGuards #-}
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

import Debug.Trace

data TyGas = I Int
           | Fp Int
   deriving (Eq,Ord,Show)

infixl 5 **>

-- | '**>' is a version of '*>' with increased fixity.
(**>) :: Applicative f => f a -> f b -> f b
p **> q = p *> q


type Asm = ([Directive],[(Maybe String, [GAS])])

-- GAS instructions generally have the form mnemonic source, destination. 
data GAS = Nop | Cli | Sti | Lock
         | Add TyGas Operand Operand
         | Sub TyGas Operand Operand
         | Mov TyGas Operand Operand
         | Bt  TyGas Operand Operand
         | Sbb TyGas Operand Operand
         | Cmp TyGas Operand Operand
         | Cmpxchg TyGas Operand Operand
         | Xchg TyGas Operand Operand
         | Sete Operand
         | Bswap TyGas Operand
         | Xadd TyGas Operand Operand
         | Bug
         | Canary TyGas Operand
         | Call String (Maybe Operand)
  deriving (Eq,Ord,Show)

data Directive = Pushsection PushDirective
			   | Balign Int
			   | Long String 
			   | Popsection
         | Word String
         | Byte String
  deriving (Eq,Ord,Show)

data PushDirective = 
    Altinstructions String
  | AltinstrReplacement String
  | Discard String String
  | Smplocks String
  deriving (Eq,Ord,Show)

data Operand = Lit Int 
             | Reg String
             | CReg String
             | Seg String Int
  deriving (Eq,Ord,Show)

-- |'Char' ~=> @[a-zA-Z0-9%$]@
pChar :: Parser Char
pChar = pLower <|> pUpper <|> pDigit -- <|> pAnySym "%$*:()"

pAChar :: Parser Char
pAChar = pLower <|> pUpper <|> pDigit <|> pAnySym "._\"-@"

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

pSpaces' ∷ Parser String
pSpaces' = pList (pSym ' ' <|> pSym '\t')

pUnOp ∷ Parser (Operand → GAS)
pUnOp =   const Sete <$> pToken "sete"
      <|> Bswap <$> pToken "bswap" **> pType

pCallString ∷ Parser (String,Maybe Operand)
pCallString =  (\a b → (a,Just (Seg "P" b))) <$> pString <*> pSym '$' **> pSym '{' **> pSNumeral <* pSym ':' **> pSym 'P' **> pSym '}'
           <|> (\a → (a,Nothing))  <$> pString 
           <|> (\b → ("",Just (Seg "P" b))) <$> pSym '$' **> pSym '{' **> pSNumeral <* pSym ':' **> pSym 'P' **> pSym '}'

pBinOp ∷ Parser (Operand → Operand → GAS)
pBinOp =  Add <$> pToken "add" **> pType
      <|> Mov <$> pToken "mov" **> pType
      <|> Sub <$> pToken "sub" **> pType
      <|> Cmpxchg <$> pToken "cmpxchg" **> pType
      <|> Xchg <$> pToken "xchg" **> pType
      <|> Xadd <$> pToken "xadd" **> pType
      <|> Bt   <$> pToken "bt" **> pType
      <|> Sbb   <$> pToken "sbb" **> pType
 --     <|> Cmp   <$> pToken "cmp" **> pType

pOperand ∷ Parser Operand
pOperand =  Lit <$> pSym '$' **> pSym '$' **> pSNumeral
        <|> Reg <$> pSym '$' **> pList1 pChar
        <|> CReg <$> pSym '%' **> pList1 pChar

pCmd ∷ Parser GAS
pCmd =  (\op b c → op b c) <$> pBinOp <*> pSpaces **> pOperand <*> pSym ',' **> pSpaces **> pOperand
	<|> (\op a → op a)     <$> pUnOp <*> pSpaces **> pOperand
	<|> (\τ α → Add τ (Lit (-1)) α) <$> pToken "dec" **> pType <*> pSpaces **> pOperand
	<|> (\τ α → Add τ (Lit 1) α)    <$> pToken "inc" **> pType <*> pSpaces **> pOperand
 -- <|> (uncurry Call)  <$> pToken "call"  **> pSpaces **> pCallString
  <|> const Lock <$> pToken "lock"
  <|> const Cli <$> pToken "cli"
  <|> const Sti <$> pToken "sti"


pGAS ∷ Parser GAS
pGAS =  pSpaces **> pCmd <* pSpaces' **> (pSym ';' <|> (pSym '\n' **> pSym '\t'))

pSec ∷ Parser (Maybe String)
pSec = (\a b → Just a) <$> pList1 pChar <*> pToken ":\n\t"

pSection ∷ Parser (Maybe String, [GAS])
pSection =  (,) <$> (pSec `opt` Nothing) <*> pList1 pGAS

pDir ∷ Parser Directive 
pDir =  Pushsection <$> pToken ".pushsection" **> pSpaces **> pDirPushSection
    <|> Balign      <$> pToken ".balign" **> pSpaces **> pSNumeral
    <|> Long        <$> pToken ".long" **> pSpaces **> (pList1 (pAChar <|> pSym ' '))
    <|> Word        <$> pToken ".word" **> pSpaces **> pSym '(' **> pWordByte <* pSym ')'
    <|> Byte        <$> pToken ".byte" **> pSpaces **> pWordByte
    <|> const Popsection  <$> pToken ".popsection"

pWordByte ∷ Parser String
pWordByte = pList1 (pAChar <|> pAnySym "*+/-" <|> pSym ' ')

pDirPushSection ∷ Parser PushDirective
pDirPushSection =  Altinstructions <$> pToken ".altinstructions" **> pSpaces **> pSym ',' **> pSpaces **> pString
               <|> AltinstrReplacement <$> pToken ".altinstr_replacement" **> pSpaces **> pSym ',' **> pSpaces **> pString
               <|> Discard <$> pToken ".discard" **> pSpaces **> pSym ',' **> pSpaces **> pString <*> pSpaces **> pSym ',' **> pSpaces **> pString
               <|> Smplocks <$> pToken ".smp_locks" **> pSpaces **> pSym ',' **> pSpaces **> pString

pDirective ∷ Parser Directive
pDirective = pDir <* pSpaces

pAsm ∷ Parser Asm
pAsm = (,) <$> pList pDirective <*> pList pSection

parseAsm ∷ String → Maybe Asm
parseAsm "" = Just ([],[])
parseAsm "1:\tud2\n.pushsection __bug_table,\"a\"\n2:\t.long 1b - 2b, ${0:c} - 2b\n\t.word ${1:c}, 0\n\t.org 2b+${2:c}\n.popsection" = 
  Just ([],[(Nothing,[Bug])])
parseAsm "movq %gs:${1:P},$0" = Just ([],[(Nothing,[Canary (I 64) (Reg "1")])])
parseAsm s | last s ≡ ';' = parseAsm' s
           | otherwise    = parseAsm' (s ⧺ ";")

parseAsm' ∷ String → Maybe Asm
parseAsm' s | (a,b) ← execParser pAsm s =
  if null b
  then Just a
  else trace ("error parseAsm " ⧺ s) $ Nothing

type AsmCs = [AsmC]

data AsmC = IC GasC | OC GasC | FC String
  deriving (Eq,Ord,Show)

data GasC = PosC Int
          | MemC
          | RegC
          | IRegC
          | CRegC String
          | ImmC
  deriving (Eq,Ord,Show)

parseAsmC ∷ String → Maybe AsmCs
parseAsmC s | (a,b) ← execParser (pComma `pListSep` pAsmC) s =
  if null b
  then Just a
  else trace ("error parseAsmC " ⧺ s) $ Nothing

pAsmC ∷ Parser AsmC
pAsmC =  OC <$> pSym '=' **> pGasC
     <|> IC <$> pGasC
     <|> FC <$> (pToken "~{memory}"
             <|> pToken "~{dirflag}"
             <|> pToken "~{fpsr}"
             <|> pToken "~{flags}"
             <|> pToken "~{cc}"
             <|> pToken "~{rcx}"
             <|> pToken "~{r8}"
             <|> pToken "~{r9}"
             <|> pToken "~{r10}"
             <|> pToken "~{r11}"
             )

pGasC ∷ Parser GasC
pGasC =  const (CRegC "eax") <$> pToken "{ax}"
     <|> const (CRegC "edx") <$> (pToken "{edx}" <|> pToken "{dx}")
     <|> const (CRegC "edi") <$> pToken "{di}"
     <|> const (CRegC "esi") <$> pToken "{si}"
     <|> const MemC <$> (pToken "*m" <|> pToken "*qm")
     <|> const RegC <$> (pToken "r" <|> pToken "&r" <|> pToken "rm")
     <|> const IRegC <$> (pToken "ir" <|> pToken "Ir")
     <|> const ImmC <$> (pToken "im" <|> pToken "imr" <|> pToken "i")
     <|> (PosC . digit2Num)  <$> pDigit

