{-# LANGUAGE UnicodeSyntax, FlexibleContexts #-}
-------------------------------------------------------------------------------
-- Module    :  Language.Asm
-- Copyright :  (c) 2013 Marcelo Sousa
-- Inline Asm 
-------------------------------------------------------------------------------

module Language.Asm where

import Text.ParserCombinators.UU hiding (parse)
import Text.ParserCombinators.UU.Utils (runParser)
import Text.ParserCombinators.UU.BasicInstances 
import Text.ParserCombinators.UU.Demo.Examples hiding (Parser)

pString ∷ Parser String
pString = pList1 (pSatisfy ((/=) ';') (Insertion "" ';' 0)) <* pSym ';'

f ∷ Parser [String]
f = pList pString -- pListSep (pSym ';') (pString <|> (pReturn ""))

parseAsm = runParser "Error Asm" f
