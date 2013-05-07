-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Util
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Util where

import Language.LLVMIR
import Data.Maybe
import Data.List

nextUnique :: Int -> (Int, Int)
nextUnique u = (u+1, u)

emptyFunction :: Function
emptyFunction = FunctionDef (Global "undefined") ExternalLinkage TyVoid False [] []

valueIdentifier :: Value -> Maybe Identifier
valueIdentifier (Id i _) = Just i
valueIdentifier (Constant (GlobalValue (GlobalVariable i _))) = Just i
valueIdentifier _ = Nothing

valueIdentifier' :: String -> Value -> Identifier
valueIdentifier' e v = 
  case valueIdentifier v of
    Nothing -> error $ e ++ ":valueIdentifier " ++ show v
    Just i  -> i

identifierName :: Identifier -> String
identifierName (Global n) = n
identifierName (Local  n) = n

findBasicBlock :: Identifier -> Function -> BasicBlock
findBasicBlock i (FunctionDecl _ _ _ _ _) = error $ "findBasicBlock: " ++ show i ++ " not found."
findBasicBlock i (FunctionDef  _ _ _ _ _ body) =
    fromMaybe (error $ "findBasicBlock: " ++ show i ++ " not found.") $
    find (isBasicBlock i) body

isBasicBlock :: Identifier -> BasicBlock -> Bool
isBasicBlock i (BasicBlock j _) = i==j