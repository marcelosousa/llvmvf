-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Util
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Util where

import Language.LLVMIR

import Data.Maybe
import Data.List
import UU.PPrint

nextUnique :: Int -> (Int, Int)
nextUnique u = (u+1, u)

emptyFunction :: Function
emptyFunction = FunctionDef (Global "undefined") ExternalLinkage TyVoid False [] []

valueIdentifier :: Value -> Maybe Identifier
valueIdentifier (Id i _) = Just i
valueIdentifier (Constant (GlobalValue (GlobalVariable i _))) = Just i
valueIdentifier (Constant (GlobalValue (FunctionValue i _))) = Just i
valueIdentifier _ = Nothing

valueIdentifier' :: String -> Value -> Identifier
valueIdentifier' e v = 
  case valueIdentifier v of
    Nothing -> error $ e ++ ":valueIdentifier " ++ show v
    Just i  -> i

identifierName :: Identifier -> String
identifierName (Global n) = n
identifierName (Local  n) = n

identifierValue :: Value -> String
identifierValue v = identifierName $ valueIdentifier' "" v

findBasicBlock :: Identifier -> Function -> BasicBlock
findBasicBlock i (FunctionDecl _ _ _ _ _) = error $ "findBasicBlock: " ++ show i ++ " not found."
findBasicBlock i (FunctionDef  _ _ _ _ _ body) =
    fromMaybe (error $ "findBasicBlock: " ++ show i ++ " not found.") $
    find (isBasicBlock i) body

isBasicBlock :: Identifier -> BasicBlock -> Bool
isBasicBlock i (BasicBlock j _) = i==j

fnBasicBlockIds :: Function -> [Identifier]
fnBasicBlockIds (FunctionDecl _ _ _ _ _) = []
fnBasicBlockIds (FunctionDef  _ _ _ _ _ body) = map getBBId body

getBBId :: BasicBlock -> Identifier
getBBId (BasicBlock i _) = i

getModFns :: Module -> Functions
getModFns (Module id layout target gvars funs nmdtys) = funs

infoValue :: Value -> Either Identifier (Identifier, [Int])
infoValue v = case v of
  Constant (ConstantExpr (GetElementPtrConstantExpr i idxs)) ->
    let vi = valueIdentifier' "infovalue gep" i
        is = map intConstantValue idxs
    in Right $ (vi,is) 
  _ -> Left $ valueIdentifier' "infovalue" v

intConstantValue :: Value -> Int
intConstantValue (Constant (SmpConst (ConstantInt v _))) = v
intConstantValue sc = error $ "intConstantValue: " ++ show sc

instance Pretty Identifier where
  pretty (Local i) = text i
  pretty (Global i) = text i

