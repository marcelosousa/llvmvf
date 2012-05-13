-------------------------------------------------------------------------------
-- Module    :  Language.LLVMIR.Parser
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LLVMIR.Parser where

import Control.Monad(forM_)
import System.FilePath

import Language.LLVMIR.Base (LLModule)

import LLVM.Core hiding (Value)
import LLVM.ExecutionEngine

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.Target as FFI

import Foreign.C.String

--parse :: FilePath -> LLModule
parse :: FilePath -> IO ()
parse file = do mdl <- readBitcodeFromFile file                         
                parseModule mdl
                printDataLayoutModule mdl
                trg <- getDataLayoutModule mdl
                print $ littleEndian trg 
 
    
type FunctionName = String
type Value = FFI.ValueRef

printDataLayoutModule :: Module -> IO ()
printDataLayoutModule mdl = withModule mdl $ \mdlPtr -> do 
                            cs <- FFI.getDataLayout mdlPtr
                            s <- peekCString cs
                            print s
                            
getDataLayoutModule :: Module -> IO TargetData
getDataLayoutModule mdl = withModule mdl $ \mdlPtr -> do 
                            cs <- FFI.getDataLayout mdlPtr
                            trgPtr <- FFI.createTargetData cs
                            return $ makeTargetData trgPtr
                            
parseModule :: Module -> IO ()
parseModule irmod = do f <- getFunctions irmod
                       forM_ f (uncurry parseFunction)
                       g <- getGlobalVariables irmod
                       forM_ g (\(x,_) -> print x)

parseFunction :: FunctionName -> Value -> IO ()
parseFunction s f = do p <- getParams f
                       print s
                       forM_ p (\(x,_) -> print x) 
              
