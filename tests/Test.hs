module Main where
import Prelude hiding(and, or)
import Control.Monad(forM_)

import LLVM.Core
import qualified LLVM.FFI.Core as FFI


-- Our module will have these two functions.

main :: IO ()
main = do mod <- readBitcodeFromFile "mymain.bc"
	  f <- getFunctions mod
	  v <- getGlobalVariables mod
	--  print "Functions"
	  forM_ f (\(x,y) -> printFunction x y)
	--  print "Global Variables"
	--  print v

printFunction :: String -> FFI.ValueRef -> IO ()
printFunction s v = do r <- getBasicBlocks v
		       print s
		       forM_ r (\(x,y) -> printInstructions x y)

printInstructions :: String -> FFI.ValueRef -> IO ()
printInstructions s v = do r <- getInstructions v
		          -- print s
		 	   forM_ r (\(x,y) -> printOperands x y) 

printOperands :: String -> FFI.ValueRef -> IO ()
printOperands s v = do r <- getOperands v
		       e <- getInstrDesc v
		       print e
		       print r
		       print "-----"
		   --    print s
		   --    print r
		       --forM_ r (\(x,y) -> printOperands x y)
