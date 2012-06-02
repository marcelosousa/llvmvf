module Mutation.Core where

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.Target as FFI

import LLVM.Core hiding (Value) 
import LLVM.Util.Optimize 

opt2 :: FilePath -> IO ()
opt2 file = do mdl <- readBitcodeFromFile file
               b   <- optimizeModule 2 mdl
               if b then do writeBitcodeToFile "output.bc" mdl
                            print "Success"
                    else print "Fail"

mutate :: FilePath -> IO ()
mutate bc = opt2 bc -- do mdl <- readBitcodeFromFile "simple.bc"

          
--                pass <- createFunctionPassManager mdlp
