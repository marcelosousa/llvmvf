module Language.SystemC.Architecture where

import Language.LLVMIR

data SCArch = SCArch SCMods SCComm
  deriving Show

type SCMods = [SCMod]
type SCComm = [SCCommElem]
type SCModVars = [SCModVar]

data SCMod = SCMod SCModName SCConstrName SCInstName SCModVars SCProcs
  deriving Show

type SCModName = String
type SCConstrName = String
type SCInstName = String

data SCCommElem = SCSignal String Type
  deriving Show

data SCModVar = SCPortIn
              | SCPortOut
              | SCEvent
              | SCTy Type
  deriving Show

type SCProcs = [SCProc]

data SCProc = SCMethod String 
            | SCThread String
  deriving Show