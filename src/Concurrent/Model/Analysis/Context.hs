{-#LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Concurrent.Model.Analysis.Context
-- Copyright :  (c) 2013 Marcelo Sousa
-------------------------------------------------------------------------------

module Concurrent.Model.Analysis.Context where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad.State

import qualified Data.Map as M

import Language.LLVMIR
import Concurrent.Model.Analysis.ControlFlow
import Concurrent.Model.Analysis.DataFlow

data Location = Location 
  { fn  :: Identifier
  , bb  :: Identifier
  , lpc :: PC
  , ise :: Bool
  }

type Locations = [Location]

data Where = BBLoc Identifier
           | FnLoc Identifier
           | ThLoc Identifier
           | EndFn 
           | EndTh Identifier 

data Loc = ExitLoc Location Where
         | SyncLoc Location Identifier
             
isLocEndTh :: Loc -> Bool
isLocEndTh (ExitLoc l w) = isWhEndTh w
isLocEndTh (SyncLoc l i) = False

isWhEndTh :: Where -> Bool
isWhEndTh (EndTh _) = True
isWhEndTh _         = False

isLocEndFn :: Loc -> Bool
isLocEndFn (ExitLoc l w) = isWhEndFn w
isLocEndFn (SyncLoc l i) = False

isWhEndFn :: Where -> Bool
isWhEndFn EndFn = True
isWhEndFn _     = False

pcLoc :: Loc -> PC
pcLoc (ExitLoc l@Location{..} _) = lpc
pcLoc (SyncLoc l@Location{..} _) = lpc
                             
type LocList = [Loc]

data Core = Core
  { nmdtys :: NamedTypes
  , vars   :: Globals
  , funs   :: Functions
  }

eCore :: Core
eCore = Core M.empty [] M.empty

type Locs = M.Map Identifier (M.Map Identifier LocList)

type Seen = M.Map Identifier [Identifier]

data Env = Env
  { corein  :: Core
  , coreout :: Core
  , ccfg    :: ControlFlow
  , df      :: DataFlow
  , ploc    :: Location
  , efloc   :: Locs
  , seen    :: Seen
  }

updateLocs :: Location -> Loc -> Locs -> Locs
updateLocs l@Location{..} loc locs = 
    M.alter f fn locs where
        f Nothing = Just $ M.singleton bb [loc]
        f (Just m) = Just $ M.alter g bb m
        g Nothing = Just [loc]
        g (Just l) = Just $ loc:l
        
getThreadExits :: Identifier -> Locs -> [PC]
getThreadExits i m = case M.lookup i m of
    Nothing -> error "getThreadExits:" -- ++ show i ++ " " ++ show m
    Just bb -> let bbi = concat $ M.elems bb
                   bbi' = filter isLocEndTh bbi
               in map pcLoc bbi'

getFunctionExits :: Identifier -> Locs -> [PC]
getFunctionExits i m = case M.lookup i m of
    Nothing -> error "getFunctionExits:" -- ++ show i ++ " " ++ show m
    Just bb -> let bbi = concat $ M.elems bb
                   bbi' = filter isLocEndFn bbi
               in map pcLoc bbi'
               
newtype Context a = Context { unContext :: State Env a }
    deriving (Monad, Functor, Applicative)

evalContext :: Context a -> Env -> Env
evalContext = execState . unContext

getEnv :: Context Env
getEnv = Context get

putEnv :: Env -> Context ()
putEnv = Context . put