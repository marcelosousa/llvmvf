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
import qualified Data.Maybe as MB
import Data.List 

import Language.LLVMIR
import Language.LLVMIR.Util
import Concurrent.Model.Analysis.ControlFlow
import Concurrent.Model.Analysis.DataFlow

data Location = Location 
  { fn  :: Identifier
  , bb  :: Identifier
  , lpc :: PC
  , ise :: Bool
  }
  deriving Show

type Locations = [Location]

data Where = BBLoc Identifier
           | FnLoc Identifier
           | ThLoc Identifier
           | EndFn 
           | EndTh Identifier 
  deriving Show

data Loc = ExitLoc Location Where
         | SyncLoc Location Identifier
  deriving Show
             
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
type Threads = M.Map Identifier [(Identifier,Identifier)]

data Env = Env
  { corein  :: Core
  , coreout :: Core
  , ccfg    :: ControlFlow
  , df      :: DataFlow
  , ploc    :: Location
  , efloc   :: Locs
  , seen    :: Seen
  , threads :: Threads
  }


findThread :: Identifier -> Identifier -> DataFlow -> Threads -> Identifier
findThread fn reg df@DataFlow{..} threads = 
  let fnLoadMap = MB.fromMaybe (error "fnLoadMap in findThread") $ M.lookup fn loadMap
      fnThreads = MB.fromMaybe (error $ "fnThreads in findThread " ++ show fn ++ " " ++ show threads) $ M.lookup fn threads
  -- It can be that reg is the actual identifier
  in case findInThreads reg fnThreads of
      Just t -> t
      Nothing -> 
        -- Check if it was the register that reg loaded from
        case M.lookup reg fnLoadMap of
          Nothing -> error $ "findThread failed " ++ show fn ++ " " ++ show reg
          Just reg' -> case findInThreads reg' fnThreads of
            Nothing -> error $ "findThread failed"
            Just t -> t

findInThreads :: Identifier -> [(Identifier, Identifier)] -> Maybe Identifier
findInThreads i l = snd <$> find (\x -> fst x == i) l

getLocs :: Location -> Locs -> LocList
getLocs l@Location{..} m =
    case M.lookup fn m of
        Nothing -> error $ "getLocs: FN: " ++ show fn ++ " " ++ show m
        Just fnt -> case M.lookup bb fnt of
            Nothing -> error "getLocs: BB"
            Just bbt -> bbt

fnWasAnalyzed :: Identifier -> Core -> Seen -> Bool
fnWasAnalyzed i c@Core{..} seen =
    let ms = M.lookup i seen
    in case M.lookup i funs of
        Nothing -> error "fnWasAnalyzed"
        Just fn -> case ms of
            Nothing -> False
            Just bbsids -> 
                let bbids = fnBasicBlockIds fn
                in bbsids == bbids

bbWasAnalyzed :: Identifier -> Identifier -> Seen -> Bool
bbWasAnalyzed fni bbi seen =
    case M.lookup fni seen of
        Nothing -> False
        Just fn -> bbi `elem` fn
            
addToSeen :: Identifier -> Identifier -> Seen -> Seen
addToSeen fni bbi seen = 
    M.alter f fni seen where
        f Nothing = Just [bbi]
        f (Just n) = Just $ bbi:n
        
updateLocs :: Location -> Loc -> Locs -> Locs
updateLocs l@Location{..} loc locs = 
    M.alter f fn locs where
        f Nothing = Just $ M.singleton bb [loc]
        f (Just m) = Just $ M.alter g bb m
        g Nothing = Just [loc]
        g (Just l) = Just $ loc:l

updateThreads :: Identifier -> (Identifier, Identifier) -> Threads -> Threads
updateThreads fn ti t = 
    M.alter f fn t where
        f Nothing = Just [ti]
        f (Just m) = Just $ ti:m
        
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
