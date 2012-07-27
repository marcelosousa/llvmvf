{-# LANGUAGE GADTs, FlexibleInstances, RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Language.LTL.Base
-- Copyright :  (c) 2012 Marcelo Sousa
-------------------------------------------------------------------------------

module Language.LTL.Base where

import Data.Set hiding (size, map)
import Prelude  hiding ((^))

-- | Property P
data P = VP String -- ^ Variables V 
       | VT        -- ^ true
       | VF        -- ^ false
  deriving (Show,Eq,Ord,Read)

data G f 
data F f
data X f
data U f g
data R f g
data Or g h
data And g h
data Not g

type Imp   f g = Or (Not f) g 
type Equiv f g = And (Imp f g) (Imp g f) 

data LTL f where
  V  :: P     -> LTL P
  F  :: LTL g -> LTL (F g)
  G  :: LTL g -> LTL (G g) 
  X  :: LTL g -> LTL (X g)
  U  :: LTL g -> LTL h -> LTL (U g h)
  R  :: LTL g -> LTL h -> LTL (R g h)
  Or :: LTL g -> LTL h -> LTL (Or g h)  
  Not :: LTL g -> LTL (Not g)
  And :: LTL g -> LTL h -> LTL (And g h)

(~>) :: LTL f -> LTL g -> LTL (Imp f g)
f ~> g = Or (Not f) g

(<->) :: LTL f -> LTL g -> LTL (Equiv f g)
f <-> g = And (f ~> g) (g ~> f)

v :: String -> LTL P
v = V . VP

-- a -> X b
pEx :: LTL (Imp P (X P))
pEx = v "a" ~> X (v "b")

-- example of satefy property - G not (a /\ b)
safePEx :: LTL (G (Not (And P P)))
safePEx = G $ Not $ And (v "a") (v "b")

-- liveness property 
livePEx :: LTL (G (Imp P (F P)))
livePEx = G $ (v "a") ~> F (v "b")

-- | State s - Set of V
data State = State (Set P)
  deriving (Show, Eq, Ord, Read) 

type States = Set State

type LabelFn = State -> Set P
type Transition = State -> State -- could also be Set (State, State)

-- | Kripke Structure K = (S,I,T,L)
data Model = Model { states     :: States     -- ^ S
                   , istates    :: States     -- ^ I - Non empty set
                   , transition :: Transition -- ^ T - T is a total function
                   , labelfun   :: LabelFn    -- ^ L
                   }
-- Models isomorphic to the set of interpretation of the
-- boolean variables V are considered.
-- forall s = V' subset V:
-- (1)  S = IP(V)   -- ^ The set of states is the power set of V
-- (2)  L(V') = V'  -- ^ L is t

type KripkeS = Model
 
-- | A Computation s of a model m 
data Computation s m where
  End     :: m      -> Computation () m
  Compute :: s -> m -> Computation s  m 

getState :: Computation t t1 -> t 
getState (Compute s m) = s 

getModel :: Computation State Model -> Model 
getModel (Compute s m) = m

-- A proposition variable p is true in a system state s 
-- iff p belongs to L(s)
evalP :: Computation State Model -> P -> Bool
evalP s p = let st = getState s
                m@Model{..} = getModel s
            in p `member` labelfun st

-- Path pi = (s0, s1, s2, ...)
data Path = Path { unPath :: [State] }
  deriving (Show, Eq, Ord, Read)

-- | Is path initialized - pi(0) = s0
isPathInitialized :: Path -> State -> Bool 
isPathInitialized (Path [])     s = False
isPathInitialized (Path (x:xs)) s = x == s

-- | pi(i)
(!~!) :: Path -> Int -> State
(Path p) !~! i = if i >= length p
                 then error "'pi(i)': Invalid index"
                 else p!!i

-- | suffix i of a path pi - pi^(i)
(^) :: Path -> Int -> Path
(Path p) ^ i = if i >= length p
               then error "'pi^(i)': Undefined subpath" 
               else Path $ drop i p

-- | size of a Path
size :: Path -> Int
size (Path p) = length p

-- | Semantics m, pi |- f
(|-) :: Model -> Path -> LTL f -> Bool
(|-) m pi (V p)     = let s = pi !~! 0    -- pi(0)
                          c = Compute s m -- 
                      in  evalP c p
(|-) m pi (Not p)   = not $ (|-) m pi p 
(|-) m pi (Or  g h) = (|-) m pi g || (|-) m pi h 
(|-) m pi (And g h) = (|-) m pi g && (|-) m pi h
(|-) m pi (F g)     = let js = [0..(size pi - 1)] 
                      in  or $ map (\j -> (|-) m (pi^j) g) js
(|-) m pi (G g)     = let js = [0..(size pi - 1)] 
                      in  and $ map (\j -> (|-) m (pi^j) g) js
(|-) m pi (X g)     = (|-) m (pi^1) g
(|-) m pi (U g h)   = let js = [0..(size pi - 1)]
                      in  or $ map (\j -> if (|-) m (pi^j) h
                                          then let ns = [0..(j-1)]
                                               in  and $ map (\n -> (|-) m (pi^n) g) ns
                                          else False) js 
(|-) m pi (R g h)   = let js = [0..(size pi - 1)]
                      in  and $ map (\j -> let ns = [0..(j-1)]
                                               r2 = or $ map (\n -> (|-) m (pi^n) g) ns
                                           in  (|-) m (pi^j) h || r2) js

-- | Get initialized paths
iPi :: KripkeS -> [Path]
iPi m@Model{..} = map (\i -> Path $ exec transition i) (toList istates)

exec :: Transition -> State -> [State]
exec f s = let s' = f s
           in  s':exec f s'

-- The LTL formula f holds on a Kripke structure K: K |= f
(|=) :: KripkeS -> LTL f -> Bool
k |= phi = and $ map (\pi -> (|-) k pi phi) $ iPi k 
