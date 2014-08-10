{-# LANGUAGE DeriveGeneric #-}

module GASsolver where

import GHC.Generics (Generic)
import Data.List
import Data.Hashable
import qualified Data.Map as Map

----------------------------------------------------------------------
-- TYPES
----------------------------------------------------------------------

-- In the previous version I used an enumerated type like:
-- data Direction = RightD | Up | LeftD | Down
-- But now I am trying it with a pair of Int to see
-- if that speeds things up.
data Direction = Direction !Int !Int
     deriving (Show, Eq, Ord, Generic)

(right, up, left, down) = (Direction   1   0,
                           Direction   0 (-1),
                           Direction (-1)  0,
                           Direction   0   1)

data Position = Position !Int !Int
     deriving (Show, Eq, Ord, Generic)

-- I am being lazy here, instead of using a better color
-- type like Data.Colour
newtype Color = Color String
        deriving (Show, Eq, Ord, Generic)

data Square = Square Color !Position !Direction
     deriving (Show, Eq, Ord, Generic)

-- Do I really need to wrap up [Square] into State?
newtype State = State [Square]
        deriving (Show, Eq, Ord, Generic)

data Circle = Circle Color Position
     deriving (Show)

data Arrow = Arrow Position Direction
     deriving (Show)

data Board = Board [Circle] [Arrow]
     deriving (Show)

instance Hashable Direction
instance Hashable Position
instance Hashable Color
-- I need to implement a different hash for Square because
-- the default hash actually had some collisions!!!!!!!!!!
-- I just add some magic numbers in such a way that I've found
-- that I no longer have collisions.
instance Hashable Square where
  hashWithSalt s (Square (Color c) (Position x y) (Direction dx dy)) =
    hashWithSalt s (c, 100+x, 200+y, 300+dx, 400+dy)

instance Hashable State

----------------------------------------------------------------------
-- Basic game stuff
----------------------------------------------------------------------

-- Is this circle satisfied by this square?
isCircleSquared (Circle cirCol cirPos) (Square sqrCol sqrPos _) =
                cirCol==sqrCol && cirPos==sqrPos

-- Is a board solved by a state?
isSolved (Board circs _ ) (State sqs) =
         all isSatisfied circs
         where isSatisfied c = any (isCircleSquared c) sqs

-- Gives the next position of some direction
nextDirPos (Direction dx dy) (Position x y) = Position (x+dx) (y+dy)

-- Test if a square is at a certain position
isInPosition p0 (Square _ p1 _) = p0 == p1

-- Find the next pushed square in a certain direction
nextPushed d (front:_) =
  find (isInPosition $ nextDirPos d posFront)
    where (Square _ posFront _) = front

-- Find all the squares pushed in a certain direction
pushed d pushing testForPush =
  case nextPushed d pushing testForPush of
    Just next -> pushed d (next:pushing) testForPush
    Nothing   -> pushing

-- do the push on a list of pushed squares
doPush d = map doPush1
  where doPush1 (Square c p d') = Square c (nextDirPos d p) d'

-- set the direction on a square if it's on top of this arrow
setArrow (Arrow arrPos d) = map setArrow1
  where setArrow1 s@(Square c p d') | p==arrPos   = Square c p d
                                    | otherwise   = s

-- From a list of arrows, set correct all the squares' directions
setArrows arrows sqs = foldl (flip setArrow) sqs arrows

-- Get the pushed state when the ith square is pushed
pushedState (Board _ arrs) (State sqs) i =
  State $ sort $ setArrows arrs (unpushed ++ doPush d pushedSqs)
    where pusher@(Square _ _ d) = sqs !! i
          pushedSqs             = pushed d [pusher] sqs
          unpushed              = sqs \\ pushedSqs

----------------------------------------------------------------------
-- Approach to solving
----------------------------------------------------------------------

-- All the possible resulting states from an individual state, along with
-- which color was pushed and the hash of the state whence they came
possibilities board state@(State sqs) =
  [(pushedState board state i, colorOf $ sqs !! i , Just $ hash state)
    | i <- [0..(length sqs - 1)]]
    where colorOf (Square (Color c) _ _) = c

-- This is the set of *unique* possibilities resulting from a list states
levelPossibilities board states =
  nubBy sameState $ concatMap (possibilities board) states
    where sameState (s1, _, _) (s2, _, _) = s1==s2

-- Testing if I have hash collisions with distinct states
notIn' (state, _, _) smap =
  case Map.member (hash state) smap of
    False  -> True -- not in smap
    True -> case (state == lookupstate) of -- in smap
               True  -> False     -- in smap and same state
               False ->  error ("states:\n" ++ show state ++ "\n" ++ show lookupstate ++ "\nhashes: " ++ show (hash state) ++ " " ++ show (hash lookupstate))
             where Just (lookupstate, _, _) = Map.lookup (hash state) smap

-- This is the set of unique possibilities which are not in the state-map
unexploredPossibilities smap board states =
  filter notIn $ levelPossibilities board states
    where notIn stateTriple = notIn' stateTriple smap
--    where notIn (state, _, _) = Map.notMember (hash state) smap

-- get the order of moves that went to a certain one
moveSequence smap [] = Nothing
moveSequence smap moves@((_, _, Nothing):rest) = Just moves
moveSequence smap moves@((_, _, Just priorHash):rest) =
  case Map.lookup priorHash smap of
    Nothing        -> Nothing
    Just priorMove -> moveSequence smap (priorMove:moves)

-- For printing
moveListStrHelper i [] = "\n"
moveListStrHelper i ((State s, c, _):rest) = 
  show i ++ ". " ++ c ++ ", " ++ show s ++ "\n"
    ++ moveListStrHelper (i+1) rest

moveListStrHelper' i [] = "\n"
moveListStrHelper' i ((State s, c, _):rest) = 
  show i ++ ". " ++ c ++ "\n" ++ moveListStrHelper' (i+1) rest

moveListStr (Just moves) = moveListStrHelper 0 moves
moveListStr Nothing      = "\n"

moveListStr' (Just moves) = moveListStrHelper' 0 moves
moveListStr' Nothing      = "\n"

-- insert on state-triple into smap
insertOne striple@(state, _, _) = Map.insert (hash state) striple

-- insert a list of state-triples
insertMany striples smap = foldl (flip insertOne) smap striples

{- Let's say that we've explored to a depth of n moves,
   with the states stored in smap, and this past level's states
   in levelStateTriples. The plan is then to:
    1. Put them all into smap'
    2. Check if any of the levelStates solve the board
    3. If so, return  (smap', Right solvedStateTriple)
    4. If not, compute levelStateTriples'
    4a. And return (smap', Left levelStateTriples')
 -}
exploreLevel board smap levelStateTriples =
  case findIndex (isSolved board) levelStates of
    Nothing -> (smap', Left levelStateTriples')
    Just i  -> (smap', Right (levelStateTriples !! i))
    where levelStates        = map (\(s, _, _) -> s) levelStateTriples
          smap'              = insertMany levelStateTriples smap
          levelStateTriples' = unexploredPossibilities smap' board levelStates

-- explore to a certain depth, same returns as above
exploreDepth board 0 smap levelStateTriples = (smap, Left levelStateTriples)
exploreDepth board i smap levelStateTriples =
  let (smap', either) = exploreLevel board smap levelStateTriples in
    case either of
      Right solved             -> (smap', Right solved)
      Left  levelStateTriples' -> exploreDepth board (i-1) smap' levelStateTriples'

-- the main driver.
-- Upon success, returns (smap, Just moveSequence)
-- Upon failure, returns (smap, Nothing)
trySolveLevel board initState maxDepth =
  case result of
    Right solved            -> (smap, moveSequence smap [solved])
    Left  levelStateTriples -> (smap, Nothing)
    where striple0        = (initState, "start", Nothing)
          smap0           = Map.empty
          (smap, result) = exploreDepth board (maxDepth+1) smap0 [striple0]

-- variant of the main driver if you don't care about the smap
trySolveLevel' board initState maxDepth =
  case result of
    Nothing      -> putStr $ "Failed to solve in " ++
                              show maxDepth ++ " moves," ++
                              " explored " ++ show (Map.size smap) ++
                              " states\n"
    (Just moves) -> putStr $ "Solved in " ++
                              show (length moves - 1) ++ " moves,"++
                              " explored " ++ show (Map.size smap) ++
                              " states\n" ++ moveListStr' (Just moves)
    where (smap, result) = trySolveLevel board initState maxDepth
