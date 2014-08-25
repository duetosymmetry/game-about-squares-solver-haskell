module GASsolver where

import Data.List
import Data.Hashable
import qualified Data.Map as Map

----------------------------------------------------------------------
-- TYPES
----------------------------------------------------------------------

-- I can't use Left and Right because those are for Either
data Direction = RightD | Up | LeftD | Down
     deriving (Show, Enum, Eq, Ord)

data Position = Position !Int !Int
     deriving (Show, Eq, Ord)

-- I am being lazy here, instead of using a better color
-- type like Data.Colour
newtype Color = Color String
        deriving (Show, Eq, Ord)

data Square = Square !Color !Position !Direction
     deriving (Show, Eq, Ord)

-- Do I really need to wrap up [Square] into State?
newtype State = State [Square]
        deriving (Show, Eq, Ord)

data Circle = Circle !Color !Position
     deriving (Show)

data Arrow = Arrow !Position !Direction
     deriving (Show)

-- The arguments to Box are Box xmin xmax ymin ymax.
-- These x,y values are inclusive, i.e. something is inside
-- the box if xmin <= x <= xmax and similar for y.
data Box = Box !Int !Int !Int !Int
     deriving (Show)

data Board = Board ![Circle] ![Arrow] !Box
     deriving (Show)

instance Hashable Color where
  hashWithSalt s (Color string) = hashWithSalt s string
-- I need to implement a different hash for Square because
-- the default hash actually had some collisions!!!!!!!!!!
-- I just add some magic numbers in such a way that I've found
-- that I no longer have collisions.
instance Hashable Square where
  hashWithSalt s (Square (Color c) (Position x y) d) =
    hashWithSalt s ((x*0x1000 + y)*0x1000+(fromEnum d), c)

instance Hashable State where
  hashWithSalt s (State sqs) = hashWithSalt s sqs

----------------------------------------------------------------------
-- Basic game stuff
----------------------------------------------------------------------

-- Is this circle satisfied by this square?
isCircleSquared (Circle cirCol cirPos) (Square sqrCol sqrPos _) =
                cirCol==sqrCol && cirPos==sqrPos

-- Is a board solved by a state?
isSolved (Board circs _ _) (State sqs) =
         all isSatisfied circs
         where isSatisfied c = any (isCircleSquared c) sqs

-- Gives the next position of some direction
nextDirPos RightD (Position x y) = Position (x+1)  y
nextDirPos Up     (Position x y) = Position  x    (y-1)
nextDirPos LeftD  (Position x y) = Position (x-1)  y
nextDirPos Down   (Position x y) = Position  x    (y+1)

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
pushedState (Board _ arrs _) (State sqs) i =
  State $ sort $ setArrows arrs (unpushed ++ doPush d pushedSqs)
    where pusher@(Square _ _ d) = sqs !! i
          pushedSqs             = pushed d [pusher] sqs
          unpushed              = sqs \\ pushedSqs

----------------------------------------------------------------------
-- Bounding box filter
----------------------------------------------------------------------
emptyBox = Box 0 0 0 0

computeBoardBox (Board circs arrs _) =
  Box xmin xmax ymin ymax
  where circxs = map (\(Circle _ (Position x y)) -> x) circs
        circys = map (\(Circle _ (Position x y)) -> y) circs
        arrxs = map (\(Arrow (Position x y) _) -> x) arrs
        arrys = map (\(Arrow (Position x y) _) -> y) arrs
        xs = circxs ++ arrxs
        ys = circys ++ arrys
        xmin = minimum xs
        xmax = maximum xs
        ymin = minimum ys
        ymax = maximum ys

inBox (Box xmin xmax ymin ymax) (Position x y) =
  (xmin <= x) && (x <= xmax) && (ymin <= y) && (y <= ymax)

padBox (Box xmin xmax ymin ymax) pad =
  Box (xmin-pad) (xmax+pad) (ymin-pad) (ymax+pad)

setBoardPadBox b@(Board circs arrs _) =
  Board circs arrs . padBox (computeBoardBox b)

allSquaresInBoardBox (Board _ _ box) (State sqs) =
  all squareInBox sqs
  where squareInBox (Square _ pos _) = inBox box pos

----------------------------------------------------------------------
-- Possibilities for solving
----------------------------------------------------------------------

-- All the possible resulting states from an individual state, along with
-- which color was pushed and the hash of the state whence they came
possibilities board state@(State sqs) =
  [(pushedStateBoard, colorOf $ sqs !! i , Just $ hash state)
    | i <- [0..(length sqs - 1)],
      let pushedStateBoard = pushedState board state i,
        allSquaresInBoardBox board pushedStateBoard ]
    where colorOf (Square (Color c) _ _) = c

-- This is the set of *unique* possibilities resulting from a list states
levelPossibilities board states =
  rmdups $ concatMap (possibilities board) states
    where rmdups = map head . groupBy sameState . sort
          sameState (s1, _, _) (s2, _, _) = s1==s2

-- This is the set of unique possibilities which are not in the state-map
unexploredPossibilities smap board states =
  filter notIn $ levelPossibilities board states
    where notIn (state, _, _) = Map.notMember (hash state) smap

----------------------------------------------------------------------
-- For printing
----------------------------------------------------------------------

-- get the order of moves that went to a certain one
moveSequence smap [] = Nothing
moveSequence smap moves@((_, _, Nothing):rest) = Just moves
moveSequence smap moves@((_, _, Just priorHash):rest) =
  case Map.lookup priorHash smap of
    Nothing        -> Nothing
    Just priorMove -> moveSequence smap (priorMove:moves)

moveListStr (Just moves) =
  flip (++) "\n" . intercalate "\n" . map (\((State s, c, _),y) -> show y ++ ". " ++ c ++ " " ++ show s) . zip moves $ [0..]
moveListStr Nothing      = "\n"

moveColorSequence = map (\(_, color, _) -> color)

moveListStr2 (Just moves) =
  flip (++) "\n" . intercalate "\n" . map (\(x,y) -> show y ++ ". " ++ x) . zip (moveColorSequence moves) $ [0..]
moveListStr2 Nothing      = "\n"

moveListStrHelper [(color,number)]        = show number ++ ". " ++ color
moveListStrHelper moves@((color,first):_) = show first ++ "-" ++ show (first+num-1) ++ ". "
                                               ++ color ++ " (" ++ show num ++ "x)"
                                             where num = length moves

moveListStr3 (Just moves) =
  flip (++) "\n" . intercalate "\n" . map moveListStrHelper . groupBy (\x y -> fst x == fst y) . zip (moveColorSequence moves) $ [0..]
moveListStr3 Nothing = "\n"

----------------------------------------------------------------------
-- For the hash Map
----------------------------------------------------------------------
-- insert one state-triple into smap
insertOne striple@(state, _, _) = Map.insert (hash state) striple

-- insert a list of state-triples
insertMany striples smap = foldl (flip insertOne) smap striples

----------------------------------------------------------------------
-- The depth-first search driver
----------------------------------------------------------------------

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

----------------------------------------------------------------------
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
                              " states\n" ++ moveListStr3 (Just moves)
    where (smap, result) = trySolveLevel board initState maxDepth
