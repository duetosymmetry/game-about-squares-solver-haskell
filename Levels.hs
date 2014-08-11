import System.Environment
import Data.List
import GASsolver

levelByName name = 
  case (lookup name levels) of
    Nothing -> Nothing
    Just (b@(Board circs arrs), State sqs) -> 
        Just (b, State (setArrows arrs sqs))

origLevelByNumber i = levelByName $ origLevels !! i

trySolveOrigLevelNum i =
  trySolveLevel' board initState 42
  where (Just (board, initState)) = origLevelByNumber i

main = do 
  (firstarg:_) <- getArgs
  trySolveOrigLevelNum (read firstarg :: Int)

----------------------------------------------------------------------
-- Data
----------------------------------------------------------------------

-- Thanks to Zach Wegner's python implementation
-- (see https://github.com/zwegner/game-about-squares-solver)
-- from where this data came.
origLevels = words "hi hi3 order2 push stairs stairs2 lift presq sq nobrainer crosst t rotation asymm clover preduced herewego reduced reduced2 spiral2 recycle2 recycle3 shirt shuttle spiral splinter elegant shuttle2 shirt2 windmill paper shuttle5 shirtDouble splinter2 reduced3 elegant2"

allLevels = map fst levels
hiddenLevels = allLevels \\ origLevels

levels = [
 ("hi",
 (
 Board [(Circle (Color "red") (Position 0 2))] [],
 State [(Square (Color "red") (Position 0 0) Down)]
 )),
 ("hi2",
 (
 Board [(Circle (Color "blue") (Position 0 0)),
 (Circle (Color "red") (Position 2 2))] []
 ,
 State [(Square (Color "blue") (Position 0 2) Up),
 (Square (Color "red") (Position 2 0) Down)]
 )),
 ("hi3",
 (
 Board [(Circle (Color "blue") (Position 0 1)),
 (Circle (Color "red") (Position 0 2))] []
 ,
 State [(Square (Color "blue") (Position 0 0) Down),
 (Square (Color "red") (Position 0 3) Up)]
 )),
 ("order",
 (
 Board [(Circle (Color "red") (Position 0 0)),
 (Circle (Color "blue") (Position 0 1)),
 (Circle (Color "black") (Position 2 2))] []
 ,
 State [(Square (Color "red") (Position 0 2) Up),
 (Square (Color "blue") (Position 2 1) LeftD),
 (Square (Color "black") (Position 2 0) Down)]
 )),
 ("order2",
 (
 Board [(Circle (Color "red") (Position 2 0)),
 (Circle (Color "blue") (Position 1 0)),
 (Circle (Color "black") (Position 1 1))] []
 ,
 State [(Square (Color "red") (Position 0 0) RightD),
 (Square (Color "blue") (Position 1 2) Up),
 (Square (Color "black") (Position 3 1) LeftD)]
 )),
 ("stupidpush",
 (
 Board [(Circle (Color "red") (Position 0 2)),
 (Circle (Color "black") (Position 0 3))] []
 ,
 State [(Square (Color "red") (Position 0 0) Down),
 (Square (Color "black") (Position 0 1) Up)]
 )),
 ("push",
 (
 Board [(Circle (Color "red") (Position 0 3)),
 (Circle (Color "blue") (Position 2 5))] []
 ,
 State [(Square (Color "blue") (Position 2 0) Down),
 (Square (Color "red") (Position 4 2) LeftD)]
 )),
 ("stairs",
 (
 Board [(Circle (Color "red") (Position 1 1)),
 (Circle (Color "black") (Position 2 2)),
 (Circle (Color "blue") (Position 3 3))] []
 ,
 State [(Square (Color "blue") (Position 0 1) RightD),
 (Square (Color "red") (Position 1 0) Down),
 (Square (Color "black") (Position 2 1) Down)]
 )),
 ("stairs2",
 (
 Board [(Circle (Color "red") (Position 1 1)),
 (Circle (Color "blue") (Position 2 2)),
 (Circle (Color "black") (Position 3 3))] []
 ,
 State [(Square (Color "blue") (Position 0 1) RightD),
 (Square (Color "red") (Position 1 0) Down),
 (Square (Color "black") (Position 2 1) Down)]
 )),
 ("lift",
 (
 Board [(Circle (Color "black") (Position 0 0)),
 (Circle (Color "blue") (Position 1 1)),
 (Circle (Color "red") (Position 2 3))] []
 ,
 State [(Square (Color "black") (Position 2 2) Up),
 (Square (Color "blue") (Position 4 1) LeftD),
 (Square (Color "red") (Position 3 0) Down)]
 )),
 ("presq",
 (
 Board [(Circle (Color "blue") (Position 2 0))]
 [(Arrow (Position 0 2) RightD),
 (Arrow (Position 2 2) Up)]
 ,
 State [(Square (Color "blue") (Position 0 0) Down)]
 )),
 ("sq",
 (
 Board [(Circle (Color "orange") (Position 2 0)),
 (Circle (Color "black") (Position 3 0))]
 [(Arrow (Position 0 2) RightD),
 (Arrow (Position 2 2) Up)]
 ,
 State [(Square (Color "orange") (Position 0 0) Down),
 (Square (Color "black") (Position 0 2) Down)]
 )),
 ("nobrainer",
 (
 Board [(Circle (Color "orange") (Position 1 0)),
 (Circle (Color "blue") (Position 2 0))]
 [(Arrow (Position 3 1) LeftD),
 (Arrow (Position 0 1) RightD),
 (Arrow (Position 2 1) Up)]
 ,
 State [(Square (Color "orange") (Position 0 1) Down),
 (Square (Color "blue") (Position 2 1) Down)]
 )),
 ("crosst",
 (
 Board [(Circle (Color "black") (Position 0 2)),
 (Circle (Color "red") (Position 1 2)),
 (Circle (Color "blue") (Position 3 2))]
 [(Arrow (Position 2 2) RightD)]
 ,
 State [(Square (Color "red") (Position 2 0) Down),
 (Square (Color "blue") (Position 4 2) LeftD),
 (Square (Color "black") (Position 2 4) Up)]
 )),
 ("t",
 (
 Board [(Circle (Color "red") (Position 1 2)),
 (Circle (Color "blue") (Position 2 2)),
 (Circle (Color "black") (Position 3 2))]
 [(Arrow (Position 2 0) Down)]
 ,
 State [(Square (Color "red") (Position 0 0) RightD),
 (Square (Color "blue") (Position 4 0) LeftD),
 (Square (Color "black") (Position 2 4) Up)]
 )),
 ("rotation",
 (
 Board [(Circle (Color "orange") (Position 1 2)),
 (Circle (Color "black") (Position 1 1))]
 [(Arrow (Position 1 0) Down),
 (Arrow (Position 3 1) LeftD)]
 ,
 State [(Square (Color "orange") (Position 0 2) RightD),
 (Square (Color "black") (Position 2 3) Up)]
 )),
 ("asymm",
 (
 Board [(Circle (Color "orange") (Position 0 4)),
 (Circle (Color "black") (Position 1 0)),
 (Circle (Color "blue") (Position 1 2))]
 [(Arrow (Position 1 1) Down)]
 ,
 State [(Square (Color "orange") (Position 3 3) LeftD),
 (Square (Color "black") (Position 1 1) Down),
 (Square (Color "blue") (Position 2 5) Up)]
 )),
 ("herewego",
 (
 Board [(Circle (Color "red") (Position 0 1)),
 (Circle (Color "blue") (Position 3 1))]
 [(Arrow (Position 0 0) RightD),
 (Arrow (Position 2 0) Down),
 (Arrow (Position 2 1) LeftD),
 (Arrow (Position 1 2) Up)]
 ,
 State [(Square (Color "red") (Position 0 0) Down),
 (Square (Color "blue") (Position 2 1) Down)]
 )),
 ("preherewego",
 (
 Board [(Circle (Color "red") (Position 1 1)),
 (Circle (Color "blue") (Position (-1) 1))]
 [(Arrow (Position 0 0) RightD),
 (Arrow (Position 2 0) Down),
 (Arrow (Position 2 1) LeftD),
 (Arrow (Position 1 2) Up)]
 ,
 State [(Square (Color "red") (Position 2 0) Down),
 (Square (Color "blue") (Position 1 2) Down)]
 )),
 ("clover",
 (
 Board [(Circle (Color "red") (Position 0 0)),
 (Circle (Color "blue") (Position 1 1)),
 (Circle (Color "black") (Position 2 2)),
 (Circle (Color "orange") (Position 2 0))] []
 ,
 State [(Square (Color "black") (Position 1 0) Down),
 (Square (Color "orange") (Position 0 1) RightD),
 (Square (Color "red") (Position 2 1) LeftD),
 (Square (Color "blue") (Position 1 2) Up)]
 )),
 ("preduced",
 (
 Board [(Circle (Color "red") (Position 1 1)),
 (Circle (Color "blue") (Position 1 2))]
 [(Arrow (Position 0 0) Down),
 (Arrow (Position 0 1) RightD),
 (Arrow (Position 3 0) LeftD)]
 ,
 State [(Square (Color "red") (Position 0 0) Down),
 (Square (Color "blue") (Position 2 2) Up)]
 )),
 ("preduced2",
 (
 Board [(Circle (Color "black") (Position 1 0)),
 (Circle (Color "red") (Position 1 1)),
 (Circle (Color "blue") (Position 1 2))]
 [(Arrow (Position 0 0) Down),
 (Arrow (Position 0 1) RightD)]
 ,
 State [(Square (Color "red") (Position 0 0) Down),
 (Square (Color "blue") (Position 2 2) Up),
 (Square (Color "black") (Position 3 0) LeftD)]
 )),
 ("reduced",
 (
 Board [(Circle (Color "red") (Position 1 1)),
 (Circle (Color "black") (Position 2 1)),
 (Circle (Color "blue") (Position 3 1))]
 [(Arrow (Position 0 0) Down),
 (Arrow (Position 0 1) RightD),
 (Arrow (Position 3 0) LeftD),
 (Arrow (Position 2 2) Up)]
 ,
 State [(Square (Color "red") (Position 0 0) Down),
 (Square (Color "blue") (Position 3 0) Down),
 (Square (Color "black") (Position 0 1) Down)]
 )),
 ("reduced2",
 (
 Board [(Circle (Color "red") (Position 1 1)),
 (Circle (Color "black") (Position 2 1)),
 (Circle (Color "blue") (Position 3 1))]
 [(Arrow (Position 0 0) Down),
 (Arrow (Position 0 1) RightD),
 (Arrow (Position 3 0) LeftD),
 (Arrow (Position 2 2) Up)]
 ,
 State [(Square (Color "black") (Position 0 0) Down),
 (Square (Color "blue") (Position 3 0) Down),
 (Square (Color "red") (Position 0 1) Down)]
 )),
 ("reduced3",
 (
 Board [(Circle (Color "black") (Position 1 2)),
 (Circle (Color "red") (Position 2 1)),
 (Circle (Color "blue") (Position 3 2))]
 [(Arrow (Position 0 0) Down),
 (Arrow (Position 0 1) RightD),
 (Arrow (Position 3 0) LeftD),
 (Arrow (Position 2 2) Up)]
 ,
 State [(Square (Color "black") (Position 0 0) Down),
 (Square (Color "blue") (Position 3 0) Down),
 (Square (Color "red") (Position 0 1) Down)]
 )),
 ("recycle",
 (
 Board [(Circle (Color "red") (Position 1 0)),
 (Circle (Color "blue") (Position 2 0)),
 (Circle (Color "black") (Position 1 1)),
 (Circle (Color "orange") (Position 2 1))]
 [(Arrow (Position 0 0) Down),
 (Arrow (Position 0 1) RightD),
 (Arrow (Position 3 0) LeftD),
 (Arrow (Position 4 0) LeftD),
 (Arrow (Position 3 1) Up),
 (Arrow (Position 4 1) Up)]
 ,
 State [(Square (Color "red") (Position 3 1) Down),
 (Square (Color "blue") (Position 3 0) Down),
 (Square (Color "black") (Position 0 1) Down),
 (Square (Color "orange") (Position 0 0) Down)]
 )),
 ("recycle2",
 (
 Board [(Circle (Color "green") (Position 1 0)),
 (Circle (Color "black") (Position 0 1)),
 (Circle (Color "purple") (Position 1 2)),
 (Circle (Color "gray") (Position 2 1))]
 [(Arrow (Position 0 0) Down),
 (Arrow (Position 0 2) RightD),
 (Arrow (Position 2 0) LeftD),
 (Arrow (Position 0 4) Up),
 (Arrow (Position 2 2) Up)]
 ,
 State [(Square (Color "black") (Position 0 0) Down),
 (Square (Color "green") (Position 0 2) Down),
 (Square (Color "gray") (Position 2 2) Down),
 (Square (Color "purple") (Position 2 0) Down)]
 )),
 ("recycle3",
 (
 Board [(Circle (Color "green") (Position 2 2)),
 (Circle (Color "yellow") (Position 2 3)),
 (Circle (Color "red") (Position 2 1)),
 (Circle (Color "gray") (Position 0 1))]
 [(Arrow (Position 2 0) LeftD),
 (Arrow (Position 0 0) Down),
 (Arrow (Position 2 4) Up),
 (Arrow (Position 0 2) Up),
 (Arrow (Position 0 3) Down),
 (Arrow (Position 0 4) RightD)]
 ,
 State [(Square (Color "red") (Position 0 4) Down),
 (Square (Color "green") (Position 2 4) Down),
 (Square (Color "gray") (Position 0 0) Down),
 (Square (Color "yellow") (Position 2 0) Down)]
 )),
 ("shuttle",
 (
 Board [(Circle (Color "red") (Position 1 3)),
 (Circle (Color "blue") (Position 2 3)),
 (Circle (Color "black") (Position 3 3))]
 [(Arrow (Position 0 2) RightD),
 (Arrow (Position 4 1) LeftD),
 (Arrow (Position 2 0) Down),
 (Arrow (Position 2 4) Up)]
 ,
 State [(Square (Color "blue") (Position 1 0) Down),
 (Square (Color "black") (Position 0 2) Down),
 (Square (Color "red") (Position 4 1) Down)]
 )),
 ("shuttle2",
 (
 Board [(Circle (Color "red") (Position 1 3)),
 (Circle (Color "blue") (Position 2 3)),
 (Circle (Color "black") (Position 3 3))]
 [(Arrow (Position 0 2) RightD),
 (Arrow (Position 4 1) LeftD),
 (Arrow (Position 3 0) Down),
 (Arrow (Position 3 4) Up)]
 ,
 State [(Square (Color "black") (Position 1 0) Down),
 (Square (Color "red") (Position 0 2) Down),
 (Square (Color "blue") (Position 4 1) Down)]
 )),
 ("shuttle5",
 (
 Board [(Circle (Color "blue") (Position 0 2)),
 (Circle (Color "red") (Position 2 2)),
 (Circle (Color "black") (Position 4 2))]
 [(Arrow (Position 1 1) RightD),
 (Arrow (Position 3 2) LeftD),
 (Arrow (Position 2 0) Down),
 (Arrow (Position 2 4) Up)]
 ,
 State [(Square (Color "blue") (Position 2 0) Down),
 (Square (Color "black") (Position 1 1) Down),
 (Square (Color "red") (Position 3 2) Down)]
 )),
 ("spiral",
 (
 Board [(Circle (Color "red") (Position 0 0)),
 (Circle (Color "blue") (Position 1 1)),
 (Circle (Color "black") (Position 2 2))]
 [(Arrow (Position 0 2) Down),
 (Arrow (Position 3 3) LeftD),
 (Arrow (Position 2 4) Up),
 (Arrow (Position 0 5) RightD),
 (Arrow (Position 3 5) Up),
 (Arrow (Position 1 3) Down),
 (Arrow (Position 1 4) RightD)]
 ,
 State [(Square (Color "red") (Position 0 2) Down),
 (Square (Color "black") (Position 2 4) Down),
 (Square (Color "blue") (Position 1 3) Down)]
 )),
 ("spiral2",
 (
 Board [(Circle (Color "red") (Position 2 (-1))),
 (Circle (Color "blue") (Position 2 1)),
 (Circle (Color "black") (Position 2 3))]
 [(Arrow (Position 0 2) Down),
 (Arrow (Position 3 3) LeftD),
 (Arrow (Position 2 4) Up),
 (Arrow (Position 0 5) RightD),
 (Arrow (Position 3 5) Up),
 (Arrow (Position 1 3) Down),
 (Arrow (Position 1 4) RightD)]
 ,
 State [(Square (Color "red") (Position 0 2) Down),
 (Square (Color "black") (Position 2 4) Down),
 (Square (Color "blue") (Position 1 3) Down)]
 )),
 ("windmill",
 (
 Board [(Circle (Color "red") (Position 2 1)),
 (Circle (Color "blue") (Position 3 2)),
 (Circle (Color "black") (Position 2 3)),
 (Circle (Color "orange") (Position 1 2))]
 [(Arrow (Position 2 0) Down),
 (Arrow (Position 4 2) LeftD),
 (Arrow (Position 2 4) Up)]
 ,
 State [(Square (Color "blue") (Position 2 0) Down),
 (Square (Color "black") (Position 4 2) Down),
 (Square (Color "orange") (Position 2 4) Down),
 (Square (Color "red") (Position 0 2) RightD)]
 )),
 ("shirt",
 (
 Board [(Circle (Color "red") (Position 2 0))]
 [(Arrow (Position 0 2) RightD),
 (Arrow (Position 1 0) Down),
 (Arrow (Position 1 4) Up),
 (Arrow (Position 4 3) LeftD)]
 ,
 State [(Square (Color "red") (Position 0 2) Down),
 (Square (Color "blue") (Position 1 0) Down),
 (Square (Color "black") (Position 4 1) Down)]
 )),
 ("shirt2",
 (
 Board [(Circle (Color "blue") (Position 0 1))]
 [(Arrow (Position 0 2) RightD),
 (Arrow (Position 1 0) Down),
 (Arrow (Position 1 4) Up),
 (Arrow (Position 4 3) LeftD)]
 ,
 State [(Square (Color "red") (Position 0 2) Down),
 (Square (Color "blue") (Position 1 0) Down),
 (Square (Color "black") (Position 4 1) Down)]
 )),
 ("shirtDouble",
 (
 Board [(Circle (Color "red") (Position 2 0)),
 (Circle (Color "blue") (Position 0 1))]
 [(Arrow (Position 0 2) RightD),
 (Arrow (Position 1 0) Down),
 (Arrow (Position 1 4) Up),
 (Arrow (Position 4 3) LeftD)]
 ,
 State [(Square (Color "red") (Position 0 2) Down),
 (Square (Color "blue") (Position 1 0) Down),
 (Square (Color "black") (Position 4 1) Down)]
 )),
 ("paper",
 (
 Board [(Circle (Color "red") (Position 0 2)),
 (Circle (Color "blue") (Position 0 1)),
 (Circle (Color "black") (Position 1 0))]
 [(Arrow (Position 0 0) Down),
 (Arrow (Position 3 0) LeftD),
 (Arrow (Position 0 3) RightD),
 (Arrow (Position 2 3) Up)]
 ,
 State [(Square (Color "blue") (Position 2 3) Down),
 (Square (Color "red") (Position 0 3) Down),
 (Square (Color "black") (Position 0 0) Down)]
 )),
 ("splinter",
 (
 Board [(Circle (Color "red") (Position 0 0)),
 (Circle (Color "blue") (Position 1 0)),
 (Circle (Color "black") (Position 2 0))]
 [(Arrow (Position 0 1) Down),
 (Arrow (Position 0 3) RightD),
 (Arrow (Position 2 3) Up),
 (Arrow (Position 3 1) LeftD)]
 ,
 State [(Square (Color "blue") (Position 0 1) Down),
 (Square (Color "black") (Position 0 3) Down),
 (Square (Color "red") (Position 2 3) Down)]
 )),
 ("splinter2",
 (
 Board [(Circle (Color "blue") (Position 0 0)),
 (Circle (Color "red") (Position 1 0)),
 (Circle (Color "black") (Position 2 0))]
 [(Arrow (Position 1 1) Down),
 (Arrow (Position 1 3) RightD),
 (Arrow (Position 3 3) Up),
 (Arrow (Position 4 1) LeftD)]
 ,
 State [(Square (Color "blue") (Position 1 1) Down),
 (Square (Color "black") (Position 1 3) Down),
 (Square (Color "red") (Position 3 3) Down)]
 )),
 ("elegant",
 (
 Board [(Circle (Color "blue") (Position 0 1)),
 (Circle (Color "orange") (Position 2 0)),
 (Circle (Color "black") (Position 3 2)),
 (Circle (Color "red") (Position 1 3))]
 [(Arrow (Position 1 0) Down),
 (Arrow (Position 0 2) RightD),
 (Arrow (Position 3 1) LeftD),
 (Arrow (Position 2 3) Up)]
 ,
 State [(Square (Color "blue") (Position 0 2) Down),
 (Square (Color "red") (Position 2 3) Down),
 (Square (Color "black") (Position 3 1) Down),
 (Square (Color "orange") (Position 1 0) Down)]
 )),
 ("elegant2",
 (
 Board [(Circle (Color "black") (Position 2 0)),
 (Circle (Color "red") (Position 3 0)),
 (Circle (Color "orange") (Position 4 0))]
 [(Arrow (Position 1 0) Down),
 (Arrow (Position 0 3) RightD),
 (Arrow (Position 3 4) Up),
 (Arrow (Position 4 1) LeftD)]
 ,
 State [(Square (Color "black") (Position 2 4) RightD),
 (Square (Color "orange") (Position 0 4) RightD),
 (Square (Color "red") (Position 1 4) RightD)]
 ))]
