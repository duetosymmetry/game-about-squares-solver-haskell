Game about squares solver (Haskell)
===================================

This is a pretty simple solver for Andrey Shevchuk's Game About
Squares, aka GAS (http://gameaboutsquares.com). As a challenge to
myself I wrote the solver in Haskell (where I have no
experience). Thanks to @akeshet for getting me hooked on the game and
originally inspiring me to write a solver (after seeing his python
implementation). This solver does not do the fancy stuff that
@zwegner/game-about-squares-solver does (though thanks to his python
level data I did not have to input it all by hand).

To compile: `ghc --make Levels`
To run: `./Levels [original level number]`
Or load interactively:
```
trySolveOrigLevelNum origLevelNum
-- or use the lower-level driver
let Just (board, initState) = origLevelByNumber 15
let (smap, moves) = trySolveLevel board initState maxDepth
putStr $ moveListStr' moves
```

If you are a Haskell guru or know one, please let me know how this
code can be made more idiomatic/efficient/etc. Feedback welcome.

Leo C. Stein
