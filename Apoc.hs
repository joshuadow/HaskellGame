{- |
Module      : Main
Description : Template to get you started on the CPSC 449 Winter 2016 Apocalypse assignment.
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3

This module is used for CPSC 449 for the Apocalypse assignment.

Feel free to modify this file as you see fit.

-}

module Main (
      -- * Main
      main, main'
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHumanSol
import StrategyUtils
import Rules
import Moves
import EndGame
import Upgrade

---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
  if length args == 0 then do
      showStrategies
      strat1 <- getStrategyForPlayer "BLACK"
      strat2 <- getStrategyForPlayer "WHITE"
      
      if stratsValid strat1 strat2 == False then do
        showStrategies
      else do
        play strat1 strat2

  else do
    let strat1 = args!!0
    let strat2 = args!!1

    if stratsValid strat1 strat2 == False then do
      showStrategies
    else do
      play strat1 strat2

  return ()

-- | Given a Played and current penalties for the player, returns the new amount of penalties
checkPenalty :: Played -> Int -> Int
checkPenalty (Goofed (_)) penalties = penalties + 1
checkPenalty _ penalties = penalties

-- | If the second passed in GameState is not None, returns it, otherwise returns the first state
getRecentState :: Maybe GameState -> Maybe GameState -> IO GameState
getRecentState oldstate newstate = do
  if newstate /= Nothing then do
    return (fromJust newstate)
  else do
    return (fromJust oldstate)


-- | Main Recursive Game Loop
gameLoop :: Chooser -> Chooser -> GameState -> IO()
gameLoop blackstrat whitestrat state = do

  -- Get the move for black
  blackInputMove <- blackstrat state Normal Black

  let blackPenalties = blackPen state
  let blackMove = parseNormalMove Black blackInputMove state

  -- Get the move for white
  whiteInputMove <- whitestrat state Normal White

  let whitePenalties = whitePen state
  let whiteMove = parseNormalMove White whiteInputMove state

  let movedState = move (state {blackPlay = blackMove, blackPen = checkPenalty blackMove blackPenalties, whitePlay = whiteMove, whitePen = checkPenalty whiteMove whitePenalties})

  print movedState

  -- Check if we need to upgrade stuff
  upgradeState <- upgrade movedState blackstrat whitestrat

  if (upgradeState /= Nothing) then do
    -- Print the upgraded state
    print $ fromJust upgradeState
  else do
    return () -- ty haskell

  recentState <- getRecentState (Just movedState) (upgradeState)

  let isEndGame = isOver recentState

  if isEndGame == Nothing then
    gameLoop blackstrat whitestrat recentState
  else
    putStrLn (fromJust isEndGame)

  return ()

-- | Starts playing the two strategies (THEY MUST BE ALREADY VALID)
play :: String -> String -> IO()
play x y = do
  let strat1 = getStrategyFromString x
  let strat2 = getStrategyFromString y

  print initBoard
  
  gameLoop strat1 strat2 initBoard

