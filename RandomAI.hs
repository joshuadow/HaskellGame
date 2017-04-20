{-|
Module      : Random AI
Description : Provides random move AI
Authors     : Stepan F-B
-}

module RandomAI where

import ApocTools
import Utils
import Rules
import System.Random

-- | Random Move Entry Point
randomMove                  :: Chooser
randomMove gs Normal player = do
    possible <- getPossibleMoves (theBoard gs) (0, 0) player

    if length possible == 0 then do
        -- No possible moves, pass
        return Nothing
    else do
        gen <- newStdGen
        let randIndex = fst (randomR (0, length possible-1) gen)
        return (Just [fst (possible!!randIndex), snd (possible!!randIndex)])


randomMove gs PawnPlacement player = do
    emptyCoords <- getAllEmptyCoords (theBoard gs) (0, 0)

    gen <- newStdGen
    let randIndex = fst (randomR (0, length emptyCoords-1) gen)

    -- There is guaranteed to be at least one empty space, so we don't need to handle that
    return (Just [(emptyCoords!!randIndex)])
