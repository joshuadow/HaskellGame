{-|
Module      : Greedy AI
Description : Provides a Greedy Move AI. If it can, it upgrades a pawn. For normal moves,
              returns the greedy move 90% of the time and a random move 10% of the time; this 
              is done to avoid infinite loops with itself. For pawn upgrades, it chooses a
              random move.
Authors     : Stepan F-B, Samuel Raemy
-}

module GreedyAI where

import Data.Maybe (fromJust, isNothing)
import ApocTools
import Utils
import Rules
import EndGame
import System.Random

-- | Defines a data structure that allows us to store a move's coords and it's "point" weighting
data GreedyMoveAndPoints = GreedyMoveAndPoints {
    src :: (Int, Int),
    dest :: (Int, Int), 
    points :: Int
}


-- | Given a Cell, returns how many greedy points its worth
getGreedyPoints :: Cell -> Int
getGreedyPoints WK = 2
getGreedyPoints BK = 2
getGreedyPoints WP = 1
getGreedyPoints BP = 1
getGreedyPoints E  = 0

-- | Returns a list of the most greedy moves on the board
getMostGreedy :: Board -> [((Int, Int), (Int, Int))] -> Player -> [GreedyMoveAndPoints]
getMostGreedy _ [] _ = [GreedyMoveAndPoints (0, 0) (0, 0) (-1)]
getMostGreedy board (x:xs) player =
    if (thispoints > nextPoints) then -- This has more points, return just this
        [GreedyMoveAndPoints src dest thispoints]
    else
        if (thispoints == nextPoints) then -- Same amount of points, add to the list
            (GreedyMoveAndPoints src dest thispoints):next
        else -- Less than the next, return next 
            next

    where
        src         = fst x
        dest        = snd x
        destCell    = getFromBoard board dest
        thispoints  = getGreedyPoints destCell
        next        = (getMostGreedy board xs player)
        nextPoints  = points (next!!0)


-- | Tries to find a move that can bring a pawn to the last rank
lookingForUpgrade :: Board -> (Int, Int) -> Player -> Maybe (((Int, Int), (Int, Int)))
lookingForUpgrade _ (0, 5) _ = Nothing
lookingForUpgrade board (x, y) player =
    if (cell == BP && player == Black && y == 1 && validMove board (x, y) (x, y-1) player) then Just ((x, y), (x, y-1)) -- We can upgrade this black pawn
    else 
        if (cell == WP && player == White && y == 3 && validMove board (x, y) (x, y+1) player) then Just ((x, y), (x, y+1)) -- We can upgrade this white pawn
        else lookingForUpgrade board newCoord player -- Try the next coord

    where 
        newCoord = (if x == 4 then 0 else x+1, if x == 4 then y+1 else y)
        cell = getFromBoard board (x, y)


-- | Entry point of the Greedy strategy
greedyMove                  :: Chooser
greedyMove gs Normal player = do
    possible <- getPossibleMoves board (0, 0) player

    if length possible == 0 then do
        -- No possible moves, pass
        return Nothing
    else do
        gen <- newStdGen
        let shouldGreedy = fst (randomR (0, 9) gen) :: Int

        let pawnUpgrade = if (pawnAmt > 1 && knightAmt < 2) then lookingForUpgrade board (0, 0) player else Nothing

        if isNothing pawnUpgrade == False then do
            -- Upgrade the pawn
            let nonJustPawn = fromJust pawnUpgrade
            return (Just [fst nonJustPawn, snd nonJustPawn])
        else do
            if (shouldGreedy > 0) then do
                -- Choose the greedy move 90% of the time

                let greedy = getMostGreedy board possible player
                let mostPoints = points (greedy!!0)

                -- Choose a random greedy move from the best moves
                gen <- newStdGen
                let randIndex = fst (randomR (0, length greedy-1) gen) :: Int
                return (Just [src (greedy!!randIndex), dest (greedy!!randIndex)])
            else do
                -- Choose a random move 10% of the time (to avoid infinite loops)
                gen <- newStdGen
                let randIndex = fst (randomR (0, length possible-1) gen) :: Int
                return (Just [fst (possible!!randIndex), snd (possible!!randIndex)])

    where
        board = (theBoard gs)
        pawnAmt = countPawns player board
        knightAmt = countKnights player board


greedyMove gs PawnPlacement player = do
    -- Get random pawn placement move
    emptyCoords <- getAllEmptyCoords (theBoard gs) (0, 0)

    gen <- newStdGen
    let randIndex = fst (randomR (0, length emptyCoords-1) gen) :: Int

    -- There is guaranteed to be at least one empty space, so we don't need to handle that
    return (Just [(emptyCoords!!randIndex)])
