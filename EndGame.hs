{-|
Module      : EndGame
Description : Tests a GameState for conditions that cause the game to end
Authors     : Wm. Keith van der Meulen, Clayton Vis
-}

module EndGame where

import ApocTools

-- | Checks for end of game
isOver :: GameState -> Maybe(String)
isOver gs
    | pawnEnd /= Nothing = pawnEnd -- Look for winner due to one player with no pawns first
    | penaltyEnd /= Nothing = penaltyEnd -- Look for winner due to one player with 2 penalties second
    | passingEnd /= Nothing = passingEnd -- Look for winner due to both players passing last
    | otherwise = Nothing
    where
        pawnEnd = noPawns gs
        penaltyEnd = twoPenalty gs
        passingEnd = doublePass gs

-- | Check for player with no pawns
noPawns :: GameState -> Maybe(String)
noPawns gs
    | blackPawn == 0 && whitePawn == 0 = findTieWinner gs
    | blackPawn == 0 = Just "White wins! Black has no pawns."
    | whitePawn == 0 = Just "Black wins! White has no pawns."
    | otherwise = Nothing
    where
        whitePawn = countPawns White (theBoard gs)
        blackPawn = countPawns Black (theBoard gs)

-- | Check for two penalty points
twoPenalty :: GameState -> Maybe(String)
twoPenalty gs
    | whitePenalty == 2 && blackPenalty == 2 = findTieWinner gs
    | whitePenalty == 2 = Just "Black wins! White has 2 penalty points."
    | blackPenalty == 2 = Just "White wins! Black has 2 penalty points."
    | otherwise = Nothing
    where
        whitePenalty = whitePen gs
        blackPenalty = blackPen gs

-- | Check for both players passing
doublePass :: GameState -> Maybe(String)
doublePass gs
    | whitePlayed == Passed && blackPlayed == Passed = findTieWinner gs
    | otherwise = Nothing
    where
        whitePlayed = whitePlay gs
        blackPlayed = blackPlay gs

-- | Evaluate winner in tie case
findTieWinner :: GameState -> Maybe(String)
findTieWinner gs
    | whitePawn > blackPawn = Just "White wins! White has more pawns than Black."
    | blackPawn > whitePawn = Just "Black wins! Black has more pawns than White."
    | whitePenalty > blackPenalty = Just "Black wins! White has more penalty points than Black."
    | blackPenalty > whitePenalty = Just "White wins! Black has more penalty points than White."
    | otherwise = Just "Tie game! Both players have the same number of pawns and penalty points."
    where
        whitePawn = countPawns White (theBoard gs)
        blackPawn = countPawns Black (theBoard gs)
        whitePenalty = whitePen gs
        blackPenalty = blackPen gs

-- | Counts the number of pawns a certain player has on the board
countPawns :: Player -> Board -> Int
countPawns player board
    | player == White = length (filter (==WP) (foldr (++) [] board))
    | player == Black = length (filter (==BP) (foldr (++) [] board))

-- | Counts the number of pawns a certain player has on the board
countKnights :: Player -> Board -> Int
countKnights player board
    | player == White = length (filter (==WK) (foldr (++) [] board))
    | player == Black = length (filter (==BK) (foldr (++) [] board))
