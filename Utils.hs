{-|
Module      : Utils
Description : Provides utility functions used throughout the Apoc program
Authors     : Wm. Keith van der Meulen, Clayton Vis, Stepan F-B, Josh Dow, Samuel Raemy
-}

module Utils where

import ApocTools
import Rules

---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

-- | Returns the amount of elements == a in the list
elemCount :: Eq a => a -> [a] -> Int
elemCount elem a = length (filter (==elem) a)

-- | Returns the amount of elements == a in the 2 dimensional list
elemCountMulti :: Eq a => a -> [[a]] -> Int
elemCountMulti _ [] = 0
elemCountMulti elem (x:xs) = elemCount elem x + elemCountMulti elem xs

-- | Converts a Played data type to a regular (src, dest) tuple
fromPlayed :: Played -> ((Int, Int), (Int, Int))
fromPlayed (Played ((srcX, srcY), (destX, destY))) = ((srcX, srcY), (destX, destY))


-- AI Utility Functions

-- | Offsets for black pawn moves
pawnBlackMoves = [(0, -1), (-1, -1), (1, -1)]

-- | Offsets for white pawn moves
pawnWhiteMoves = [(0, 1), (-1, 1), (1, 1)]

-- | Offsets for knight moves
knightMoves = [(1, 2), (1, -2), (-1, -2), (-1, 2), (2, 1), (-2, 1), (2, -1), (-2, -1)]

-- | Returns the possible moves for a given piece
getPieceMoves :: Board -> (Int, Int) -> [(Int, Int)] -> Player -> [((Int, Int), (Int, Int))]
getPieceMoves _ _ [] _ = []
getPieceMoves board coord possible@(x:xs) player = 
    if validMove board (xCoord, yCoord) (xCoord+xOffset, yCoord+yOffset) player then
        ((xCoord, yCoord), (xCoord+xOffset, yCoord+yOffset)):[] ++ getPieceMoves board coord xs player
    else
        getPieceMoves board coord xs player
    where
        xCoord = fst coord
        yCoord = snd coord
        xOffset = fst x
        yOffset = snd x

-- | Returns move offets for a given piece
getMovesForPiece :: Cell -> [(Int, Int)]
getMovesForPiece WP = pawnWhiteMoves
getMovesForPiece BP = pawnBlackMoves
getMovesForPiece WK = knightMoves
getMovesForPiece BK = knightMoves
getMovesForPiece E  = []

-- | Gets possible moves for board (starting with (Int, Int))
getPossibleMoves :: Board -> (Int, Int) -> Player -> IO ([((Int, Int), (Int, Int))])
getPossibleMoves _ (0, 5) _ = do return []
getPossibleMoves board (x, y) player = do
    let newCoord = (if x == 4 then 0 else x+1, if x == 4 then y+1 else y)

    nextMoves <- getPossibleMoves board newCoord player

    if ownedByPlayer then do
        let offsets = getMovesForPiece cell
        return (getPieceMoves board (x, y) offsets player ++ nextMoves)
    else do
        return nextMoves

    where
        cell = getFromBoard board (x, y)
        ownedByPlayer = cell /= E && playerOf (pieceOf (cell)) == player
        isKnight = cell == WK || cell == BK
        isPawn = cell == WP || cell == BP

-- | Returns all of the empty cell coords
getAllEmptyCoords :: Board -> (Int, Int) -> IO ([(Int, Int)])
getAllEmptyCoords _ (0, 5) = do return []
getAllEmptyCoords board (x, y) = do
    let newCoord = (if x == 4 then 0 else x+1, if x == 4 then y+1 else y)

    next <- getAllEmptyCoords board newCoord
    if cell == E then do
        return ((x, y):[] ++ next)
    else do
        return (next)
    where
        cell = getFromBoard board (x, y)
