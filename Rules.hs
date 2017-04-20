{-|
Module      : Rules
Description : Provides methods to validate whether a move is valid on a given board state
Authors     : Stepan F-B
-}

module Rules where

import Data.Maybe (fromJust, isNothing)
import ApocTools

-- | Returns a bool as to whether the player owns the piece at the tile
ownsPiece :: [[Cell]] -> (Int, Int) -> Player -> Bool
ownsPiece board (srcX, srcY) player
    | cell /= E && playerOf (pieceOf (cell)) == player = True -- Note: pieceOf doesn't have a case for E :(
    | otherwise = False
    where
        cell = getFromBoard board (srcX, srcY)

-- | Returns a bool as to whether the pawn move is valid directionally
validPawnDirection :: [[Cell]] -> (Int, Int) -> (Int, Int) -> Player -> Bool
validPawnDirection board (srcX, srcY) (destX, destY) player
    | abs deltaY > 1 = False -- They can only move 1 forward
    | deltaX /= 0 && abs deltaX /= 1 = False -- They can only move 0 sideways or 1 sideways
    | player == White && deltaY <= 0 = False -- Ensure they are moving in the right direction (if it is White, move down (larger index), else up)
    | player == Black && deltaY >= 0 = False
    -- Now we know that they are moving 1 forward and either 0 or 1 tiles sideways
    | deltaX == 0 && abs deltaY == 1 && destCell == E = True -- They are moving forward into an empty cell
    | deltaX == 0 && abs deltaY == 1 && destCell /= E = False -- They are moving forward into a opponent
    -- They must be moving diagonaly to be valid now
    | destCell == E = False -- You can't move diagonally to an empty cell
    | playerOf (pieceOf (destCell)) /= player = True -- Moving diagonally to a cell that is the not the player
    | otherwise = False
    where 
        srcCell = getFromBoard board (srcX, srcY)
        destCell = getFromBoard board (destX, destY)
        deltaX = destX - srcX
        deltaY = destY - srcY

-- | Returns a bool as to whether the knight move is valid directionally
validKnightDirection :: [[Cell]] -> (Int, Int) -> (Int, Int) -> Player -> Bool
validKnightDirection board (srcX, srcY) (destX, destY) player
    | abs deltaX == 1 && abs deltaY == 2 = True
    | abs deltaX == 2 && abs deltaY == 1 = True
    | otherwise = False
    where
        deltaX = destX - srcX
        deltaY = destY - srcY

-- | Returns bool as to whether the bounds are proper for a given move
validMoveBounds :: (Int, Int) -> (Int, Int) -> Bool
validMoveBounds (srcX, srcY) (destX, destY)
    | srcX < 0 || srcX > 4 = False
    | srcY < 0 || srcY > 4 = False
    | destX < 0 || destX > 4 = False
    | destY < 0 || destY > 4 = False
    | otherwise = True

-- | Returns whether that given move on the board is valid for player
validMove :: [[Cell]] -> (Int, Int) -> (Int, Int) -> Player -> Bool
validMove board (srcX, srcY) (destX, destY) player
    | validMoveBounds (srcX, srcY) (destX, destY) == False = False -- Ensure the bounds are valid (human handles this for us, but whatever)
    | isPawn == False && isKnight == False = False -- Ensure it is a pawn or knight
    | ownsPiece board (srcX, srcY) player == False = False -- Ensure they own the piece they're moving
    | ownsPiece board (destX, destY) player == True = False -- Ensure the piece destination is a tile they don't have a piece in
    | isPawn && validPawnDirection board (srcX, srcY) (destX, destY) player == True = True -- If it is a pawn, check if it is a valid direction
    | isKnight && validKnightDirection board (srcX, srcY) (destX, destY) player == True = True -- If it is a knight, check if it is a valid direction
    | otherwise = False
    where 
        srcCell = getFromBoard board (srcX, srcY)
        destCell = getFromBoard board (destX, destY)
        isPawn = srcCell == WP || srcCell == BP
        isKnight = srcCell == WK || srcCell == BK

-- | Parses and returns the proper Played var (Only works for standard moves -ex. Not pawn placements or whatever)
parseNormalMove :: Player -> Maybe [(Int, Int)] -> GameState -> Played
parseNormalMove player move state
    | isNothing move = Passed
    | validMove board (srcX, srcY) (destX, destY) player == False = Goofed ((srcX, srcY), (destX, destY)) -- Ensure the move is valid
    | otherwise = Played ((srcX, srcY), (destX, destY))
    where 
        board = theBoard state
        justMove = fromJust move
        srcX = fst (justMove!!0)
        srcY = snd (justMove!!0)
        destX = fst (justMove!!1)
        destY = snd (justMove!!1)

-- | Returns a bool as to whether the given pawn relocation move is valid
validRelocationMove :: Maybe [(Int, Int)] -> GameState -> Bool
validRelocationMove move gs
    | isNothing move = False
    | destCell == E = True
    | otherwise = False
    where 
        board = theBoard gs
        justMove = fromJust move
        destX = fst (justMove!!0)
        destY = snd (justMove!!0)
        destCell = getFromBoard board (destX, destY)
