{-|
Module      : Moves
Description : Provides move logic handling functions
Authors     : Clayton Vis, Stepan F-B
-}

module Moves where

import ApocTools
import Utils

-- | Given a GameState with two unperformed moves, performs the valid moves
move :: GameState -> GameState
move gs
    | (isPlayed $ whitePlay gs) && (isPlayed $ blackPlay gs)    = processBoth gs
    | isPlayed $ whitePlay gs                                   = processSingle White gs
    | isPlayed $ blackPlay gs                                   = processSingle Black gs
    | otherwise                                                 = gs

-- | Given two unperformed valid moves in a GameState, executes the moves on the board and returns the new GameSate
processBoth :: GameState -> GameState
processBoth gs
    | whiteSource == blackDest  && blackSource == whiteDest             = swap
    | blackDest == whiteSource                                          = blackDestToWhiteSrc
    | whiteDest == blackSource                                          = whiteDestToBlackSrc
    | whiteDest == blackDest    && blPiece == BP    && whPiece == WP    = clash
    | whiteDest == blackDest    && blPiece == BK    && whPiece == WK    = clash
    | whiteDest == blackDest    && blPiece == BK    && whPiece == WP    = whitePawnKill
    | whiteDest == blackDest    && blPiece == BP    && whPiece == WK    = blackPawnKill
    | otherwise = moveBoth
    where
        board          = theBoard gs
        state          = GameState (blackPlay gs) (blackPen gs) (whitePlay gs) (whitePen gs)
        whiteSource    = fst $ justCord $ whitePlay gs
        whiteDest      = snd $ justCord $ whitePlay gs
        blackSource    = fst $ justCord $ blackPlay gs
        blackDest      = snd $ justCord $ blackPlay gs
        whPiece        = getFromBoard board whiteSource
        blPiece        = getFromBoard board blackSource
        blPlay         = blackPlay gs
        whPlay         = whitePlay gs

        -- If the piece is moving to the origin of the other piece that is moving, we ensure that we preserve the piece types and only clear the proper origin
        blackDestToWhiteSrc   = state $ replace2 (replace2 (replace2 board blackSource E) whiteDest whPiece) blackDest blPiece
        whiteDestToBlackSrc   = state $ replace2 (replace2 (replace2 board whiteSource E) blackDest blPiece) whiteDest whPiece

        swap           = state $ swapPieces board blackDest whiteDest blPiece whPiece
        clash          = state $ allEmpty board whiteSource blackSource
        moveBoth       = state $ movePiece (movePiece board whPlay) blPlay
        whitePawnKill  = state $ replace2 (replace2 (replace2 board whiteSource E) blackSource E) blackDest BK
        blackPawnKill  = state $ replace2 (replace2 (replace2 board whiteSource E) blackSource E) whiteDest WK

-- | Given a Player and GameState with a Played move for that player, performs the move and returns the new GameState
processSingle :: Player -> GameState -> GameState
processSingle p gs
    | p == White  = state $ movePiece (theBoard gs) (whitePlay gs)
    | p == Black  = state $ movePiece (theBoard gs) (blackPlay gs)
    where
        state = GameState (blackPlay gs) (blackPen gs) (whitePlay gs) (whitePen gs)
         
-- | Performs a given Played move on the Board and returns the new GameState
movePiece :: Board -> Played -> Board
movePiece board (Played (sourceCord, destCord)) = replace2 (replace2 board sourceCord E) destCord sourcePiece
    where
        sourcePiece = getFromBoard board (sourceCord)

-- | Given a Board and two piece coords and their Cell types, swaps the two pieces and returns the new state of the Board
swapPieces :: Board -> (Int, Int) -> (Int, Int) -> Cell -> Cell -> Board
swapPieces board (blCoord) (whCoord) blPiece whPiece = replace2 (replace2 board blCoord blPiece) whCoord whPiece

-- | Given a Board and two coords, removes the pieces on both tiles and returns the new Board
allEmpty :: Board -> (Int, Int) -> (Int, Int) -> Board
allEmpty board cord1 cord2 = replace2 (replace2 board cord1 E) cord2 E

-- | Given a Played data type, returns the source and destination tuples specifying the move coords
justCord :: Played -> ((Int,Int),(Int,Int))
justCord (Played (src, dest)) = (src, dest)

-- | Checks whether a Played data type is of a Played state
isPlayed :: Played -> Bool
isPlayed (Played((_,_),(_,_)))    = True
isPlayed x                        = False

-- | Moves a given upgraded piece from the given source coord to the dest coord on the board and returns it
moveUpgradePiece :: Board -> ((Int,Int),(Int,Int)) -> Board
moveUpgradePiece board move = (replace2 (replace2 board src E) dest srcPiece)
    where
        src = fst move
        dest = snd move
        srcPiece = getFromBoard board src
