{-|
Module      : Upgrade
Description : Provides functions that test and upgrade a given game state after moves are made
Authors     : Stepan F-B, Josh Dow
-}

module Upgrade where

import Data.Maybe (fromJust, isNothing)
import ApocTools
import Utils
import Moves
import Rules

-- | Given a GameState with recent moves, performs necessary pawn upgrades and relocation
upgrade :: GameState -> Chooser -> Chooser -> IO (Maybe GameState)
upgrade gs blackstrat whitestrat = do
    if (blNeedUpgrade && whNeedUpgrade) then do
        newgs <- upgradeBoth gs blackstrat whitestrat
        return (Just newgs)
    else do
        if (blNeedUpgrade) then do -- Execute black if possible
            newgs <- upgradeSingle whiteClearPlay Black blackstrat
            return (Just newgs)
        else do
            if (whNeedUpgrade) then do -- Execute white if possible
                newgs <- upgradeSingle blackClearPlay White whitestrat
                return (Just newgs)
            else do
                return Nothing
    where
        board           = theBoard gs

        isBlPlay        = isPlayed $ blackPlay gs
        blDest          = snd $ fromPlayed $ blackPlay gs
        blDestY         = snd blDest
        blDestPiece     = getFromBoard board blDest
        blNeedUpgrade   = isBlPlay && blDestY == 0 && blDestPiece == BP

        isWhPlay        = isPlayed $ whitePlay gs
        whDest          = snd $ fromPlayed $ whitePlay gs
        whDestY         = snd whDest
        whDestPiece     = getFromBoard board whDest
        whNeedUpgrade   = isWhPlay && whDestY == 4 && whDestPiece == WP

        blackClearPlay  = gs {blackPlay = None}
        whiteClearPlay  = gs {whitePlay = None}

-- | Upgrades both moves on the GameState
upgradeBoth :: GameState -> Chooser -> Chooser -> IO GameState
upgradeBoth gs blackstrat whitestrat = do
    firstUpgrade <- upgradeSingle gs Black blackstrat
    secondUpgrade <- upgradeSingle firstUpgrade White whitestrat

    return secondUpgrade

-- | Prompts for relocation on the GameState for the given Player and returns the updated state
relocationPrompt :: GameState -> Player -> Chooser -> IO GameState
relocationPrompt gs player strat = do
    move <- strat gs PawnPlacement player


    if move /= Nothing && player == White then do
        let moveCoords = (whiteDest, fromJust move!!0)

        if validRelocationMove move gs then do
            let newBoard = moveUpgradePiece board moveCoords
            return (gs {whitePlay = PlacedPawn moveCoords, theBoard = newBoard})
        else do
            return (gs {whitePlay = BadPlacedPawn moveCoords, whitePen = (whitePen gs) + 1})
    else do
        if move /= Nothing then do
            -- Must be black
            let moveCoords = (blackDest, fromJust move!!0)

            if validRelocationMove move gs then do
                let newBoard = moveUpgradePiece board moveCoords
                return (gs {blackPlay = PlacedPawn moveCoords, theBoard = newBoard})
            else do
                return (gs {blackPlay = BadPlacedPawn moveCoords, blackPen = (blackPen gs) + 1})
        else do
            if player == White then do
                return (gs {whitePlay = NullPlacedPawn, whitePen = (whitePen gs) + 1})
            else do
                return (gs {blackPlay = NullPlacedPawn, blackPen = (blackPen gs) + 1})
    where
        board       = theBoard gs

        whiteCoords = fromPlayed (whitePlay gs)
        whiteDest   = snd whiteCoords

        blackCoords = fromPlayed (blackPlay gs)
        blackDest   = snd blackCoords

-- | Upgrades a single move for the player on the GameState, and returns the resultant state
upgradeSingle :: GameState -> Player -> Chooser -> IO GameState
upgradeSingle gs player strat = do
    if (player == Black && blDestY == 0 && blDestPiece == BP) then do
        if (elemCountMulti BK board < 2) then do
            return blackToKnight
        else do
            prompt <- blackPrompt
            return prompt
    else do
        if (player == White && whDestY == 4 && whDestPiece == WP) then do
            if (elemCountMulti WK board < 2) then do
                return whiteToKnight
            else do
                prompt <- whitePrompt
                return prompt
        else do
            return gs
    where
        whDest          = snd $ fromPlayed $ whitePlay gs
        whDestY         = snd whDest
        whDestPiece     = getFromBoard board whDest
        blDest          = snd $ fromPlayed $ blackPlay gs
        blDestY         = snd blDest
        blDestPiece     = getFromBoard board blDest
        board           = theBoard gs
        blackToKnight   = gs {theBoard = (replace2 board blDest BK), blackPlay = UpgradedPawn2Knight (blDest)}
        whiteToKnight   = gs {theBoard = (replace2 board whDest WK), whitePlay = UpgradedPawn2Knight (whDest)}
        whitePrompt     = relocationPrompt gs White strat
        blackPrompt     = relocationPrompt gs Black strat
