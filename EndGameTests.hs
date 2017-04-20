{-|
Module      : EndGameTests
Description : Testing suite for EndGame module
Authors     : Wm. Keith van der Meulen, Clayton Vis
-}

module EndGameTests where

import ApocTools
import EndGame

-- | Produces an array of Strings that contains the results of the tests with each index corresponding the test condition in main
stringResults :: Int -> [Bool] -> [String]
stringResults _ [] = []
stringResults x (b:bs) = (prettyResults x b) : stringResults (x+1) bs

-- | Converts a boolean that describes if the test results matches the expected result to a string with the test number and the result
prettyResults :: Int -> Bool -> String
prettyResults x True = "Test " ++ (show x) ++ ": " ++ "Pass"
prettyResults x False = "Test " ++ (show x) ++ ": " ++ "Fail"

-- | Runs all the different test cases through the testing function
runEndGameTests :: [(GameState, Maybe(String))] -> [Bool]
runEndGameTests arr = map gameStateTestResult arr

-- | Tests if the EndGame produces the expected result for a given GameState
gameStateTestResult :: (GameState, Maybe(String)) -> Bool
gameStateTestResult (gs, expected)
    | (isOver gs) == expected = True
    | otherwise = False

-- | Describes the tests run on the EndGame module and prints the results
main = do
    mapM_ (putStrLn.show) $ stringResults 1 (runEndGameTests gss)
    where
        -- All of the tests as an array of (GameState, Expected Result) tuples
        gss = [
                -- Test 1: Initial Board
                ((GameState Init 0 Init 0 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Nothing),
                
                -- Test 2: No white pawns
                ((GameState Init 0 Init 0 [
                [WK, E , E , E , WK],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "Black wins! White has no pawns."),
                
                -- Test 3: No black pawns
                ((GameState Init 0 Init 0 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BK, E , E , E , BK]]),
                
                Just "White wins! Black has no pawns."),
                
                -- Test 4: No pawns at all, equal penalties
                ((GameState Init 0 Init 0 [
                [WK, E , E , E , WK],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BK, E , E , E , BK]]),
                
                Just "Tie game! Both players have the same number of pawns and penalty points."),
                
                -- Test 5: No pawns at all, black has more penalties
                ((GameState Init 1 Init 0 [
                [WK, E , E , E , WK],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BK, E , E , E , BK]]),
                
                Just "White wins! Black has more penalty points than White."),
                
                -- Test 6: No pawns at all, white has more penalties
                ((GameState Init 0 Init 1 [
                [WK, E , E , E , WK],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BK, E , E , E , BK]]),
                
                Just "Black wins! White has more penalty points than Black."),
                
                -- Test 7: No pawns at all, black has 2 penalties
                ((GameState Init 2 Init 0 [
                [WK, E , E , E , WK],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BK, E , E , E , BK]]),
                
                Just "White wins! Black has more penalty points than White."),
                
                -- Test 8: No pawns at all, white has 2 penalties
                ((GameState Init 0 Init 2 [
                [WK, E , E , E , WK],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BK, E , E , E , BK]]),
                
                Just "Black wins! White has more penalty points than Black."),
                
                -- Test 9: No pawns at all, both have 2 penalties
                ((GameState Init 2 Init 2 [
                [WK, E , E , E , WK],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BK, E , E , E , BK]]),
                
                Just "Tie game! Both players have the same number of pawns and penalty points."),
                
                -- Test 10: Black has 2 penalties
                ((GameState Init 2 Init 0 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "White wins! Black has 2 penalty points."),
                
                -- Test 11: White has 2 penalties
                ((GameState Init 0 Init 2 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "Black wins! White has 2 penalty points."),
                
                -- Test 12: Both have 2 penalties, same number of pawns
                ((GameState Init 2 Init 2 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "Tie game! Both players have the same number of pawns and penalty points."),
                
                -- Test 13: Both have 2 penalties, black has more pawns
                ((GameState Init 2 Init 2 [
                [WK, WP, WP, WP, WK],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "Black wins! Black has more pawns than White."),
                
                -- Test 14: Both have 2 penalties, white has more pawns
                ((GameState Init 2 Init 2 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BK, BP, BP, BP, BK]]),
                
                Just "White wins! White has more pawns than Black."),
                
                -- Test 15: Black passed
                ((GameState Passed 0 Init 0 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Nothing),
                
                -- Test 16: White passed
                ((GameState Init 0 Passed 0 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Nothing),
                
                -- Test 17: Both passed, same number of pawns
                ((GameState Passed 0 Passed 0 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "Tie game! Both players have the same number of pawns and penalty points."),
                
                -- Test 18: Both passed, black has more pawns
                ((GameState Passed 0 Passed 0 [
                [WK, WP, WP, WP, WK],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "Black wins! Black has more pawns than White."),
                
                -- Test 19: Both passed, white has more pawns
                ((GameState Passed 0 Passed 0 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BK, BP, BP, BP, BK]]),
                
                Just "White wins! White has more pawns than Black."),
                
                -- Test 20: Both have two penalties, both passed, same number of pawns
                ((GameState Passed 2 Passed 2 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "Tie game! Both players have the same number of pawns and penalty points."),
                
                -- Test 21: Both have two penalties, both passed, black has more pawns
                ((GameState Passed 2 Passed 2 [
                [WK, WP, WP, WP, WK],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "Black wins! Black has more pawns than White."),
                
                -- Test 22: Both have two penalties, both passed, white has more pawns
                ((GameState Passed 2 Passed 2 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BK, BP, BP, BP, BK]]),
                
                Just "White wins! White has more pawns than Black."),
                
                -- Test 23: Both have two penalties, black passed, same number of pawns
                ((GameState Passed 2 Init 2 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "Tie game! Both players have the same number of pawns and penalty points."),
                
                -- Test 24: Both have two penalties, black passed, black has more pawns
                ((GameState Passed 2 Init 2 [
                [WK, WP, WP, WP, WK],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "Black wins! Black has more pawns than White."),
                
                -- Test 25: Both have two penalties, black passed, white has more pawns
                ((GameState Passed 2 Init 2 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BK, BP, BP, BP, BK]]),
                
                Just "White wins! White has more pawns than Black."),
                
                -- Test 26: Both have two penalties, white passed, same number of pawns
                ((GameState Init 2 Passed 2 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "Tie game! Both players have the same number of pawns and penalty points."),
                
                -- Test 27: Both have two penalties, white passed, black has more pawns
                ((GameState Init 2 Passed 2 [
                [WK, WP, WP, WP, WK],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BP, E , E , E , BP],
                [BK, BP, BP, BP, BK]]),
                
                Just "Black wins! Black has more pawns than White."),
                
                -- Test 28: Both have two penalties, white passed, white has more pawns
                ((GameState Init 2 Passed 2 [
                [WK, WP, WP, WP, WK],
                [WP, E , E , E , WP],
                [E , E , E , E , E ],
                [E , E , E , E , E ],
                [BK, BP, BP, BP, BK]]),
                
                Just "White wins! White has more pawns than Black.")
            ]
