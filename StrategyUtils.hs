{-|
Module      : Strategy Utils
Description : Provides utility functions in order to show, parse, and return strategies for given input
Authors     : Stepan F-B
-}

module StrategyUtils where

import ApocTools
import ApocStrategyHumanSol
import RandomAI
import GreedyAI

-- | Only human is necessary for now, but we need two other AI strategies
strategies = ["human", "random", "greedy"]

-- | Prints possible strategies in console
showStrategies = do
  putStrLn "Possible strategies:"
  mapM_ showStrategy strategies

-- | Prints the strategy with proper formatting in console
showStrategy :: String -> IO ()
showStrategy x = do
  putStrLn ("  " ++ x)

-- | Returns whether both strategies are valid
stratsValid :: String -> String -> Bool
stratsValid strat1 strat2
  | stratValid strat1 && stratValid strat2 = True
  | otherwise = False

-- | Returns whether the strat is valid
stratValid :: String -> Bool
stratValid strat
  | strat `elem` strategies = True
  | otherwise = False

-- | Given a strategy string, returns the strategy function
getStrategyFromString :: String -> Chooser
getStrategyFromString strat
  | strat == "human" = human
  | strat == "random" = randomMove
  | strat == "greedy" = greedyMove

-- | Prompts the user 
getStrategyForPlayer x = do
  putStrLn ("Enter the strategy for " ++ x ++ ":")
  strat <- getLine
  return strat
