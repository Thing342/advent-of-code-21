module Day07 (soln) where

import Lib

distCost :: Bool -> Int -> Int -> Int
distCost False from to = abs (from - to)
distCost True from to = let 
    n = abs (from - to)
    in n * (n + 1) `div` 2 

sumDist :: Bool -> [Int] -> Int -> Int
sumDist p2 d v = sum [distCost p2 v x | x <- d]

solve :: Bool -> String -> Int
solve p2 line = let
    d = read <$> splitBy ',' line
    in minimize [sumDist p2 d i | i <- [1..length d]]

solveIO :: Bool -> IO Int
solveIO p2 = do
    inpt <- readInput 7 1
    return . solve p2 $ head inpt

soln :: Lib.Day
soln = Lib.Day 7 (solveIO False) (solveIO True)