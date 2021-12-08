module Day07 where

import Lib

distCost :: Bool -> Int -> Int -> Int
distCost False from to = abs (from - to)
distCost True from to = let 
    n = abs (from - to)
    in n * (n + 1) `div` 2 

sumDist :: Bool -> Int -> [Int] -> Int
sumDist p2 v = foldr (\x s -> s + (distCost p2 x v)) 0

solve :: Bool -> String -> Int
solve p2 line = let
    d = read <$> splitBy ',' line
    in foldr min maxBound [ sumDist p2 x d | x <- [1..length d] ]

solveIO :: Bool -> IO Int
solveIO p2 = do
    inpt <- readInput 7 1
    (return . (solve p2) . head) inpt

soln :: Lib.Day
soln = Lib.Day 7 (solveIO False) (solveIO True)