module Lib where

import Text.Printf
import Debug.Trace

import Data.Map (Map, (!))
import qualified Data.Map as Map

data Day = Day {
    _daynum :: Int,
    _part1  :: IO Int,
    _part2  :: IO Int
}

puzzleInputDir :: String
puzzleInputDir = "./in"

puzzleInputFile :: Int -> Int -> String
puzzleInputFile day part = printf "%s/day%02d.%d" puzzleInputDir day part

testInputFile :: Int -> Int -> String
testInputFile day part = printf "%s/test%02d.%d" puzzleInputDir day part

readInput :: Int -> Int -> IO [String]
readInput day part = do
    myInput <- readFile (puzzleInputFile day part)
    return $ lines myInput

testInput :: Int -> Int -> IO [String]
testInput day part = do
    myInput <- readFile (testInputFile day part)
    return $ lines myInput

chunks :: Int -> [a] -> [[a]]
chunks n = windows n n

windows :: Int -> Int -> [a] -> [[a]]
windows _ _ [] = []
windows step n items = (take n items) : (windows step n (drop step items))

insertWithMany :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a -> Map k a
insertWithMany _ [] mymap = mymap
insertWithMany f ((key, val):ns) mymap = Map.insertWith f key val (insertWithMany f ns mymap)

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy delim [] = [[]]
splitBy delim l@(x:xs) = let
    (r:rs) = splitBy delim xs
    in if x == delim then []:r:rs else (x:r):rs

divmod :: (Integral a) => a -> a -> (a, a)
divmod a b = (a `div` b, a `mod` b)

debugval val = trace (show val) val
