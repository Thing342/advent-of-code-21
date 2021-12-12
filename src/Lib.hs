module Lib where

import Text.Printf
import Debug.Trace

import Data.Map (Map, (!))
import qualified Data.Map as Map

import qualified BinaryTree

data Day = Day {
    _daynum :: Int,
    _part1  :: IO Int,
    _part2  :: IO Int
}

puzzleInputDir :: String
puzzleInputDir = "./in"

puzzleInputFile :: Int -> Int -> String
puzzleInputFile = printf "%s/day%02d.%d" puzzleInputDir

testInputFile :: Int -> Int -> String
testInputFile = printf "%s/test%02d.%d" puzzleInputDir

readInput :: Int -> Int -> IO [String]
readInput day part = do
    myInput <- readFile $ puzzleInputFile day part
    return $ lines myInput

testInput :: Int -> Int -> IO [String]
testInput day part = do
    myInput <- readFile $ testInputFile day part
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

easyrange :: Int -> Int -> [Int]
easyrange a b = if a > b then [a,a-1..b] else [a..b]

minimize :: (Ord a, Bounded a) => [a] -> a
minimize = foldr min maxBound

maximize :: (Ord a, Bounded a) => [a] -> a
maximize = foldr max minBound

count :: (Traversable t, Ord k, Num v) => t k -> Map k v
count = let
    accum c m = Map.insertWith (+) c 1 m
    in foldr accum Map.empty

median :: (Ord a) => [a] -> Either (a,a) a
median = BinaryTree.treeMedian . BinaryTree.fromList

eprintf = error . printf

debugval val = trace (show val) val
