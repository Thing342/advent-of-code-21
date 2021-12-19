module Lib where

import qualified BinaryTree
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Debug.Trace
import Text.Printf
import Data.Maybe (catMaybes, listToMaybe)

data Day = Day
  { _daynum :: Int,
    _part1 :: IO String,
    _part2 :: IO String
  }

puzzleInputDir :: String
puzzleInputDir = "./in"

puzzleInputFile :: Int -> Int -> String
puzzleInputFile = printf "%s/day%02d.%d" puzzleInputDir

testInputFile :: Int -> Int -> String
testInputFile = printf "%s/test%02d.%d" puzzleInputDir

puzzleInput :: Bool -> Int -> Int -> IO [String]
puzzleInput testing day part = lines <$> readFile (file day part)
    where file = if testing then testInputFile else puzzleInputFile

readInput :: Int -> Int -> IO [String]
readInput = puzzleInput False

testInput :: Int -> Int -> IO [String]
testInput = puzzleInput True

chunks :: Int -> [a] -> [[a]]
chunks n = windows n n

windows :: Int -> Int -> [a] -> [[a]]
windows _ _ [] = []
windows step n items = take n items : windows step n (drop step items)

insertWithMany :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a -> Map k a
insertWithMany f kvs m = foldr (uncurry $ Map.insertWith f) m kvs

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy delim [] = [[]]
splitBy delim l@(x : xs) =
  let (r : rs) = splitBy delim xs
   in if x == delim then [] : r : rs else (x : r) : rs

divmod :: (Integral a) => a -> a -> (a, a)
divmod a b = (a `div` b, a `mod` b)

easyrange :: Int -> Int -> [Int]
easyrange a b = if a > b then [a, a -1 .. b] else [a .. b]

minimize :: (Ord a, Bounded a) => [a] -> a
minimize = foldr min maxBound

maximize :: (Ord a, Bounded a) => [a] -> a
maximize = foldr max minBound

count :: (Traversable t, Ord k, Num v) => t k -> Map k v
count = foldr (\c -> Map.insertWith (+) c 1) Map.empty

median :: (Ord a) => [a] -> Either (a, a) a
median = BinaryTree.treeMedian . BinaryTree.fromList

anyWithKey :: (k -> a -> Bool) -> Map k a -> Bool
anyWithKey f m = Map.size (Map.filterWithKey f m) == 0

max2 :: (Foldable t, Bounded a, Ord a, Bounded b, Ord b) => t (a, b) -> (a, b)
max2 = foldr (\(xb, yb) (xm, ym) -> (max xb xm, max yb ym)) (minBound, minBound)

readCSV :: (Read a) => String -> [a]
readCSV s = read <$> splitBy ',' s

readCSV2 :: (Read a) => String -> (a,a)
readCSV2 s = let (x : y : _) = readCSV s in (x,y)

enumerate :: [a] -> [(Int, a)]
enumerate xs = [1..] `zip` xs

minMaxOf :: (Foldable f, Bounded a, Ord a) => f a -> (a,a)
minMaxOf = foldr (\a1 (mn, mx) -> (min mn a1, max mx a1)) (maxBound, minBound)

within :: (Ord a) => a -> (a,a) -> Bool
a `within` (a1,a2) = a1 <= a && a <= a2  

findMaybe :: [Maybe a] -> Maybe a
findMaybe = listToMaybe . catMaybes



eprintf = error . printf

debugval val = trace (show val) val
