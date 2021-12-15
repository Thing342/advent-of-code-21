module Day14 (soln) where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Lib

type Input = (String, Map String Char)

type Formulas = Map String Char

type PairMap = Map String Int

formula :: String -> (String, Char)
formula line =
  let (from : _ : to : _) = words line
   in (from, head to)

pairMapFromSeed :: String -> PairMap
pairMapFromSeed s = Map.fromListWith (+) [(p, 1) | p <- windows 1 2 s, length p > 1]

fromInput :: [String] -> Input
fromInput ls =
  let (seed, _ : formulas) = splitAt 1 ls
   in (head seed, Map.fromList $ map formula formulas)

react :: Formulas -> PairMap -> PairMap
react fs =
  let go (pair@[x, y], i) = Map.insertWith (+) [x, k] i . Map.insertWith (+) [k, y] i where k = fs ! pair
      go _ = error "unreachable"
   in foldr go Map.empty . Map.toList

countPairs :: String -> PairMap -> Map Char Int
countPairs seed =
  let f (pair, n) = Map.insertWith (+) (head pair) n . Map.insertWith (+) (last pair) n
      bump c = Map.insertWith (+) c 1
   in bump (head seed) . bump (last seed) . foldr f Map.empty . Map.toList

solve :: Int -> Input -> Int
solve n (seed, formula) =
  let (mn, mx) = minMaxOf . map snd . Map.toList . countPairs seed . (!! n) . iterate (react formula) . pairMapFromSeed $ seed
   in (mx - mn) `div` 2

soln :: Lib.Day
soln = Lib.Day 14 (show . solve 10 . fromInput <$> readInput 14 1) (show . solve 40 . fromInput <$> readInput 14 1)