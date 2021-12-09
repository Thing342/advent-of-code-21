module Day08 (soln) where

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Maybe
import Data.List

import qualified Data.Sort (sort)

import Lib

countCharAppearances :: [String] -> Map Char Int
countCharAppearances values = let
	accum c m = Map.insertWith (+) c 1 m
	in foldr accum (Map.empty) (concat values)

findMapping :: [String] -> Map Char Char
findMapping sigvalues = let
	apps = countCharAppearances sigvalues
	four = fromJust $ find (\w -> (length w) == 4) sigvalues

	decodeV :: Char -> Int -> Char
	decodeV c 9 = 'f'
	decodeV c 4 = 'e'
	decodeV c 6 = 'b'
	decodeV c 7 = if c `elem` four then 'd' else 'g'
	decodeV c 8 = if c `elem` four then 'c' else 'a'

	in Map.fromList [(k, decodeV k v) | (k,v) <- Map.toList apps ]

decodeDigit :: Map Char Char -> String -> Int
decodeDigit m scrambled = let
	decoded = sort $ (m !) <$> scrambled
	in case decoded of
		"abcefg" -> 0
		"cf" -> 1
		"acdeg" -> 2
		"acdfg" -> 3
		"bcdf" -> 4
		"abdfg" -> 5
		"abdefg" -> 6
		"acf" -> 7
		"abcdefg" -> 8
		"abcdfg" -> 9

decodeOutputDigits :: String -> [Int]
decodeOutputDigits line = let
	(signal, _:output) = span (\s -> s /= "|") $ words line
	mapping = findMapping signal
	in (decodeDigit mapping) <$> output

readDigits :: [Int] -> Int
readDigits xs = snd $ foldr (\a (m, b) -> (m * 10, (a * m) + b)) (1,0) xs

decodeOutputNumbers :: String -> Int
decodeOutputNumbers line = readDigits $ decodeOutputDigits line

solve :: Bool -> [String] -> Int
solve False ls = length $ (filter (\x -> x `elem` [1,4,7,8])) $ concat (map decodeOutputDigits ls) where
solve True ls = sum (map decodeOutputNumbers ls)

solveIO :: Bool -> IO Int
solveIO p2 = do
	inpt <- readInput 8 1
	return $ solve p2 inpt

soln :: Lib.Day
soln = Lib.Day 8 (solveIO False) (solveIO True)