module Day08 (soln) where

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.List

import qualified Data.Sort (sort)

import Lib

findMapping :: [String] -> Map Char Char
findMapping sigvalues = let
    apps = count $ concat sigvalues
    (four:_) = filter (\w -> (length w) == 4) sigvalues

    decodeV c 9 = 'f'
    decodeV c 4 = 'e'
    decodeV c 6 = 'b'
    decodeV c 7 = if c `elem` four then 'd' else 'g'
    decodeV c 8 = if c `elem` four then 'c' else 'a'

    in Map.mapWithKey decodeV apps

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
decodeOutputNumbers = readDigits . decodeOutputDigits

solve :: Bool -> [String] -> Int
solve False = length . filter (`elem` [1,4,7,8]) . concat . map decodeOutputDigits
solve True = sum . map decodeOutputNumbers

solveIO :: Bool -> IO Int
solveIO p2 = do
    inpt <- readInput 8 1
    return $ solve p2 inpt

soln :: Lib.Day
soln = Lib.Day 8 (solveIO False) (solveIO True)