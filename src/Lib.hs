module Lib where

import Text.Printf
import Debug.Trace

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

debugval val = trace (show val) val