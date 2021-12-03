module Lib
    ( someFunc, readInput, puzzleInputFile, skip2, day01_01, day01_02
    ) where

import Text.Printf
import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

puzzleInputDir = "/Users/wjordan/code/adventofcode21/in"

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

skip2 :: [a] -> [(a,a)]
skip2 [] = []
skip2 (x:[]) = []
skip2 (x:y:[]) = [(x,y)]
skip2 (x:y:zs) = (x,y) : (skip2 (y:zs))

skip3 :: [a] -> [(a,a,a)]
skip3 [] = []
skip3 (x:[]) = []
skip3 (x:y:[]) = []
skip3 (x:y:z:[]) = [(x,y,z)]
skip3 (x:y:z:aas) = (x,y,z) : (skip3 (y:z:aas))

day01_1_pure :: [Int] -> Int
day01_1_pure ds = let
    dds = skip2 ds
    increased = map (\(a,b) -> if (a < b) then 1 else 0) dds
    in sum increased

day01_2_pure :: [Int] -> Int
day01_2_pure ds = let
    dms = skip3 ds
    dmss = map (\(a,b,c) -> a + b + c) dms
    in day01_1_pure (trace (show dmss) dmss)

day01_01 :: IO Int
day01_01 = do
    ss <- readInput 1 1
    let ds = [read s :: Int | s <- ss]
    return $ day01_1_pure ds

day01_02 :: IO Int
day01_02 = do
    ss <- readInput 1 1
    let ds = [read s :: Int | s <- ss]
    return $ day01_2_pure ds
