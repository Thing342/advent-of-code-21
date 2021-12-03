module Lib where

import Text.Printf
import Debug.Trace
import Data.Complex

puzzleInputDir :: String
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

---

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
    in day01_1_pure dmss

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

---
type State = (Int, Int, Int)
type Cmd = (String, Int)

zero :: State
zero = (0, 0, 0)

getCmd :: String -> Cmd
getCmd line = let
    ws = words line
    v = read $ (head . tail) ws
    in (head ws, v)

readCmd :: Bool ->  State -> Cmd -> State
readCmd False (x,y,aim) ("forward", v) = (x + v, y, aim)
readCmd False (x,y,aim) ("up", v)  = (x, y - v, aim)
readCmd False (x,y,aim) ("down", v) = (x, y + v, aim)
readCmd True (x,y,aim) ("forward", v) = (x + v, y + (v * aim), aim)
readCmd True (x,y,aim) ("up", v) = (x, y, aim - v)
readCmd True (x,y,aim)("down", v) = (x, y, aim + v)

day02_01_pure :: State -> [Cmd] -> State
day02_01_pure = foldl (readCmd False)

day02_01 :: IO Int
day02_01 = do
    ss <- readInput 2 1
    let cmds = map getCmd ss
    let (x,y,aim) = day02_01_pure zero cmds
    return $ x * y

day02_02_pure :: State -> [Cmd] -> State
day02_02_pure = foldl (readCmd True)

day02_02 :: IO Int
day02_02 = do
    ss <- readInput 2 1
    let cmds = map getCmd ss
    let (x,y,aim) = day02_02_pure zero cmds
    return $ x * y