module Lib where

import Text.Printf
import Debug.Trace
import Data.Complex
import qualified Data.Vector.Unboxed as V
import Data.Bits

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

--

type Vector = V.Vector

bin2dec :: String -> Int
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

mcb :: Int -> Vector Int -> Bool
mcb i nums = let 
    bits = map (\j -> if (testBit j i) then 1 else 0) (V.toList nums)
    in (sum bits) > ((V.length nums) `div` 2)

group :: (a -> Bool) -> [a] -> ([a], [a])
group f [] = ([], [])
group f (x:xs) = let
    (ts, fs) = group f xs
    in if (f x) then (x:ts, fs) else (ts, x:fs)

groupByDigit :: Int -> [Int] -> ([Int], [Int])
groupByDigit bit = group (\n -> testBit n bit)

filterOxy :: Int -> [Int] -> Int
filterOxy _ (i:[]) = i
filterOxy bit nums = let
    (ones, zeros) = groupByDigit bit nums
    next = if ((length ones) >= (length zeros)) then ones else zeros
    in filterOxy (bit - 1) next

filterCO2 :: Int -> [Int] -> Int
filterCO2 _ (i:[]) = i
filterCO2 bit nums = let
    (ones, zeros) = groupByDigit bit nums
    next = if ((length ones) >= (length zeros)) then zeros else ones
    in filterCO2 (bit - 1) next

gamma :: Int -> Vector Int -> Int
gamma msb nums = (foldr (\c s -> s * 2 + c) 0 ) $ [fromEnum (mcb j nums) | j <- [0..msb-1]]

epsilon :: Int -> Vector Int -> Int
epsilon msb nums = (foldr (\c s -> s * 2 + c) 0 ) $ [(fromEnum . not) (mcb j nums) | j <- [0..msb-1]]

day03_01_pure :: [String] -> (Int, Int)
day03_01_pure ss = let
    bs = length $ head ss
    nums = V.fromList (map bin2dec ss)
    g = gamma bs nums
    e = epsilon bs nums
    in (g,e)

day03_01 :: IO Int
day03_01 = do
    ss <- readInput 3 1
    let (g, e) = day03_01_pure ss
    return (g * e)

day03_02_pure :: [String] -> (Int, Int)
day03_02_pure ss = let
    bs = length $ head ss
    nums = map bin2dec ss
    oxy = filterOxy (bs - 1) nums
    co2 = filterCO2 (bs - 1) nums
    in (oxy, co2)

day03_02 :: IO Int
day03_02 = do
    ss <- readInput 3 1
    let (oxy, co2) = day03_02_pure ss
    return (oxy * co2)

