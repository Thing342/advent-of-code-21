module Day03 (soln) where

import Text.Printf
import Debug.Trace
import qualified Data.Vector.Unboxed as V
import Data.Bits

import Lib

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

part1 :: IO Int
part1 = do
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

part2 :: IO Int
part2 = do
    ss <- readInput 3 1
    let (oxy, co2) = day03_02_pure ss
    return (oxy * co2)

soln :: Lib.Day
soln = Lib.Day {_daynum = 3, _part1 = part1, _part2 = part2}