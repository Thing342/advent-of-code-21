module Day01 (soln) where

import Text.Printf
import Debug.Trace

import Lib

skip2 :: [a] -> [(a,a)]
skip2 [] = []
skip2 [x] = []
skip2 [x,y] = [(x,y)]
skip2 (x:y:zs) = (x,y) : skip2 (y:zs)

skip3 :: [a] -> [(a,a,a)]
skip3 [] = []
skip3 [x] = []
skip3 [x,y] = []
skip3 [x,y,z] = [(x,y,z)]
skip3 (x:y:z:aas) = (x,y,z) : skip3 (y:z:aas)

day01_1_pure :: [Int] -> Int
day01_1_pure ds = let
    dds = skip2 ds
    increased = map (\(a,b) -> if a < b then 1 else 0) dds
    in sum increased

day01_2_pure :: [Int] -> Int
day01_2_pure ds = let
    dms = skip3 ds
    dmss = map (\(a,b,c) -> a + b + c) dms
    in day01_1_pure dmss

part1 :: IO Int
part1 = do
    ss <- Lib.readInput 1 1
    let ds = [read s :: Int | s <- ss]
    return $ day01_1_pure ds

part2 :: IO Int
part2 = do
    ss <- readInput 1 1
    let ds = [read s :: Int | s <- ss]
    return $ day01_2_pure ds

soln :: Lib.Day
soln = Lib.Day {_daynum = 1, _part1 = part1, _part2 = part2}