module Day09 (soln) where

import Data.List (sort)

import Lib

type Matrix = [[Int]]
type Point = (Int,Int)

fromInput :: [String] -> Matrix
fromInput = map (map (\x -> read [x]))

rowLength :: Matrix -> Int
rowLength = length . head

colLength :: Matrix -> Int
colLength = length

(!) :: Matrix -> Point -> Int
m ! (x, y)
     | 0 <= y && y < colLength m && 0 <= x && x < rowLength m = m !! y !! x
     | otherwise = maxBound

neighbours :: Point -> [Point]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isLowPoint :: Matrix -> Point -> Bool
isLowPoint m p = all ((> (m ! p)) . (m !)) . neighbours $ p

lowPoints :: Matrix -> [Point]
lowPoints m = [(x,y) | x <- [0..rowLength m - 1], y <- [0..colLength m - 1], isLowPoint m (x, y)]

riskLevel :: Matrix -> Point -> Int
riskLevel m p = 1 + m ! p

collectBasin :: Matrix -> [Point] -> Point -> [Point]
collectBasin m s p
     | m ! p < 9 && notElem p s = foldl (collectBasin m) (p:s) (neighbours p)
     | otherwise = s

collectBasins :: Matrix -> [Point] -> [[Point]]
collectBasins m = map (collectBasin m [])

solve :: Bool -> Matrix -> Int
solve False m = sum . map (riskLevel m) . lowPoints $ m
solve True m = product . take 3 . reverse . sort . map (sum . map length) . collectBasins m . lowPoints $ m

solveIO :: Bool -> IO Int
solveIO p2 = solve p2 . fromInput <$> readInput 9 1

soln :: Lib.Day
soln = Lib.Day 9 (solveIO False) (solveIO True)