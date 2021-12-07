module Day05 where

import Text.Printf
import Debug.Trace
import Data.Bits
import Data.List

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Lib

easyrange :: Int -> Int -> [Int]
easyrange a b = if a > b then [a,a-1..b] else [a..b]

data Point = Point Int Int deriving (Show, Eq, Ord)

initPoint :: String -> Point
initPoint s = let 
    (x,y) = read $ printf "(%s)" s
    in Point x y

data Line = Line Point Point deriving (Show)
data LineType = Horizontal | Vertical | Diagonal deriving (Show)

initLine :: String -> Line
initLine line = let
    (p1:_:p2:_) = words line
    in Line (initPoint p1) (initPoint p2)

lineType :: Line -> LineType
lineType (Line (Point x1 y1) (Point x2 y2)) = if (x1 == x2) 
        then Vertical 
    else if (y1 == y2) 
        then Horizontal 
    else Diagonal

interpolate :: Bool -> Line -> Maybe [Point]
interpolate part2 line@(Line (Point x1 y1) (Point x2 y2)) = case lineType line of
    Horizontal -> Just [Point x y1 | x <- easyrange x1 x2]
    Vertical   -> Just [Point x1 y | y <- easyrange y1 y2]
    Diagonal   -> case part2 of
        False      -> Nothing
        True       -> Just [Point x y | (x,y) <- (easyrange x1 x2) `zip` (easyrange y1 y2)]

data ProblemState = ProblemState {
    _lines :: [Line],
    _overlaps :: Map Point Int,
    _p2 :: Bool
} deriving (Show)

readLine :: Line -> ProblemState -> ProblemState
readLine line@(Line from to) st = let
    nlines = line : _lines st
    in case interpolate (_p2 st) line of
        Just ps -> let
            noverlaps = insertWithMany (+) [(p, 1) | p <- ps] (_overlaps st)
            in ProblemState {_lines = nlines, _overlaps = noverlaps, _p2 = _p2 st}
        Nothing -> st

initProblem :: Bool -> [String] -> ProblemState
initProblem part2 ls = let
    st = ProblemState {_lines = [], _overlaps = Map.empty, _p2 = part2}
    in foldr readLine st (initLine <$> ls)

solve :: ProblemState -> Int
solve st = length $ (filter (>= 2)) $ Map.elems $ _overlaps st

solveIO :: Bool -> Bool -> IO Int
solveIO p2 test = do
    let readin = if test then testInput else readInput
    d <- readin 5 1
    let st = initProblem p2 d
    return $ solve st

soln :: Lib.Day
soln = Lib.Day 5 (solveIO False False) (solveIO True False)