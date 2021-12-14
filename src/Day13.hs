module Day13 (soln) where


import Data.Set (Set, member, notMember)
import qualified Data.Set as Set
import Control.Monad.State (State, get, gets, put, when, modify, execState)

import Advent.OCR (parseLetters)

import Lib

type Point = (Int,Int)
data DotMatrix = DotMatrix {
  _m :: Set Point,
  _xz :: Int,
  _yz :: Int
} deriving (Show)

type Fold = (String,Int)

type Action a = State DotMatrix a

initPoints :: [String] -> Set Point
initPoints = let
  go s m = let (x : y : _) = read <$> splitBy ',' s
    in Set.insert (x,y) m
  in foldr go Set.empty

initFolds :: [String] -> [Fold]
initFolds = map go where
  go l = (head w, read $ w !! 1) where
    term = words l !! 2
    w = splitBy '=' term

max2 :: (Bounded a, Ord a, Bounded b, Ord b) => [(a,b)] -> (a,b)
max2 = foldr (\(xb,yb) (xm,ym) -> (max xb xm, max yb ym)) (minBound, minBound)

initFromInput :: [String] -> (DotMatrix, [Fold])
initFromInput ss = let
  b = splitBy "" ss
  m = initPoints $ head b
  (xz,yz) = max2 $ Set.toList m
  in (DotMatrix {_xz = xz, _yz = yz, _m = m}, initFolds (b !! 1))

movePoint :: Point -> Point -> Action ()
movePoint p1 p2 = modify $ \st -> st {_m = Set.insert p2 . Set.delete p1 $ _m st }

foldPointUp :: Int -> Point -> Action ()
foldPointUp y p@(x1,y1) = when (y1 >= y) $ do
  let y2 = y - (y1 - y)
  movePoint p (x1, y2)

foldPointLeft :: Int -> Point -> Action ()
foldPointLeft x p@(x1,y1) = when (x1 >= x) $ do
  let x2 = x - (x1 - x)
  movePoint p (x2, y1)

fold :: Fold -> Action ()
fold (d, v) = do
  m <- gets _m
  let (action, update) = if d == "x" then (foldPointLeft, \x st -> st {_xz = x}) else (foldPointUp, \y st -> st {_yz = y})
  mapM_ (action v) m
  modify $ update v

part1 :: [String] -> Int
part1 inpt = let
  (st, folds) = initFromInput inpt
  actions = fold $ head folds
  in length . _m . execState actions $ st

part2 :: [String] -> String
part2 inpt = let
  (st, folds) = initFromInput inpt
  actions = mapM fold folds
  (Just code) = parseLetters . _m . execState actions $ st
  in code

soln :: Lib.Day
soln = Lib.Day 13 (show . part1 <$> readInput 13 1) (part2 <$> readInput 13 1)