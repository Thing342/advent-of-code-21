module Day13 (soln) where

import Advent.OCR (parseLetters)
import Control.Monad.State (State, execState, get, gets, modify, put, when)
import Data.Maybe (fromJust)
import Data.Set (Set, member, notMember)
import qualified Data.Set as Set
import Lib

type Point = (Int, Int)

type DotMatrix = Set Point

data FoldDir = FoldLeft | FoldUp deriving (Show, Eq)

type Fold = (FoldDir, Int)

type Action a = State DotMatrix a

initPoints :: [String] -> Set Point
initPoints =
  let go s = Set.insert (x, y) where (x : y : _) = read <$> splitBy ',' s
   in foldr go Set.empty

initFoldDir :: Char -> FoldDir
initFoldDir 'x' = FoldLeft
initFoldDir 'y' = FoldUp
initFoldDir c = eprintf "initFoldDir %c" c

initFolds :: [String] -> [Fold]
initFolds = map go
  where
    go l = (d, v)
      where
        (ds : vs : _) = splitBy '=' (words l !! 2)
        d = initFoldDir . head $ ds
        v = read vs

initFromInput :: [String] -> (DotMatrix, [Fold])
initFromInput ss =
  let (ps : fs : _) = splitBy "" ss
   in (initPoints ps, initFolds fs)

movePoint :: Point -> Point -> Action ()
movePoint p1 p2 = modify $ Set.insert p2 . Set.delete p1

foldPoint :: Fold -> Point -> Action ()
foldPoint (FoldLeft, x) p@(x1, y1) = when (x1 >= x) $ do
  let x2 = x - (x1 - x)
  movePoint p (x2, y1)
foldPoint (FoldUp, y) p@(x1, y1) = when (y1 >= y) $ do
  let y2 = y - (y1 - y)
  movePoint p (x1, y2)

fold :: Fold -> Action ()
fold f = get >>= mapM_ (foldPoint f)

part1 :: [String] -> Int
part1 inpt =
  let (st, fd : _) = initFromInput inpt
   in length . execState (fold fd) $ st

part2 :: [String] -> String
part2 inpt =
  let (st, folds) = initFromInput inpt
      actions = mapM fold folds
   in fromJust . parseLetters . execState actions $ st

soln :: Lib.Day
soln = Lib.Day 13 (show . part1 <$> readInput 13 1) (part2 <$> readInput 13 1)