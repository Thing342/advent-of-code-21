module Day15 where

import Data.Array.Unboxed
import Data.Set (Set)
import qualified Data.Set as Set
import Lib

type Grid = Array (Int, Int) Int

type Point = (Int, Int)

type Risk = (Int, Point)

type Bounds = (Point, Point)

makeGrid :: [String] -> (Grid, Int, Int)
makeGrid ss =
  let is = map (map (read . pure)) ss
      w = length (head is)
      h = length is
   in (listArray ((0, 0), (h - 1, w - 1)) $ concat is, w, h)

makeBigGrid :: (Grid, Int, Int) -> Grid
makeBigGrid (grid, w, h) = array ((0, 0), (h * 5 - 1, w * 5 - 1)) $ do
  row <- [0 .. 4]
  col <- [0 .. 4]
  ((y, x), r) <- assocs grid
  return ((y + h * row, x + w * col), (r - 1 + row + col) `mod` 9 + 1)

search' :: Grid -> Bounds -> Set Point -> Set Risk -> Int
search' grid bounds visited fringe = case Set.deleteFindMin fringe of
  ((risk, i), next) | i == snd bounds -> risk
  ((risk, i), next) | Set.member i visited -> search' grid bounds visited next
  ((risk, i), next) ->
    let risk' x = (risk + (grid ! x), x)
        visited' = Set.insert i visited
        fringe' = foldr (Set.insert . risk') next (adj bounds i)
     in search' grid bounds visited' fringe'

adj :: Bounds -> Point -> [Point]
adj ((y1, x1), (y2, x2)) (y, x) =
  filter
    (\(y', x') -> y1 <= y' && y' <= y2 && x1 <= x' && x' <= x2)
    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]

solve :: Grid -> Int
solve grid = search' grid (bounds grid) Set.empty (Set.singleton (0, (0, 0)))

part1 :: [String] -> Int
part1 ss = let (grid, _, _) = makeGrid ss in solve grid

part2 :: [String] -> Int
part2 ss =
  let grid = makeBigGrid . makeGrid $ ss
   in solve grid

soln :: Lib.Day
soln = Lib.Day 15 (show . part1 <$> readInput 15 1) (show . part2 <$> readInput 15 1)
