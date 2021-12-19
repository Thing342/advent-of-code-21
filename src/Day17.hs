module Day17 where

import Control.Monad.State
import Lib

type TimeStep = Int

type Velocity = Int

type Position = Int

type Point = (Position, Position)

type Bounds = ((Int, Int), (Int, Int))

parse :: String -> Bounds
parse =
  let st = do
        expect "target area: x="
        xmin <- grabNum
        expect ".."
        xmax <- grabNum
        expect ", y="
        ymin <- grabNum
        expect ".."
        ymax <- grabNum
        return ((xmin, xmax), (ymin, ymax))
   in evalState st

inBounds :: Bounds -> Point -> Bool
inBounds (xb, yb) (x, y) = y `within` yb && x `within` xb

vys :: Velocity -> [Velocity]
vys = iterate (\y -> y - 1)

ys :: Bounds -> Velocity -> [Position]
ys (_, (y_min, _)) = takeWhile (>= y_min) . scanl (+) 0 . vys

vxs :: Velocity -> [Velocity]
vxs = iterate (\x -> x - signum x)

xs :: Velocity -> [Position]
xs = scanl (+) 0 . vxs

xtrajs :: Bounds -> [[Position]]
xtrajs b@((_, xmax), _) = map xs [0 .. xmax]

ytrajs :: Bounds -> [[Position]]
ytrajs b@(_, (ymin, ymax)) = filter (\ysn -> last ysn <= ymax) . map (ys b) $ [ymin .. abs ymin]

validTrajs :: Bounds -> [[Point]]
validTrajs b = do
  ys <- ytrajs b
  xs <- xtrajs b
  let traj = xs `zip` ys
  guard $ any (inBounds b) traj
  return traj

part1 :: String -> Int
part1 = maximize . map snd . last . validTrajs . parse

part2 :: String -> Int
part2 = length . validTrajs . parse

soln :: Lib.Day
soln = Lib.Day 17 (show . part1 . head <$> readInput 17 1) (show . part2 . head <$> readInput 17 1)