module Main where

import Control.Monad.Cont (mapM_)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import Lib
import Text.Printf

runDay :: Day -> IO ()
runDay Day {_daynum = d, _part1 = p1, _part2 = p2} = do
  ansOne <- p1
  ansTwo <- p2
  printf "=== DAY %02d ===\n" d
  printf "PART 1: %s\n" ansOne
  printf "PART 2: %s\n" ansTwo

main :: IO ()
main = do
  mapM_
    runDay
    [ Day01.soln,
      Day02.soln,
      Day03.soln,
      Day04.soln,
      Day05.soln,
      Day06.soln,
      Day07.soln,
      Day08.soln,
      Day09.soln,
      Day10.soln,
      Day11.soln,
      Day12.soln,
      Day13.soln,
      Day14.soln,
      Day15.soln,
      Day16.soln,
      Day17.soln,
      Day18.soln,
      Day19.soln
    ]