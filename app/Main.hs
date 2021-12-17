module Main where

import Lib
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

import Text.Printf

runDay :: Day -> IO ()
runDay Day {_daynum = d, _part1 = p1, _part2 = p2 } = do
    ansOne <- p1
    ansTwo <- p2
    printf "=== DAY %02d ===\n" d
    printf "PART 1: %s\n" ansOne
    printf "PART 2: %s\n" ansTwo


main :: IO ()
main = do
    runDay Day01.soln
    runDay Day02.soln
    runDay Day03.soln
    runDay Day04.soln
    runDay Day05.soln
    runDay Day06.soln
    runDay Day07.soln
    runDay Day08.soln
    runDay Day09.soln
    runDay Day10.soln
    runDay Day11.soln
    runDay Day12.soln
    runDay Day13.soln
    runDay Day14.soln
    runDay Day15.soln
    runDay Day16.soln