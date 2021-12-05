module Main where

import Lib
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05

import Text.Printf

runDay :: Day -> IO ()
runDay Day {_daynum = d, _part1 = p1, _part2 = p2 } = do
    ansOne <- p1
    ansTwo <- p2
    printf "Day %d Part 1: %s\n" d $ show ansOne
    printf "Day %d Part 2: %s\n" d $ show ansTwo


main :: IO ()
main = do
    runDay Day01.soln
    runDay Day02.soln
    runDay Day03.soln
    runDay Day04.soln
    runDay Day05.soln
