module Main where

import Lib
import qualified Day01
import qualified Day02
import qualified Day03

import Text.Printf

runDayOld :: (Show a, Show b) => Int -> IO a -> IO b -> IO ()
runDayOld n partOne partTwo = do
    ansOne <- partOne
    ansTwo <- partTwo
    printf "Day %d Part 1: %s\n" n $ show ansOne
    printf "Day %d Part 2: %s\n" n $ show ansTwo

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
    --runDay 2 Lib.day02_01 Lib.day02_02
    --runDay 3 Lib.day03_01 Lib.day03_02
