module Main where

import Lib

import Text.Printf

runDay :: (Show a, Show b) => Int -> IO a -> IO b -> IO ()
runDay n partOne partTwo = do
	ansOne <- partOne
	ansTwo <- partTwo
	printf "Day %d Part 1: %s\n" n $ show ansOne
	printf "Day %d Part 2: %s\n" n $ show ansTwo


main :: IO ()
main = do
	runDay 1 Lib.day01_01 Lib.day01_02
	runDay 2 Lib.day02_01 Lib.day02_02

