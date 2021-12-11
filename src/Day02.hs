module Day02 (soln) where

import Text.Printf
import Debug.Trace

import Lib

type State = (Int, Int, Int)
type Cmd = (String, Int)

zero :: State
zero = (0, 0, 0)

getCmd :: String -> Cmd
getCmd line = let
    ws = words line
    v = read $ (head . tail) ws
    in (head ws, v)

readCmd :: Bool ->  State -> Cmd -> State
readCmd False (x,y,aim) ("forward", v) = (x + v, y, aim)
readCmd False (x,y,aim) ("up", v)  = (x, y - v, aim)
readCmd False (x,y,aim) ("down", v) = (x, y + v, aim)
readCmd True (x,y,aim) ("forward", v) = (x + v, y + (v * aim), aim)
readCmd True (x,y,aim) ("up", v) = (x, y, aim - v)
readCmd True (x,y,aim)("down", v) = (x, y, aim + v)
readCmd p2 st cmd = eprintf "readCmd %s %s %s" (show p2) st cmd

day02_01_pure :: State -> [Cmd] -> State
day02_01_pure = foldl (readCmd False)

part1 :: IO Int
part1 = do
    ss <- readInput 2 1
    let cmds = map getCmd ss
    let (x,y,aim) = day02_01_pure zero cmds
    return $ x * y

day02_02_pure :: State -> [Cmd] -> State
day02_02_pure = foldl (readCmd True)

part2 :: IO Int
part2 = do
    ss <- readInput 2 1
    let cmds = map getCmd ss
    let (x,y,aim) = day02_02_pure zero cmds
    return $ x * y

soln :: Lib.Day
soln = Lib.Day {_daynum = 2, _part1 = part1, _part2 = part2}