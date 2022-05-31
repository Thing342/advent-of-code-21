module Day21 where

import Control.Monad.State
import Debug.Trace
import Lib
import Data.Maybe (isJust)

detDice :: Int -> [Int]
detDice n = cycle [1 .. n]

data Game = Game {
  _p1score :: Int,
  _p1space :: Int,
  _p2score :: Int,
  _p2space :: Int,
  _dice    :: [Int],
  _rolls   :: Int
}

data GameResult = P1Win | P2Win

newSpace :: Int -> Int -> Int
newSpace sp roll = let
  b = (sp + roll) `mod` 10
  in if b == 0 then 10 else b

-- State Actions --

roll :: Int -> State Game [Int]
roll n = do
  st <- get
  let dice = _dice st
  let rolls = _rolls st
  let (d, dice') = splitAt n dice
  put (st { _dice = dice', _rolls = rolls + n })
  return d

p1Turn :: State Game (Maybe GameResult)
p1Turn = do
  ds <- roll 3
  st <- get
  let s = sum ds
  let space' = newSpace (_p1space st) s
  let score' = _p1score st + space'
  put (st { _p1score = score', _p1space = space' })
  return $ trace ("Player 1 rolls " ++ show ds ++ " and moves to space " ++ show space' ++ " for a total score of " ++ show score') (if score' >= 1000 then Just P1Win else Nothing)

p2Turn :: State Game (Maybe GameResult)
p2Turn = do
  ds <- roll 3
  st <- get
  let s = sum ds
  let space' = newSpace (_p2space st) s
  let score' = _p2score st + space'
  put (st { _p2score = score', _p2space = space' })
  return $ trace ("Player 2 rolls " ++ show ds ++ " and moves to space " ++ show space' ++ " for a total score of " ++ show score') (if score' >= 1000 then Just P2Win else Nothing)

gameLoop :: State Game GameResult
gameLoop = do
  p1res <- p1Turn
  case p1res of
    Just r -> return r
    Nothing -> do
      p2res <- p2Turn
      maybe gameLoop return p2res


newtype Prob = Prob Game

readProb :: [String] -> Prob
readProb (p1:p2:_) =
  let p1st = (read $ drop 28 p1) :: Int 
      p2st = (read $ drop 28 p2) :: Int
   in Prob $ Game 0 p1st 0 p2st (detDice 100) 0
readProb _ = error "bad input"

part1 :: Prob -> Int
part1 (Prob gs) = debugval (loserScore gs') * debugval (_rolls gs') where
  (res, gs') = runState gameLoop gs
  loserScore = case res of
    P1Win -> _p2score
    P2Win -> _p1score

part2 :: Prob -> Int
part2 (Prob gs) = 0

soln :: Lib.Day
soln = do Lib.Day 21 (show . part1 . readProb <$> testInput 21 1) (show . part2 . readProb <$> readInput 21 1)