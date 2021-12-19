module Day18 (soln) where

import Control.Monad.State
import Data.Char (isDigit)
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

import Lib
import Parser

data SnailfishNum = Pair SnailfishNum SnailfishNum | Literal Int deriving (Show, Eq)

fromLiteral :: SnailfishNum -> Int
fromLiteral (Literal l) = l
fromLiteral _ = error "FromLiteral called on Pair!"

-- PARSE --

literal :: Parser SnailfishNum
literal = Literal <$> grabNum

pair :: Parser SnailfishNum
pair = do
  expect "["
  first <- parse
  expect ","
  second <- parse
  expect "]"
  return $ Pair first second

parse :: Parser SnailfishNum
parse = do
  cs <- get
  case head cs of
    '[' -> pair
    c | isDigit c -> literal
    c -> error "Expected digit or open pair '['"

parseSnailfish :: String -> SnailfishNum
parseSnailfish = evalState parse

-- EVAL --

addL :: Int -> SnailfishNum -> SnailfishNum
addL rval (Literal l) = Literal (l + rval)
addL rval (Pair l r) = Pair (addL rval l) r

addR :: SnailfishNum -> Int -> SnailfishNum
addR (Literal r) lval = Literal (r + lval)
addR (Pair l r) lval = Pair l (addR r lval)

explode :: Int -> SnailfishNum -> Maybe (Int, SnailfishNum, Int)
explode d (Literal l) = Nothing
explode d (Pair l r) | d >= 4 = Just (fromLiteral l, Literal 0, fromLiteral r)
explode d (Pair l r) = case explode (d + 1) l of
  Just (lval, l', rval) -> Just (lval, Pair l' (addL rval r), 0)
  Nothing -> case explode (d + 1) r of
    Just (lval, r', rval) -> Just (0, Pair (addR l lval) r', rval)
    Nothing -> Nothing

split :: SnailfishNum -> Maybe SnailfishNum
split (Literal n) | n >= 10 = let (q, r) = n `divmod` 2 in Just $ Pair (Literal q) (Literal (q + r))
split (Literal n) = Nothing
split (Pair l r) = case split l of
  Just l' -> Just $ Pair l' r
  Nothing -> case split r of
    Just r' -> Just $ Pair l r'
    Nothing -> Nothing

reduce :: SnailfishNum -> SnailfishNum
reduce sn = case explode 0 sn of
  Just (_, sn', _) -> reduce sn'
  Nothing -> maybe sn reduce (split sn)

snailfishAdd :: SnailfishNum -> SnailfishNum -> SnailfishNum
snailfishAdd l r = reduce $ Pair l r

magnitude :: SnailfishNum -> Int
magnitude (Literal l) = l
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

part1 :: [String] -> Int
part1 = magnitude . foldl1 snailfishAdd . map parseSnailfish

part2 :: [String] -> Int
part2 ss = maximize $ do
  let sns = parseSnailfish <$> ss
  sn1 <- sns
  sn2 <- sns
  guard (sn1 /= sn2)
  return . magnitude $ snailfishAdd sn1 sn2

soln :: Lib.Day
soln = Lib.Day 18 (show . part1 <$> readInput 18 1) (show . part2 <$> readInput 18 1)
