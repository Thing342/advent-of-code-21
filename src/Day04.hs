{-# LANGUAGE TupleSections #-}
module Day04 (soln) where

import Text.Printf
import Debug.Trace
import Data.Bits

import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Set (Set, member)
import qualified Data.Set as Set
import Data.List

import Lib
import Control.Monad (filterM)

type Matrix a = Vector (Vector a)
type Nums = Set Int

data BingoCard = BingoCard {
    _cardId      :: Int,
    _squares :: Matrix Int
} deriving (Show)

instance Eq BingoCard where
    (==) a b = (_cardId a) == (_cardId b)
    (/=) a b = (_cardId a) /= (_cardId b)

initData :: [String] -> ([Int], [BingoCard])
initData (s1:ss) = let
    numbers = read $ printf "[%s]" s1
    bingos = initBingos ss
    in (numbers, bingos)
initData ss = eprintf "initData %s" (show ss)

initBingos :: [String] -> [BingoCard]
initBingos ss = (\(i,sss) -> initBingo i (tail sss)) <$> [1..] `zip` (6 `chunks` ss)

initBingo :: Int -> [String] -> BingoCard
initBingo i ss = let
    initLine s = V.fromList [ read z :: Int | z <- words s ]
    squares = V.fromList [ initLine s | s <- take 5 ss ]
    in BingoCard i squares

bingoScore :: Nums -> BingoCard -> Int
bingoScore nums card = let
    vsum = V.foldr (+) 0
    vsumIf f v = vsum $ V.filter f v
    notMarked n = not $ n `member` nums
    in vsum $ vsumIf notMarked <$> _squares card

hasBingoRow :: Nums -> BingoCard -> Int -> Bool
hasBingoRow nums card row = V.all (`member` nums) $ _squares card ! row

hasBingoCol :: Nums -> BingoCard -> Int -> Bool
hasBingoCol nums card col = V.all (\row -> (row ! col) `member` nums) $ _squares card

hasBingo :: Nums -> BingoCard -> Bool
hasBingo nums card = or [ hasBingoRow nums card i || hasBingoCol nums card i | i <- [0..4] ]


findWinners :: [Int] -> Vector BingoCard -> [(Nums, BingoCard, Int)]
findWinners numbers cards = let
    first = (Set.empty, cards, numbers)

    next (called, cards, []) = Nothing
    next (called, cards, n:nums) = let
        ncalled = Set.insert n called
        winners = V.toList . V.map (ncalled,, n) . V.filter (hasBingo ncalled) $ cards
        iswinner card = any (\(_, c, _) -> card == c) winners
        ncards = V.filter (not . iswinner) cards
        in Just (winners, (ncalled, ncards, nums))

    in concat $ unfoldr next first

part1pure :: [Int] -> [BingoCard] -> (BingoCard, Int, Int)
part1pure gen cards = let
    (drawn, winner, winningNum) = head $ findWinners gen (V.fromList cards)
    score = bingoScore drawn winner
    in (winner, score, winningNum)

part2pure :: [Int] -> [BingoCard] -> (BingoCard, Int, Int)
part2pure gen cards = let
    (drawn, winner, winningNum) = last $ findWinners gen (V.fromList cards)
    score = bingoScore drawn winner
    in (winner, score, winningNum)

part1 :: IO Int
part1 = do
    inpt <- readInput 4 1
    let (gen, cards) = initData inpt
    let (winner, score, winningNum) = part1pure gen cards
    return $ winningNum * score

part2 :: IO Int
part2 = do
    inpt <- readInput 4 1
    let (gen, cards) = initData inpt
    let (winner, score, winningNum) = part2pure gen cards
    return $ winningNum * score

soln :: Lib.Day
soln = Lib.Day 4 part1 part2