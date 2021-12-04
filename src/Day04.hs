module Day04 where

import Text.Printf
import Debug.Trace
import Data.Bits

import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as Set

import Lib

type Matrix a = Vector (Vector a)
type Nums = Set Int

data BingoCard = BingoCard {
    _cardId      :: Int,
    _squares :: Matrix Int
} deriving (Show)

instance Eq BingoCard where
    (==) a b = (_cardId a) == (_cardId b)
    (/=) a b = (_cardId a) /= (_cardId b)

index :: Matrix a -> Int -> Int -> a
index mat r c = let
    row = mat ! r
    in row ! c

matseq :: Matrix a -> [a]
matseq mat = concat (V.toList <$> mat)

initData :: [String] -> ([Int], [BingoCard])
initData (s1:ss) = let
    numbers = read $ printf "[%s]" s1
    bingos = initBingos 0 ss
    in (numbers, bingos)

initBingos :: Int -> [String] -> [BingoCard]
initBingos _ [] = []
initBingos i ss = let
    sss = tail ss -- drop whitespace
    (cardlines, restlines) = splitAt 5 sss
    restcards = initBingos (i+1) restlines
    card = initBingo i cardlines
    in card : restcards

initBingo :: Int -> [String] -> BingoCard
initBingo i ss = let 
    squares = V.fromList [ initBingoLine s | s <- take 5 ss ]
    in BingoCard i squares

initBingoLine :: String -> V.Vector Int
initBingoLine s = V.fromList [ read z :: Int | z <- words s ]

bingoScore :: Nums -> BingoCard -> Int
bingoScore nums card = sum $ filter (\n -> not $ n `Set.member` nums) $ matseq (_squares card)

hasBingoRow :: Nums -> BingoCard -> Int -> Bool
hasBingoRow nums card row = V.all (\i -> Set.member i nums) $ (_squares card) ! row

hasBingoCol :: Nums -> BingoCard -> Int -> Bool
hasBingoCol nums card col = V.all (\row -> Set.member (row ! col) nums) $ _squares card

hasBingo :: Nums -> BingoCard -> Bool
hasBingo nums card = any id [ (hasBingoRow nums card i) || (hasBingoCol nums card i) | i <- [0..4] ]

findWinner :: Nums -> Vector BingoCard -> [Int]-> (Nums, BingoCard, Int)
findWinner nums cards (i:is) = let
    nnums = Set.insert i nums
    in case (V.find (hasBingo nnums) cards) of
        Just card -> (nnums, card, i)
        Nothing   -> findWinner nnums cards is

findWinners :: Nums -> Vector BingoCard -> [Int] -> [(Nums, BingoCard, Int)]
findWinners _ _ [] = []
findWinners nums cards (i:is) = let
    nnums = Set.insert i nums
    
    winners = V.toList $ V.map (\c -> (nnums, c, i)) $ V.filter (hasBingo nnums) cards
    iswinner card = any (\(_, c, _) -> card == c) winners

    ncards = V.filter (not .iswinner) cards
    rest = findWinners nnums ncards is
    in winners ++ rest

part1pure :: [Int] -> [BingoCard] -> (BingoCard, Int, Int)
part1pure gen cards = let
    (drawn, winner, winningNum) = findWinner Set.empty (V.fromList cards) gen
    score = bingoScore drawn winner
    in (winner, score, winningNum)

part2pure :: [Int] -> [BingoCard] -> (BingoCard, Int, Int)
part2pure gen cards = let
    (drawn, winner, winningNum) = last $ findWinners Set.empty (V.fromList cards) gen
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
soln = Lib.Day {_daynum = 4, _part1 = part1, _part2 = part2}