module Day16 where

import Control.Monad.State
import Data.Bits
import Data.Char (digitToInt)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Lib

data Packet = Packet
  { _version :: Int,
    _type :: Int,
    _data :: PacketData
  }
  deriving (Show)

data PacketData = Literal Int | Operator Op [Packet] deriving (Show)

data Op = Sum | Prod | Min | Max | Lit | GreaterThan | LessThan | EqualTo deriving (Show, Enum)

readHex :: Char -> [Bool]
readHex 'F' = [True, True, True, True]
readHex 'E' = [True, True, True, False]
readHex 'D' = [True, True, False, True]
readHex 'C' = [True, True, False, False]
readHex 'B' = [True, False, True, True]
readHex 'A' = [True, False, True, False]
readHex '9' = [True, False, False, True]
readHex '8' = [True, False, False, False]
readHex '7' = [False, True, True, True]
readHex '6' = [False, True, True, False]
readHex '5' = [False, True, False, True]
readHex '4' = [False, True, False, False]
readHex '3' = [False, False, True, True]
readHex '2' = [False, False, True, False]
readHex '1' = [False, False, False, True]
readHex '0' = [False, False, False, False]
readHex _ = error "readHex"

fromInput :: String -> [Bool]
fromInput = concatMap readHex

bits2Int :: [Bool] -> Int
bits2Int bs = foldr (\(bt, i) b -> if bt then setBit b i else b) 0 (reverse bs `zip` [0 ..])

type ParseSt = [Bool]

grabBits :: Int -> State ParseSt [Bool]
grabBits n = do
  st <- get
  let (bs, rs) = splitAt n st
  put rs
  return bs

grab :: Int -> State ParseSt Int
grab n = do
  bs <- grabBits n
  return $ bits2Int bs

literal :: State ParseSt [Bool]
literal = do
  sig <- grab 1
  bs <- grabBits 4
  if sig == 1
    then do
      rest <- literal
      return $ bs ++ rest
    else return bs

operatorLength :: State ParseSt [Packet]
operatorLength = do
  len <- grab 15
  subpackBits <- grabBits len
  return $ readPacks subpackBits

operatorCount :: State ParseSt [Packet]
operatorCount = do
  num <- grab 11
  mapM (const parse) [1 .. num]

parse :: State ParseSt Packet
parse = do
  version <- grab 3
  packType <- grab 3
  packData <- case toEnum packType of
    Lit -> Literal . bits2Int <$> literal
    op -> do
      ltid <- grab 1
      Operator op <$> (if ltid == 1 then operatorCount else operatorLength)
  return $ Packet version packType packData

readPacks :: [Bool] -> [Packet]
readPacks bs =
  let (p, bs') = runState parse bs
   in case bs' of
        [] -> [p]
        bs' -> p : readPacks bs'

sumVersion :: Packet -> Int
sumVersion p =
  _version p + case _data p of
    Literal _ -> 0
    Operator _ subpacks -> sum . map sumVersion $ subpacks

evalPacket :: Packet -> Int
evalPacket p = case _data p of
  Literal x -> x
  Operator Sum subs -> sum . map evalPacket $ subs
  Operator Prod subs -> product . map evalPacket $ subs
  Operator Min subs -> minimize . map evalPacket $ subs
  Operator Max subs -> maximize . map evalPacket $ subs
  Operator GreaterThan [s1, s2] -> if evalPacket s1 > evalPacket s2 then 1 else 0
  Operator LessThan [s1, s2] -> if evalPacket s1 < evalPacket s2 then 1 else 0
  Operator EqualTo [s1, s2] -> if evalPacket s1 == evalPacket s2 then 1 else 0
  _ -> error "literal operator ???"

part1 :: [String] -> Int
part1 = sumVersion . evalState parse . fromInput . head

part2 :: [String] -> Int
part2 = evalPacket . evalState parse . fromInput . head

soln :: Lib.Day
soln = Lib.Day 16 (show . part1 <$> readInput 16 1) (show . part2 <$> readInput 16 1)
