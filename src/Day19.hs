module Day19 where

import Control.Monad.State

import Data.Either (partitionEithers)
import Data.List (nub, permutations)
import Data.Maybe (listToMaybe)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vector (Vector)
import qualified Data.Vector as V

import Lib
import Parser

type Point3 = (Int, Int, Int)

pointSum :: Point3 -> Point3 -> Point3
pointSum (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

pointDist :: Point3 -> Point3 -> Point3
pointDist (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

l1Dist :: Point3 -> Point3 -> Int
l1Dist (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

orientations :: Vector Point3 -> Vector (Vector Point3)
orientations ps = V.fromList $ do
  [f1, f2, f3] <- permutations [\(a, _, _) -> a, \(_, b, _) -> b, \(_, _, c) -> c]
  [a, b, c] <- mapM (const [-1, 1]) [0, 1, 2]
  return $ V.map (\s -> (a * f1 s, b * f2 s, c * f3 s)) ps

data Scanner = Scanner Int Point3 (Vector Point3) deriving (Show)

instance Eq Scanner where
  a == b = scanId a == scanId b

beacons :: Scanner -> Vector Point3
beacons (Scanner _ _ bs) = bs

basis :: Scanner -> Point3
basis (Scanner _ bs _) = bs

scanId :: Scanner -> Int
scanId (Scanner i _ _) = i

overlapPoints :: Vector Point3 -> Vector Point3 -> Vector Point3
overlapPoints as bs = V.fromList . Map.keys . Map.filter (>= 12) . count $ pointDist <$> as <*> bs

orDefaultE :: Maybe a -> b -> Either b a
orDefaultE (Just k) _ = Right k
orDefaultE Nothing j = Left j

alignScanners :: Vector Scanner -> [Scanner] -> Vector Scanner -> Vector Scanner
alignScanners result _ v | null v = result
alignScanners result (anchor : anchors) floating =
  let (unknown, found) = V.partitionWith (\scanner -> vMaybe (possAlignments anchor scanner) `orDefaultE` scanner) floating
   in alignScanners (found V.++ result) (V.toList found ++ anchors) unknown
alignScanners _ _ _ = error "unreachable"

vMaybe :: Vector a -> Maybe a
vMaybe v | V.null v = Nothing
vMaybe v = Just $ V.head v

possAlignments :: Scanner -> Scanner -> Vector Scanner
possAlignments (Scanner scanid _ as) (Scanner _ _ bs) = do
  obs <- orientations bs
  basis <- overlapPoints as obs
  let ps = V.map (pointSum basis) obs
  return $ Scanner scanid basis ps

parseScannerPoint :: Parser Point3
parseScannerPoint = do
  x <- grabNum
  expect ","
  y <- grabNum
  expect ","
  z <- grabNum
  expect "\n"
  return (x, y, z)

parseScanner :: Parser Scanner
parseScanner = do
  expect "--- scanner "
  scanid <- grabNum
  expect " ---\n"
  let loop = do
        peek <- gets listToMaybe
        case peek of
          Nothing -> return []
          Just '\n' -> return []
          _ -> do
            point <- parseScannerPoint
            ps <- loop
            return $ point : ps
  Scanner scanid (0, 0, 0) . V.fromList <$> loop

parseScanners :: String -> [Scanner]
parseScanners = runParser parser
  where
    parser = do
      isFinished <- gets null
      if isFinished
        then return []
        else do
          scanner <- parseScanner
          expect "\n"
          rest <- parser
          return $ scanner : rest

solve :: [String] -> Vector Scanner
solve ss =
  let (base : scanners) = parseScanners . unlines $ ss
   in alignScanners (V.singleton base) [base] (V.fromList scanners)

part1 :: [String] -> Int
part1 = length . nub . V.toList . V.concatMap beacons . solve

part2 :: [String] -> Int
part2 ss =
  let res = V.toList $ solve ss
   in maximize [l1Dist (basis s1) (basis s2) | s1 <- res, s2 <- res]

soln :: Lib.Day
soln = Lib.Day 19 (show . part1 <$> readInput 19 1) (show . part2 <$> readInput 19 1)