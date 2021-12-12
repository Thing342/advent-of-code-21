module Day11 (soln) where

import Data.Char (digitToInt)

import Control.Monad.State

import Lib 
import Matrix (Matrix, Point, (!))
import qualified Matrix as M

import qualified Data.Vector as V
import Data.Vector (Vector)

type Octopus = Int
type Day11State = Matrix Octopus

showSt :: Day11State -> String
showSt = unlines . V.toList . V.map show

neighbors :: Matrix a -> Point -> Vector Point
neighbors m (r,c) = V.fromList $ do
    dr <- [-1,0,1]
    dc <- [-1,0,1]
    let dp = (r+dr,c+dc)
    guard $ M.inbounds dp m
    guard $ dp /= (r,c)
    return dp

readMat :: [String] -> Matrix Octopus
readMat ss = V.fromList [ V.fromList [ digitToInt x | (x,c) <- s `zip` [0..length s - 1]] | (s,r) <- ss `zip` [0..length ss - 1]]

day11State :: [String] -> Day11State
day11State = readMat

getOcto :: Day11State -> Point -> Octopus
getOcto st p = case st ! p of
    Just o  -> o
    Nothing -> eprintf "octo st"

setOctopus :: Point -> Octopus -> State Day11State ()
setOctopus p o = do
    st <- get
    let nos = M.update_ st p o
    put nos

incrementOctopus :: Point -> State Day11State ()
incrementOctopus p = do
    st <- get
    let o = st `getOcto` p
    let l = o + 1
    setOctopus p l

    when (l == 10) $ 
        mapM_ incrementOctopus . neighbors st $ p

resetOctopus :: Point -> State Day11State Int
resetOctopus p = do
    st <- get
    let o = st `getOcto` p
    case o of
        l | l > 9 -> do 
            setOctopus p 0
            return 1
        _ -> return 0

gameStep :: State Day11State Int
gameStep = do
    st <- get

    let idx = M.idx st
    mapM_ incrementOctopus idx
    flashs <- mapM resetOctopus idx

    st <- get
    return . V.sum $ flashs

part1 :: Int -> [String] -> Int
part1 n inpt = let
    actions = replicateM n gameStep
    flashes = evalState actions (day11State inpt)
    in sum flashes

doPart2 :: Int -> State Day11State Int
doPart2 n = do
    flashes <- gameStep
    st <- get
    if flashes == M.size st
        then return n
        else doPart2 (n+1)

part2 :: [String] -> Int
part2 inpt = evalState (doPart2 1) (day11State inpt)

soln :: Lib.Day
soln = Lib.Day 11 (part1 100 <$> readInput 11 1) (part2 <$> readInput 11 1)
