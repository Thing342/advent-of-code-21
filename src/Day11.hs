module Day11 (soln) where

import Data.Char (digitToInt, GeneralCategory (Control))

import Control.Monad.State

import Lib 
import Matrix (Matrix, Point, (!))
import qualified Matrix as M

import qualified Data.Vector as V
import Data.Vector (Vector)

import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Data.List (intersperse)
import qualified Control.Monad

data Octopus = Octopus {
    _pos :: Point,
    _level :: Int
} deriving (Show)

data Day11State = Day11State {
    _octopi :: Matrix Octopus,
    _flashes :: Int
} deriving (Show)

showSt :: Day11State -> String
showSt = unlines . V.toList . V.map show . M.map _level . _octopi

inc :: Octopus -> Octopus
inc o = Octopus { _level = _level o + 1 , _pos = _pos o}

neighbors :: Matrix a -> Point -> Vector Point
neighbors m (r,c) = V.fromList $ do
    dr <- [-1,0,1]
    dc <- [-1,0,1]
    let dp = (r+dr,c+dc)
    guard $ M.inbounds dp m
    guard $ dp /= (r,c)
    return dp

readMat :: [String] -> Matrix Octopus
readMat ss = V.fromList [ V.fromList [ Octopus (r,c) (digitToInt x) | (x,c) <- s `zip` [0..length s - 1]] | (s,r) <- ss `zip` [0..length ss - 1]]

day11State :: [String] -> Day11State
day11State ss = Day11State {
    _octopi = readMat ss,
    _flashes = 0
}

getOcto :: Day11State -> Point -> Octopus
getOcto st p = case _octopi st ! p of
    Just o  -> o
    Nothing -> eprintf "octo st"

setOctopus :: Octopus -> State Day11State ()
setOctopus o = do
    st <- get
    let nos = M.update_ (_octopi st) (_pos o) o
    put st { _octopi = nos }

incrementOctopus :: Point -> State Day11State ()
incrementOctopus p = do
    st <- get
    let o = st `getOcto` p
    let os = _octopi st
    let l = _level o + 1
    setOctopus o {_level = l}

    when (l == 10) $ 
        mapM_ incrementOctopus . neighbors os $ p

resetOctopus :: Point -> State Day11State Int
resetOctopus p = do
    st <- get
    let o = st `getOcto` p
    case _level o of
        l | l > 9 -> do 
            setOctopus o {_level = 0}
            return 1
        _ -> return 0

gameStep :: State Day11State ()
gameStep = do
    st <- get

    let idx = M.idx $ _octopi st
    mapM_ incrementOctopus idx
    flashs <- mapM resetOctopus idx

    st <- get
    put st { _flashes = _flashes st + V.sum flashs }
    return ()

part1 :: Int -> [String] -> Int
part1 n inpt = let
    actions = replicateM n gameStep
    (_,out) = runState actions (day11State inpt)
    in _flashes out

doPart2 :: Int -> State Day11State Int
doPart2 n = do
    gameStep
    st <- get
    if M.all (\o -> _level o == 0) $ _octopi st
        then return n
        else doPart2 (n+1)

part2 :: [String] -> Int
part2 inpt = evalState (doPart2 1) (day11State inpt)

soln :: Lib.Day
soln = Lib.Day 11 (part1 100 <$> readInput 11 1) (part2 <$> readInput 11 1)
