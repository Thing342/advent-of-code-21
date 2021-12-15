module Day11 (soln) where

import Control.Monad.State
import Data.Char (digitToInt)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Lib
import Matrix (Matrix, Point, (!))
import qualified Matrix as M

type Octopus = Int

type Day11State = Matrix Octopus

showSt :: Day11State -> String
showSt = unlines . V.toList . V.map show

neighbors :: Matrix a -> Point -> Vector Point
neighbors m (r, c) = V.fromList $ do
  dr <- [-1, 0, 1]
  dc <- [-1, 0, 1]
  let dp = (r + dr, c + dc)
  guard $ M.inbounds dp m
  guard $ dp /= (r, c)
  return dp

readMat :: [String] -> Matrix Octopus
readMat = V.fromList . map (V.fromList . map digitToInt)

getOcto :: Day11State -> Point -> Octopus
getOcto st p = case st ! p of
  Just o -> o
  Nothing -> eprintf "octo st"

setOctopus :: Point -> Octopus -> State Day11State ()
setOctopus p o = modify $ \st -> M.update_ st p o

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
  if st `getOcto` p > 9
    then do
      setOctopus p 0
      return 1
    else return 0

gameStep :: State Day11State Int
gameStep = do
  idx <- gets M.idx
  mapM_ incrementOctopus idx
  flashs <- mapM resetOctopus idx
  return . V.sum $ flashs

part1 :: Int -> [String] -> Int
part1 n inpt =
  let actions = replicateM n gameStep
      flashes = evalState actions (readMat inpt)
   in sum flashes

doPart2 :: Int -> State Day11State Int
doPart2 n = do
  flashes <- gameStep
  mz <- gets M.size
  if flashes == mz
    then return n
    else doPart2 (n + 1)

part2 :: [String] -> Int
part2 inpt = evalState (doPart2 1) (readMat inpt)

soln :: Lib.Day
soln = Lib.Day 11 (show . part1 100 <$> readInput 11 1) (show . part2 <$> readInput 11 1)
