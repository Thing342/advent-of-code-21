module Day12 where

import Control.Monad.State
import Data.Char (isLower)
import Data.Map (Map, (!), notMember)
import qualified Data.Map as Map
import Lib

type GraphMap k = Map k [k]
type Path = Map String Int

graphFromInput :: [String] -> GraphMap String
graphFromInput =
  let go l = Map.insertWith (++) to [from] . Map.insertWith (++) from [to]
        where (to : from : _) = splitBy '-' l
   in foldr go Map.empty

isSmall :: String -> Bool
isSmall = isLower . head

isLarge :: String -> Bool
isLarge = not . isSmall

isValid :: Int -> String -> Path -> Bool
isValid n node path =
  isLarge node
    || node `notMember` path
    || node /= "start" && anyWithKey (\k v -> isSmall k && v >= n) path

walk :: Int -> GraphMap String -> Path -> String -> State Int ()
walk _ _ _ "end" = modify (+ 1)
walk n graph path node = when (isValid n node path) $ do
  let path' = Map.insertWith (+) node 1 path
  mapM_ (walk n graph path') (graph ! node)

solve :: Bool -> [String] -> Int
solve p2 inpt =
  let caves = graphFromInput inpt
      action = walk (if p2 then 2 else 1) caves Map.empty "start"
   in execState action 0

soln :: Lib.Day
soln = Lib.Day 12 (show . solve False <$> readInput 12 1) (show . solve True <$> readInput 12 1)