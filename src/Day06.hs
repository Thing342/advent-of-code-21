module Day06 (soln) where

import Text.Printf
import Debug.Trace
import Data.Bits
import Data.List

import Lib

data FishWeek = FishWeek {
    _monday :: Int,
    _tuesday :: Int,
    _wednesday :: Int,
    _thursday :: Int,
    _friday :: Int,
    _saturday :: Int,
    _sunday :: Int,
    _next_monday :: Int,
    _next_tuesday :: Int
} deriving (Show)

initFish :: String -> FishWeek
initFish cs = let
    fishdays = read <$> splitBy ',' cs
    zero = FishWeek 0 0 0 0 0 0 0 0 0
    in foldr initFishRead zero fishdays

initFishRead :: Int -> FishWeek -> FishWeek
initFishRead 0 fw = fw {_monday = _monday fw + 1}
initFishRead 1 fw = fw {_tuesday = _tuesday fw + 1}
initFishRead 2 fw = fw {_wednesday = _wednesday fw + 1}
initFishRead 3 fw = fw {_thursday = _thursday fw + 1}
initFishRead 4 fw = fw {_friday = _friday fw + 1}
initFishRead 5 fw = fw {_saturday = _saturday fw + 1}
initFishRead 6 fw = fw {_sunday = _sunday fw + 1}
initFishRead 7 fw = fw {_next_monday = _next_monday fw + 1}
initFishRead 8 fw = fw {_next_tuesday = _next_tuesday fw + 1}
initFishRead n fw = eprintf "initFishRead %d %s" n fw

fishGrowthWeek :: FishWeek -> FishWeek
fishGrowthWeek (FishWeek mon tues weds thurs fri sat sun nextmon nexttues) = FishWeek {
    _monday = mon + nextmon,
    _tuesday = tues + nexttues,
    _wednesday = weds + mon,
    _thursday = thurs + tues,
    _friday = fri + weds,
    _saturday = sat + thurs,
    _sunday = sun + fri,
    _next_monday = sat,
    _next_tuesday = sun
}

fishTotal :: Int -> FishWeek -> Int
fishTotal day fw@(FishWeek mon tues weds thurs fri sat sun nextmon nexttues) = let 
    base = mon + tues + weds + thurs + fri + sat + sun + nextmon + nexttues
    adj = case day of
        0 -> 0
        1 -> mon
        2 -> mon + tues
        3 -> mon + tues + weds
        4 -> mon + tues + weds + thurs
        5 -> mon + tues + weds + thurs + fri
        6 -> mon + tues + weds + thurs + fri + sat
        _ -> eprintf "fishTotal %d %s" day fw
    in base + adj

simulate :: Int -> FishWeek -> FishWeek
simulate n fish = foldr (\_ f -> fishGrowthWeek f) fish [1..n]

solve :: Int -> IO Int
solve n = do
    inpt <- readInput 6 1
    let fish = initFish $ head inpt
    let (weeks,days) = n `divmod` 7
    let fish' = simulate weeks fish
    return $ fishTotal days fish'

soln :: Lib.Day
soln = Lib.Day 6 (solve 80) (solve 256)