module Matrix where

import Data.Vector (Vector,(!))
import qualified Data.Vector as V
import Control.Monad (guard)

type Matrix a = Vector (Vector a)
type Point = (Int, Int)

row :: Matrix a -> Int -> Vector a
m `row` r = m V.! r

col :: Matrix a -> Int -> Vector a
m `col` c = V.fromList [m V.! r V.! c | r <- [0..Matrix.length m - 1]]

length :: Matrix a -> Int
length = V.length

width :: Matrix a -> Int
width m = V.length . row m $ 0

inbounds :: Point -> Matrix a -> Bool
inbounds (r, c) m = 0 <= r && r < Matrix.length m && 0 <= c && c < width m

(!) :: Matrix a -> Point -> Maybe a
m ! p
    | inbounds p m = Just $ getNoBounds m p
    | otherwise = Nothing

getNoBounds :: Matrix a -> Point -> a
getNoBounds m (r,c) = m V.! r V.! c

rowIdx :: Matrix a -> Vector Int
rowIdx m = V.fromList [0..Matrix.length m - 1]

colIdx :: Matrix a -> Vector Int
colIdx m = V.fromList [0..Matrix.width m - 1]

idx :: Matrix a -> Vector Point
idx m = do
    r <- rowIdx m
    c <- colIdx m
    return (r,c)

update_ :: Matrix a -> Point -> a -> Matrix a
update_ m (r,c) x = let
    rw' = m `row` r V.// [(c, x)]
    in m V.// [(r, rw')]

update :: Matrix a -> Vector (Point,a) -> Matrix a
update m ps = V.update m $ do
    (r,rw) <-  rowIdx m `V.zip` m
    let updates = do
            p@((rp,cp), x) <- ps
            guard (rp == r)
            return (cp, x)
    let updated = V.update rw updates
    return (r, updated)

updateWith :: Matrix a -> Vector (Point,a -> a) -> Matrix a
updateWith m ps = V.update m $ do
    (r,rw) <-  rowIdx m `V.zip` m
    let updates = do
            p@((rp,cp), f) <- ps
            guard (rp == r)
            return (cp, f ( getNoBounds m (rp, cp) ))
    let updated = V.update rw updates
    return (r, updated)

filter :: (a -> Bool) -> Matrix a -> Vector Point
filter f m = V.filter (f . getNoBounds m) . idx $ m

all :: (a -> Bool) -> Matrix a -> Bool
all f = V.all (V.all f)

map :: (a -> b) -> Matrix a -> Matrix b
map f m = (f <$>) <$> m

size :: Matrix a -> Int
size m = Matrix.length m * Matrix.width m