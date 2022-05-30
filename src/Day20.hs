module Day20 where

import Control.Monad (when)
import Data.Array
import Data.Bits
import Data.Type.Equality (apply)
import Lib

data Pixel = Light | Dark deriving (Show)

readPx :: Char -> Pixel
readPx '#' = Light
readPx '.' = Dark
readPx _ = error "readPx expected [#.]"

asBoolPx :: Pixel -> Bool
asBoolPx Light = True
asBoolPx Dark = False

flipPx :: Pixel -> Pixel
flipPx Light = Dark
flipPx Dark = Light

asBitsPx :: Pixel -> Int -> Int
asBitsPx Light = bit
asBitsPx Dark = const 0

asCharPx :: Pixel -> Char
asCharPx Light = '#'
asCharPx Dark = '.'

data PixelGroup
  = PixelGroup
      Pixel
      Pixel
      Pixel
      Pixel
      Pixel
      Pixel
      Pixel
      Pixel
      Pixel
  deriving (Show)

asIntPg :: PixelGroup -> Int
asIntPg (PixelGroup nw n ne e c w sw s se) =
  let seb = asBitsPx se 0
      sb = asBitsPx s 1
      swb = asBitsPx sw 2
      wb = asBitsPx w 3
      cb = asBitsPx c 4
      eb = asBitsPx e 5
      neb = asBitsPx ne 6
      nb = asBitsPx n 7
      nwb = asBitsPx nw 8
   in seb .|. sb .|. swb .|. wb .|. cb .|. eb .|. neb .|. nb .|. nwb

newtype ImageAlg = ImageAlg (Array Int Pixel) deriving (Show)

readAlg :: String -> ImageAlg
readAlg line = ImageAlg $ array bounds mapping
  where
    bounds = (0, 511)
    mapping = [(i, readPx c) | (i, c) <- zip [0 ..] line]

applyAlgPx :: ImageAlg -> PixelGroup -> Pixel
applyAlgPx (ImageAlg a) pg = a ! asIntPg pg

zeroBitAlg :: ImageAlg -> Pixel
zeroBitAlg (ImageAlg a) = a ! 0

fullBitAlg :: ImageAlg -> Pixel
fullBitAlg (ImageAlg a) = a ! 511

type Point = (Int, Int)

data Image = Image (Array Point Pixel) Pixel deriving (Show)

readImage :: [String] -> Image
readImage ls = Image (array bounds mapping) Dark
  where
    bounds = ((0, 0), (length ls - 1, length (head ls) - 1))
    mapping = [((i, j), readPx c) | (i, row) <- zip [0 ..] ls, (j, c) <- zip [0 ..] row]

infImg :: Image -> Pixel
infImg (Image _ inf) = inf

boundsImg :: Image -> (Point, Point)
boundsImg (Image a _) = bounds a

pointsImg :: Image -> [Point]
pointsImg (Image a _) = indices a

pixelsImg :: Image -> [Pixel]
pixelsImg (Image a _) = elems a

inBoundsImg :: Image -> Point -> Bool
inBoundsImg (Image a _) (r, c) = inRows && inCols
  where
    ((rl, cl), (rh, ch)) = bounds a
    inRows = rl <= r && r <= rh
    inCols = cl <= c && c <= ch

getPixelImg :: Image -> Point -> Pixel
getPixelImg img@(Image a inf) p =
  if inBoundsImg img p
    then a ! p
    else inf

sampleImage :: Image -> Point -> PixelGroup
sampleImage img (r, c) =
  PixelGroup
    (getPixelImg img (r -1, c -1))
    (getPixelImg img (r -1, c))
    (getPixelImg img (r -1, c + 1))
    (getPixelImg img (r, c -1))
    (getPixelImg img (r, c))
    (getPixelImg img (r, c + 1))
    (getPixelImg img (r + 1, c -1))
    (getPixelImg img (r + 1, c))
    (getPixelImg img (r + 1, c + 1))

applyAlgImg :: ImageAlg -> Image -> Image
applyAlgImg alg img = Image (array newBounds mapped) newInf
  where
    db = 1
    ((rl, cl), (rh, ch)) = boundsImg img
    newBounds = ((rl - db, cl - db), (rh + db, ch + db))
    mapping p = applyAlgPx alg (sampleImage img p)
    mapped = [((r, c), mapping (r, c)) | r <- [rl - db .. rh + db], c <- [cl - db .. ch + db]]
    newInf =
      if asBoolPx (infImg img)
        then fullBitAlg alg
        else zeroBitAlg alg

data Prob = Prob
  { _alg :: ImageAlg,
    _img :: Image
  }
  deriving (Show)

readProb :: [String] -> Prob
readProb (algs : _ : imgss) = Prob (readAlg algs) (readImage imgss)
readProb _ = error "Bad input"

solve :: Int -> Prob -> Int
solve n (Prob alg img0) = numLit (imgs !! n)
  where
    numLit img = sum [if asBoolPx px then 1 else 0 | px <- pixelsImg img]
    imgs = iterate (applyAlgImg alg) img0

soln :: Lib.Day
soln = do Lib.Day 20 (show . solve 2 . readProb <$> readInput 20 1) (show . solve 50 . readProb <$> readInput 20 1)
