module Utility.Math
  ( -- * Bounding functions
    clamp
  , clampV2
  , expandRange

    -- * Normal vectors
  , up
  , down
  , left
  , right
  ) where

import           Linear
import           RIO


--------------------------------------------------------------------------------
clamp :: Ord a => a -> a -> a -> a
clamp minValue maxValue value | value < minValue = minValue
                              | value > maxValue = maxValue
                              | otherwise        = value

clampV2 :: Ord a => V2 a -> V2 a -> V2 a -> V2 a
clampV2 (V2 minX minY) (V2 maxX maxY) (V2 x y) =
  V2 (clamp minX maxX x) (clamp minY maxY y)

-- | expandRange : [0, 1) -> [lower, upper)
expandRange :: (Integral a, Num b) => a -> a -> a -> b
expandRange inInitialRange lower upper =
  fromIntegral $ inInitialRange `mod` ((upper - lower + 1) + lower)


--------------------------------------------------------------------------------
up :: Num a => V2 a
up = V2 0 1

left :: Num a => V2 a
left = V2 (-1) 0

down :: Num a => V2 a
down = V2 0 (-1)

right :: Num a => V2 a
right = V2 1 0
