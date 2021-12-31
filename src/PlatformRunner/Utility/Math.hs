module PlatformRunner.Utility.Math where

import           Linear
import           RIO

clamp :: Ord a => a -> a -> a -> a
clamp minValue maxValue value | value < minValue = minValue
                              | value > maxValue = maxValue
                              | otherwise        = value

clampV2 :: Ord a => V2 a -> V2 a -> V2 a -> V2 a
clampV2 (V2 minX minY) (V2 maxX maxY) (V2 x y) =
  V2 (clamp minX maxX x) (clamp minY maxY y)

