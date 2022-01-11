{-# LANGUAGE OverloadedLists #-}
module PlatformRunner.Level.Generated
  ( --allBuiltinLevels
  -- , flatWorld
  ) where

import           PlatformRunner.Level.Generated.FlatWorld
import           PlatformRunner.Level.Internal.Types
                                                ( Level )
import           RIO
import           System.Random.SplitMix         ( SMGen )

-- allBuiltinLevels :: Monad m => SMGen -> Vector Level
-- allBuiltinLevels g = [flatWorld g]

