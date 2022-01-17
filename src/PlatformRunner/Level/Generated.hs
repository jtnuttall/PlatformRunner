{-# LANGUAGE OverloadedLists #-}
module PlatformRunner.Level.Generated
  ( allBuiltinLevels
  , flatWorld
  , Level
  ) where

import           PlatformRunner.Level.Generated.FlatWorld
import           PlatformRunner.Level.Internal.STM
                                                ( InitialLevelParameters )
import           RIO
import           System.Random.Stateful         ( StatefulGen )

type Level m = InitialLevelParameters m

allBuiltinLevels :: (StatefulGen g m) => g -> Vector (Level m)
allBuiltinLevels g = [flatWorld g]

