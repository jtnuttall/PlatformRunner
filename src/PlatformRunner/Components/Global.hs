{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PlatformRunner.Components.Global where

import           Apecs                          ( Component
                                                , Global
                                                , Storage
                                                )
import           RIO

newtype Score = Score Int deriving (Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = 0

instance Component Score where
  type Storage Score = Global Score

newtype Time = Time Double deriving (Show, Num)

instance Semigroup Time where
  (<>) = (+)

instance Monoid Time where
  mempty = 0

instance Component Time where
  type Storage Time = Global Time
