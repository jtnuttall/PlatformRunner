{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PlatformRunner.Components.Global where

import           Apecs                          ( Component
                                                , Global
                                                , Storage
                                                )
import           Linear                         ( Additive(zero)
                                                , V2
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

newtype LastReceivedPlatform =
  LastReceivedPlatform (V2 Double)
  deriving (Show, Num)

instance Semigroup LastReceivedPlatform where
  (<>) = (+)

instance Monoid LastReceivedPlatform where
  mempty = LastReceivedPlatform zero

instance Component LastReceivedPlatform where
  type Storage LastReceivedPlatform = Global LastReceivedPlatform

newtype WaitingForPlatforms =
  WaitingForPlatforms Bool
  deriving Show

instance Semigroup WaitingForPlatforms where
  (<>) (WaitingForPlatforms left) (WaitingForPlatforms right) =
    WaitingForPlatforms $ left && right

instance Monoid WaitingForPlatforms where
  mempty = WaitingForPlatforms True

instance Component WaitingForPlatforms where
  type Storage WaitingForPlatforms = Global WaitingForPlatforms
