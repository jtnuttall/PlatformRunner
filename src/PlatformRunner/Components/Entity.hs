module PlatformRunner.Components.Entity
  ( Particle(..)
  , Coin(..)
  , Platform(..)
  , Player(..)
  , IsFlying(..)
  ) where

import           Apecs                          ( Component
                                                , Map
                                                , Storage
                                                , Unique
                                                )
import           RIO                            ( Float
                                                , Show
                                                )

newtype Particle = Particle Float deriving Show

instance Component Particle where
  type Storage Particle = Map Particle

data Coin = Coin
  deriving Show

instance Component Coin where
  type Storage Coin = Map Coin

data Platform = Platform
  deriving Show

instance Component Platform where
  type Storage Platform = Map Platform

data Player = Player
  deriving Show

instance Component Player where
  type Storage Player = Unique Player

data IsFlying = IsFlying
  deriving Show

instance Component IsFlying where
  type Storage IsFlying = Map IsFlying
