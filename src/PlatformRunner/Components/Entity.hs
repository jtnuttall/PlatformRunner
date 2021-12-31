module PlatformRunner.Components.Entity
  ( Target(..)
  , Particle(..)
  , Coin(..)
  , Platform(..)
  , Player(..)
  ) where

import           Apecs                          ( Component
                                                , Map
                                                , Storage
                                                , Unique
                                                )
import           RIO                            ( Float
                                                , Show
                                                )

data Target = Target
  deriving Show

instance Component Target where
  type Storage Target = Map Target

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
