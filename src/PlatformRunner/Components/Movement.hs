module PlatformRunner.Components.Movement
  ( Position(..)
  , Velocity(..)
  ) where

import           Apecs                          ( Component(..)
                                                , Map
                                                )
import           Linear                         ( V2 )
import           RIO                            ( Float
                                                , Show
                                                )

newtype Position = Position (V2 Float) deriving Show

instance Component Position where
  type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving Show

instance Component Velocity where
  type Storage Velocity = Map Velocity
