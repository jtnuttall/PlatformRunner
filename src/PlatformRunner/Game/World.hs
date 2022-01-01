{-# LANGUAGE TemplateHaskell #-}
module PlatformRunner.Game.World where

import           Apecs
import           Apecs.Physics                  ( Physics )
import           PlatformRunner.Components.Entity
import           PlatformRunner.Components.Global
import           PlatformRunner.Components.Movement
import           RIO                     hiding ( Map )

makeWorld "PlatformWorld"
  [ ''Physics
  , ''Position
  , ''Velocity
  , ''Particle
  , ''Coin
  , ''Player
  , ''Score
  , ''Time
  ]
