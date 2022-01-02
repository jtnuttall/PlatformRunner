{-# LANGUAGE TemplateHaskell #-}
module PlatformRunner.Game.World where

import           Apecs
import           Apecs.Gloss                    ( Camera )
import           Apecs.Physics                  ( Physics )
import           PlatformRunner.Components.Entity
import           PlatformRunner.Components.Global
import           PlatformRunner.Components.Movement
import           RIO                     hiding ( Map )

makeWorld "PlatformWorld"
  [ ''Position
  , ''Velocity
  , ''Particle
  , ''Coin
  , ''Platform
  , ''Player
  , ''Score
  , ''Time
  , ''Physics
  , ''Camera
  ]
