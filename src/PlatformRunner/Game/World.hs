{-# LANGUAGE TemplateHaskell #-}
module PlatformRunner.Game.World where

import           Apecs
import           Apecs.Gloss                    ( Camera )
import           Apecs.Physics                  ( Physics )
import           PlatformRunner.Components.Entity
import           PlatformRunner.Components.Global
import           RIO                     hiding ( Map )

makeWorld "PlatformWorld"
  [ ''Particle
  , ''Coin
  , ''Platform
  , ''Player
  , ''IsFlying
  , ''Score
  , ''Time
  , ''Physics
  , ''Camera
  ]
