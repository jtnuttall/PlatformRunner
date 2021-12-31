{-# LANGUAGE TemplateHaskell #-}
module PlatformRunner.World where

import           Apecs
import           PlatformRunner.Components.Entity
import           PlatformRunner.Components.Global
import           PlatformRunner.Components.Movement
import           RIO                     hiding ( Map )

makeWorld
  "PlatformWorld"
  [''Position, ''Velocity, ''Target, ''Particle, ''Player, ''Score, ''Time]
