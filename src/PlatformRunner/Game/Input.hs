module PlatformRunner.Game.Input where

import           Apecs
import           Apecs.Gloss
import           Apecs.Physics
import           Common.Import
import           PlatformRunner.Components.Entity
import           PlatformRunner.Env
import           PlatformRunner.Game.Constant

handleEvent
  :: (HasLogFunc env, HasConfigElem env PlatformRunnerConstants)
  => Event
  -> PlatformRunnerSystem env ()
handleEvent event = do
  constants <- lift viewConfig

  case event of
    EventKey (Char 'w') Down _ _ -> do
      cmap $ \(Player, Velocity v0, Not :: Not IsFlying) ->
        Velocity (v0 + playerJumpVelocity constants)
    EventKey (SpecialKey KeySpace) Down _ _ -> do
      cmap $ \(Player, Velocity v0, Not :: Not IsFlying) ->
        Velocity (v0 + playerJumpVelocity constants)

    EventKey (Char 'q') Down _ _ -> do
      lift $ logInfo "Got 'q' -- exiting gracefully."
      exitSuccess

    _ -> return ()
