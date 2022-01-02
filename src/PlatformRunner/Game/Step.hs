module PlatformRunner.Game.Step where

import           Apecs
import           Linear
import           PlatformRunner.Env
import           PlatformRunner.Game.Constant
import           PlatformRunner.Import
import           PlatformRunner.Settings.Types  ( windowDims )

initializeSystem
  :: (HasLogFunc env, HasGameConstantsRef env) => PlatformRunnerSystem env ()
initializeSystem = do
  constants <- lift $ readSomeRef =<< view gameConstantsRefL
  lift $ logDebug $ "Initializing System with: " <> displayShow constants

  playerEty <- newEntity
    (Player, Position . playerStartPos $ constants, Velocity 0)

  return ()

clearPlatforms :: (HasAppSettingsRef env) => PlatformRunnerSystem env ()
clearPlatforms = do
  settings <- lift $ readSomeRef =<< view appSettingsRefL
  let (V2 windowWidth _) = windowDims settings

  cmap $ \platforms@(Platform, Position (V2 x _), Velocity _) ->
    if x < 0 || x > windowWidth then Nothing else Just platforms

stepPosition :: Float -> PlatformRunnerSystem env ()
stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

incTime :: Float -> PlatformRunnerSystem env ()
incTime dT = modify global $ \(Time t) -> Time (t + dT)

step :: Float -> PlatformRunnerSystem env ()
step dT = do
  incTime dT
  stepPosition dT
