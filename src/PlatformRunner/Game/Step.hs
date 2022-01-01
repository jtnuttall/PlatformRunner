module PlatformRunner.Game.Step where

import           Apecs
import           Linear
import           PlatformRunner.AppEnv
import           PlatformRunner.Game.Constant
import           PlatformRunner.Import
import           PlatformRunner.Settings.Types  ( Settings(..) )

initializeSystem
  :: (HasLogFunc env, HasAppSettingsRef env) => PlatformRunnerSystem env ()
initializeSystem = do
  settings <- lift $ readSomeRef =<< view appSettingsRefL
  let constants = platformRunnerConstants (difficulty settings)

  lift $ logDebug $ "Initializing System with: " <> displayShow constants

  playerEty <- newEntity
    (Player, Position . playerStartPos $ constants, Velocity 0)

  return ()

stepPosition :: Float -> PlatformRunnerSystem env ()
stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

incTime :: Float -> PlatformRunnerSystem env ()
incTime dT = modify global $ \(Time t) -> Time (t + dT)

step :: Float -> PlatformRunnerSystem env ()
step dT = do
  incTime dT
  stepPosition dT
