module PlatformRunner.Game.Step where

import           Apecs
import           Linear
import           PlatformRunner.Env
import           PlatformRunner.Game.Constant
import           PlatformRunner.Import

initializeSystem
  :: (HasLogFunc env, HasGameConstantsRef env) => PlatformRunnerSystem env ()
initializeSystem = do
  constants <- lift $ readSomeRef =<< view gameConstantsRefL
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
