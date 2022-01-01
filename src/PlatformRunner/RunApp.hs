module PlatformRunner.RunApp
  ( runApp
  ) where

import           Apecs                          ( runSystem )
import           PlatformRunner.Env
import           PlatformRunner.Game.Step       ( step )
import           PlatformRunner.Game.World      ( initPlatformWorld )
import           PlatformRunner.Import

runApp :: RIO PlatformRunnerEnv ()
runApp = do
  cliOptions <- view appCliOptionsL
  configDir  <- view appConfigDirL
  settings   <- readSomeRef =<< view appSettingsRefL

  logInfo "Initializing PlatformRunner.."
  logDebug $ "Config directory: " <> displayShow configDir
  logDebug $ "Received CLI options: " <> displayShow cliOptions
  logDebug $ "Initial settings: " <> displayShow settings

  platformWorld <- liftIO initPlatformWorld
  runSystem (step 1.1) platformWorld
