module PlatformRunner.RunApp
  ( runApp
  ) where

import           PlatformRunner.AppEnv
import           PlatformRunner.Import

runApp :: RIO App ()
runApp = do
  cliOptions <- view appCliOptionsL
  configDir  <- view appConfigDirL
  settings   <- readSomeRef =<< view appSettingsRefL

  logInfo "Initializing PlatformRunner.."
  logDebug $ "Config directory: " <> displayShow configDir
  logDebug $ "Received CLI options: " <> displayShow cliOptions
  logDebug $ "Initial settings: " <> displayShow settings
