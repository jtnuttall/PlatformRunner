module PlatformRunner.RunApp
  ( runApp
  ) where

import           PlatformRunner.AppEnv
import           PlatformRunner.Import
import           PlatformRunner.Settings.IO     ( readOrCreateAppSettings )

runApp :: RIO App ()
runApp = do
  cliOptions <- view appCliOptionsL
  settings   <- view appSettingsL

  logInfo $ displayShow settings
  logInfo $ displayShow cliOptions

  logInfo "We're inside the application!"
