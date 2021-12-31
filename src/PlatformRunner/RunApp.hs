module PlatformRunner.RunApp
  ( runApp
  ) where

import           PlatformRunner.AppEnv
import           PlatformRunner.Import

runApp :: RIO App ()
runApp = do
  cliOptions <- view appCliOptionsL
  settings   <- readSomeRef =<< view appSettingsRefL

  logInfo $ displayShow settings
  logInfo $ displayShow cliOptions

  logInfo "We're inside the application!"
