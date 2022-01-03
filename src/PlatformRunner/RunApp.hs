module PlatformRunner.RunApp
  ( runApp
  ) where

import           Apecs                          ( runWith )
import           Apecs.Gloss                    ( Display(InWindow) )
import           Apecs.Physics.Gloss            ( black )
import           PlatformRunner.Env
import           PlatformRunner.Game.Input      ( handleEvent )
import           PlatformRunner.Game.Picture    ( draw )
import           PlatformRunner.Game.Step       ( initializeSystem
                                                , step
                                                )
import           PlatformRunner.Game.World      ( initPlatformWorld )
import           PlatformRunner.Import
import           PlatformRunner.Settings.Types  ( glossDisplayMode )
import           PlatformRunner.Utility.Gloss   ( play )

runApp :: RIO PlatformRunnerEnv ()
runApp = do
  cliOptions <- view appCliOptionsL
  configDir  <- view appConfigDirL
  settings   <- readSomeRef =<< view appSettingsRefL

  logInfo "Initializing PlatformRunner.."
  logDebug $ "Config directory: " <> displayShow configDir
  logDebug $ "Received CLI options: " <> displayShow cliOptions
  logDebug $ "Initial settings: " <> displayShow settings

  let displayMode = glossDisplayMode "Platform Runner" (10, 10) settings

  platformWorld <- liftIO initPlatformWorld
  runWith platformWorld $ do
    initializeSystem
    play displayMode black 60 draw handleEvent step
