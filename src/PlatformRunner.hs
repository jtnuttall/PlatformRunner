module PlatformRunner
  ( run
  ) where

import           Apecs                          ( runWith )
import           Apecs.Physics.Gloss            ( white )
import           PlatformRunner.Env
import           PlatformRunner.Game.Draw       ( draw )
import           PlatformRunner.Game.Input      ( handleEvent )
import           PlatformRunner.Game.Step       ( initializeSystem
                                                , step
                                                )
import           PlatformRunner.Game.World      ( initPlatformWorld )
import           PlatformRunner.Settings        ( Settings(..)
                                                , getGlossDisplayMode
                                                , getSettings
                                                )
import           RIO
import           Utility.Gloss                  ( play )

run :: RIO PlatformRunnerEnv ()
run = do
  cliOptions            <- viewConfig @CliOptions
  ConfigDir  configDir  <- viewConfig
  ScreenSize screenSize <- viewConfig

  displayMode           <- getGlossDisplayMode
  settings              <- getSettings

  logInfo "Initializing Platform Runner..."
  logInfo $ "Found native screen size => " <> displayShow screenSize
  logInfo $ "Running with Display: " <> displayShow displayMode

  logDebug $ "Config directory: " <> displayShow configDir
  logDebug $ "Received CLI options: " <> displayShow cliOptions
  logDebug $ "Initial settings: " <> displayShow settings

  platformWorld <- liftIO initPlatformWorld
  runWith platformWorld $ do
    initializeSystem
    play displayMode white (fps settings) draw handleEvent step
