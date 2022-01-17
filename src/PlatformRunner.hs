module PlatformRunner
  ( runWith
  ) where

import qualified Apecs                          ( runWith )
import qualified Apecs.Gloss                   as Apecs
                                                ( white )
import           PlatformRunner.Env
import           PlatformRunner.Game.Draw       ( draw )
import           PlatformRunner.Game.Input      ( handleEvent )
import           PlatformRunner.Game.Step       ( initializeSystem
                                                , step
                                                )
import           PlatformRunner.Game.World      ( initPlatformWorld )
import           PlatformRunner.Level           ( flatWorld
                                                , withLevel
                                                )
import           PlatformRunner.Settings        ( Settings(fps)
                                                , getGlossDisplayMode
                                                , getSettings
                                                )
import           RIO
import           System.Random.MWC              ( createSystemRandom )
import           Utility.Gloss                  ( play )

runWith :: PlatformRunnerEnv -> IO ()
runWith platformRunnerEnv = do
  g <- createSystemRandom
  withLevel (flatWorld g) $ \levelMetadata pullUpdate -> do
    let envWithLevel =
          EnvWithLevel { platformRunnerEnv, levelMetadata, pullUpdate }

    runRIO envWithLevel runLevel

runLevel :: RIO EnvWithLevel ()
runLevel = do
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

  Apecs.runWith platformWorld $ do
    initializeSystem
    play displayMode Apecs.white (fps settings) draw handleEvent step


