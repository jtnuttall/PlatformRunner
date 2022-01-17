{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  ) where

import           Common.Import
import           Graphics.Gloss.Interface.Environment
                                                ( getScreenSize )
import           Linear                         ( V2(V2) )
import           Options.Applicative.Simple
import qualified Paths_PlatformRunner
import qualified PlatformRunner                 ( runWith )
import           PlatformRunner.Env
import           PlatformRunner.Game.Constant   ( platformRunnerConstants )
import           PlatformRunner.Settings
import           PlatformRunner.Types
import           RIO.Process

verboseParser :: Parser OptionVerboseFlag
verboseParser = OptionVerboseFlag <$> optional
  (switch (long "verbose" <> short 'v' <> help "Verbose output?"))

overrideSettingsFile :: Parser OptionOverrideSettingsPath
overrideSettingsFile = OptionOverrideSettingsPath <$> optional
  (strOption
    (long "settings" <> short 's' <> help "Overrides the settings file.")
  )

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  CliOptions <$> verboseParser <*> overrideSettingsFile <**> helper

main :: IO ()
main = do
  (cliOptions, ()) <- simpleOptions
    $(simpleVersion Paths_PlatformRunner.version)
    "Platform Runner"
    "Runs as a GL window"
    cliOptionsParser
    empty

  let verboseFlag =
        fromMaybe False (unOptionVerboseFlag $ optionVerboseFlag cliOptions)

  logOptions     <- logOptionsHandle stderr verboseFlag
  processContext <- mkDefaultProcessContext

  withLogFunc logOptions $ \logFunc -> do
    screenSize   <- getScreenSize
    appConfigDir <- getOrCreateAppConfigDir

    let appEnv = AppEnv
          { appLogFunc        = logFunc
          , appProcessContext = processContext
          , appCliOptions     = cliOptions
          , appScreenSize = ScreenSize . Dimensions $ uncurry V2 $!! screenSize
          , appConfigDir      = ConfigDir appConfigDir
          }

    appSettings <- runRIO appEnv readOrCreateAppSettings

    case appSettings of
      Left  except   -> throwIO except
      Right settings -> do
        appSettingsRef <- newSomeRef settings

        let appBaseSettingsEnv = AppBaseSettingsEnv { appEnv, appSettingsRef }

        gameConstants <- runRIO appBaseSettingsEnv getPlatformRunnerParameters

        let platformRunnerEnv =
              PlatformRunnerEnv { appBaseSettingsEnv, gameConstants }

        PlatformRunner.runWith platformRunnerEnv
