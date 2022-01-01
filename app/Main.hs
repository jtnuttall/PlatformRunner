{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main
  ( main
  ) where

import           Options.Applicative.Simple
import qualified Paths_PlatformRunner
import           PlatformRunner.Env
import           PlatformRunner.Game.Constant   ( platformRunnerConstants )
import           PlatformRunner.Import
import           PlatformRunner.RunApp          ( runApp )
import           PlatformRunner.Settings.Defaults
                                                ( configFolderName
                                                , defaultSettingsFileName
                                                )
import           PlatformRunner.Settings.IO
import           PlatformRunner.Settings.Types  ( Settings(difficulty) )
import           RIO.FilePath                   ( (</>) )
import           RIO.Process

cliOptionsParser :: Parser CliOptions
cliOptionsParser =
  CliOptions
    <$>  switch (long "verbose" <> short 'v' <> help "Verbose output?")
    <*>  optional
           (strOption
             (long "settings" <> short 's' <> help
               (  "Overrides the settings file. Defaults to \""
               <> (configFolderName </> defaultSettingsFileName)
               <> "\""
               )
             )
           )
    <**> helper

main :: IO ()
main = do
  (cliOptions, ()) <- simpleOptions
    $(simpleVersion Paths_PlatformRunner.version)
    "Platform Runner"
    "Runs as a GL window"
    cliOptionsParser
    empty

  lo <- logOptionsHandle stderr (optionsVerbose cliOptions)
  pc <- mkDefaultProcessContext


  withLogFunc lo $ \lf -> do
    let appEnv = AppEnv { appLogFunc        = lf
                        , appProcessContext = pc
                        , appCliOptions     = cliOptions
                        }

    appConfigDir <- runRIO appEnv getOrCreateAppConfigDir

    let appBaseConfig =
          AppBaseConfig { appEnv = appEnv, appConfigDir = appConfigDir }

    appSettings <- runRIO appBaseConfig readOrCreateAppSettings

    case appSettings of
      Left  except   -> fail $ show except -- TODO handle errors with retry?
      Right settings -> do
        appSettingsRef <- newSomeRef settings
        gameConstantsRef <- newSomeRef $ platformRunnerConstants (difficulty settings)

        let platformRunnerEnv = PlatformRunnerEnv
              { appBaseConfig
              , appSettingsRef
              , gameConstantsRef
              }

        runRIO platformRunnerEnv runApp
