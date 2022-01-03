{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main
  ( main
  ) where

import           Graphics.Gloss.Interface.Environment
                                                ( getScreenSize )
import           Linear                         ( V2(V2) )
import           Options.Applicative.Simple
import qualified Paths_PlatformRunner
import           PlatformRunner.Env
import           PlatformRunner.Game.Constant   ( platformRunnerConstants )
import           PlatformRunner.Import
import           PlatformRunner.RunApp          ( runApp )
import           PlatformRunner.Settings.Defaults
                                                ( configFolderName
                                                , defaultSettingsBaseName
                                                , defaultSettingsExt
                                                )
import           PlatformRunner.Settings.IO
import           PlatformRunner.Settings.Types  ( Dimensions(Dimensions)
                                                , Settings(difficulty)
                                                )
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
               <> (   configFolderName
                  </> defaultSettingsBaseName
                  <>  defaultSettingsExt
                  )
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
    screenSize <- getScreenSize

    let appEnv = AppEnv { appLogFunc        = lf
                        , appProcessContext = pc
                        , appCliOptions     = cliOptions
                        , appScreenSize     = Dimensions $ uncurry V2 screenSize
                        }

    appConfigDir <- runRIO appEnv getOrCreateAppConfigDir

    let appBaseConfig =
          AppBaseConfig { appEnv = appEnv, appConfigDir = appConfigDir }

    appSettings <- runRIO appBaseConfig readOrCreateAppSettings

    case appSettings of
      Left  except   -> fail $ show except -- TODO handle errors with retry?
      Right settings -> do
        appSettingsRef   <- newSomeRef settings
        gameConstantsRef <- newSomeRef
          $ platformRunnerConstants (difficulty settings)

        let platformRunnerEnv = PlatformRunnerEnv { appBaseConfig
                                                  , appSettingsRef
                                                  , gameConstantsRef
                                                  }

        runRIO platformRunnerEnv runApp
