module PlatformRunner.Settings.Defaults where

import           GHC.IO                         ( FilePath )
import           PlatformRunner.Settings.Types

configFolderName :: FilePath
configFolderName = "platform-runner"

defaultSettingsFileName :: FilePath
defaultSettingsFileName = "settings.yaml"

defaultSettings :: Settings
defaultSettings = Settings { displayMode = Fullscreen, difficulty = Normal }
