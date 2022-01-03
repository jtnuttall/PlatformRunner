module PlatformRunner.Settings.Defaults where

import           RIO

configFolderName :: FilePath
configFolderName = "platform-runner"

defaultSettingsBaseName :: FilePath
defaultSettingsBaseName = "settings"

defaultSettingsExt :: FilePath
defaultSettingsExt = ".yaml"

defaultSettingsDocExt :: FilePath
defaultSettingsDocExt = ".doc"
