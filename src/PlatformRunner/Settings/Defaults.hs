module PlatformRunner.Settings.Defaults where

import           Linear                         ( V2(V2) )
import           PlatformRunner.Settings.Types
-- import Data.Coerce (coerce)
import           RIO

configFolderName :: FilePath
configFolderName = "platform-runner"

defaultSettingsBaseName :: FilePath
defaultSettingsBaseName = "settings"

defaultSettingsExt :: FilePath
defaultSettingsExt = ".yaml"

defaultSettingsDocExt :: FilePath
defaultSettingsDocExt = ".doc"

defaultSettings :: Settings
defaultSettings = Settings { displayMode = Fullscreen
                           , difficulty  = Normal
                           , resolution  = Dimensions (V2 640 360)
                           }
