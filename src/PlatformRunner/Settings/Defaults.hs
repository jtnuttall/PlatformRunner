module PlatformRunner.Settings.Defaults where

import           GHC.IO                         ( FilePath )
import           Linear                         ( V2(V2) )
import           PlatformRunner.Settings.Types
-- import Data.Coerce (coerce)
import           RIO

configFolderName :: FilePath
configFolderName = "platform-runner"

defaultSettingsFileName :: FilePath
defaultSettingsFileName = "settings.yaml"

defaultSettings :: Settings
defaultSettings = Settings { displayMode = Fullscreen
                           , difficulty  = Normal
                           , windowDims  = WindowDims $ V2 640 360
                           }
