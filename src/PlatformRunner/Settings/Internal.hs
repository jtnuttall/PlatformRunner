module PlatformRunner.Settings.Internal
  ( -- * Types
    DisplayMode(..)
  , Difficulty(..)
  , Settings(..)

    -- * Constraints
  , HasDisplayMode(..)
  , HasDifficulty(..)
  , HasSettings(..)

    -- * Defaults
  , defaultSettings
  , defaultConfigFolderName
  , defaultSettingsBaseName
  , defaultSettingsExt
  , defaultSettingsDocExt

    -- * Yaml Schemas
  , settingsSchema
  ) where

import           Data.Yaml               hiding ( Parser )
import           Linear                         ( V2(V2) )
import           PlatformRunner.Types
import           RIO
import           YamlParse.Applicative


--------------------------------------------------------------------------------
data DisplayMode
  = Fullscreen
  | Windowed Dimensions
  deriving (Generic, Show)

instance YamlSchema DisplayMode where
  yamlSchema = alternatives
    [ Fullscreen <$ literalString "Fullscreen"
    , objectParser
      "Windowed"
      (   Windowed
      <$> optionalFieldWithDefault "size" defaultWindowSize "Window Size"
      )
    ]

instance ToJSON DisplayMode

instance FromJSON DisplayMode where
  parseJSON = viaYamlSchema

class HasDisplayMode settings where
  displayModeL :: Lens' settings DisplayMode

instance HasDisplayMode DisplayMode where
  displayModeL = id


--------------------------------------------------------------------------------
data Difficulty = Easy | Normal | Hard deriving (Generic)

instance Show Difficulty where
  show = \case
    Easy   -> "Easy"
    Normal -> "Normal"
    Hard   -> "Hard"

instance YamlSchema Difficulty where
  yamlSchema = alternatives
    [ Easy <$ literalString "Easy"
    , Normal <$ literalString "Normal"
    , Hard <$ literalString "Hard"
    ]

instance ToJSON Difficulty

instance FromJSON Difficulty where
  parseJSON = viaYamlSchema

class HasDifficulty settings where
  difficultyL :: Lens' settings Difficulty

instance HasDifficulty Difficulty where
  difficultyL = id


--------------------------------------------------------------------------------
data Settings = Settings
  { displayMode :: !DisplayMode
  , difficulty  :: !Difficulty
  , fps         :: !Int
  }
  deriving (Generic, Show)

class HasSettings env where
  settingsL :: Lens' env Settings

instance HasSettings Settings where
  settingsL = id

instance HasDisplayMode Settings where
  displayModeL = lens displayMode (\x y -> x { displayMode = y })

instance HasDifficulty Settings where
  difficultyL = lens difficulty (\x y -> x { difficulty = y })

instance YamlSchema Settings where
  yamlSchema =
    objectParser "PlatformRunner Settings"
      $   Settings
      <$> optionalFieldWithDefault "displayMode"
                                   (displayMode defaultSettings)
                                   "The display mode to use"
      <*> optionalFieldWithDefault "difficulty"
                                   (difficulty defaultSettings)
                                   "Game difficulty"
      <*> optionalFieldWithDefault "fps" (fps defaultSettings) "FPS target"

instance ToJSON Settings

instance FromJSON Settings where
  parseJSON = viaYamlSchema


--------------------------------------------------------------------------------
defaultConfigFolderName :: FilePath
defaultConfigFolderName = "platform-runner"

defaultSettingsBaseName :: FilePath
defaultSettingsBaseName = "settings"

defaultSettingsExt :: FilePath
defaultSettingsExt = ".yaml"

defaultSettingsDocExt :: FilePath
defaultSettingsDocExt = ".doc"

defaultWindowSize :: Dimensions
defaultWindowSize = Dimensions (V2 640 380)

defaultSettings :: Settings
defaultSettings = Settings { displayMode = Windowed defaultWindowSize
                           , difficulty  = Normal
                           , fps         = 60
                           }


--------------------------------------------------------------------------------
settingsSchema :: Schema
settingsSchema = explainParser (yamlSchema :: YamlParser Settings)
