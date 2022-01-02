module PlatformRunner.Settings.Types
  ( Dimensions(..)
  , Coordinates(..)
  , DisplayMode(..)
  , glossDisplayMode
  , Difficulty(..)
  , Settings(..)
  ) where

import qualified Apecs.Gloss                   as Apecs
                                                ( Display(..) )
import           Data.Yaml               hiding ( Parser )
import           Linear                         ( V2(V2) )
import           PlatformRunner.Import
import           YamlParse.Applicative

newtype Dimensions = Dimensions { unDimensions :: V2 Int }

instance Show Dimensions where
  show Dimensions { unDimensions = V2 width height } =
    show width <> "x" <> show height

instance YamlSchema Dimensions where
  yamlSchema =
    objectParser "dimensions"
      $   Dimensions
      <$> (V2 <$> requiredField' "width" <*> requiredField' "height")

instance ToJSON Dimensions where
  toJSON Dimensions { unDimensions = V2 width height } =
    object ["width" .= width, "height" .= height]

newtype Coordinates = Coordinates { unCoordinates :: V2 Int }

instance Show Coordinates where
  show Coordinates { unCoordinates = V2 x y } =
    "(" <> show x <> ", " <> show y <> ")"

instance ToJSON Coordinates where
  toJSON Coordinates { unCoordinates = V2 x y } = object ["x" .= x, "y" .= y]

data DisplayMode
  = Fullscreen
  | Windowed
  deriving (Generic, Show)

instance YamlSchema DisplayMode where
  yamlSchema = alternatives
    [ Fullscreen <$ literalString "Fullscreen"
    , Windowed <$ literalString "Windowed"
    ]

instance ToJSON DisplayMode

instance FromJSON DisplayMode where
  parseJSON = viaYamlSchema

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

data Settings = Settings
  { displayMode :: !DisplayMode
  , difficulty  :: !Difficulty
  , resolution  :: !Dimensions
  }
  deriving (Generic, Show)

glossDisplayMode :: String -> (Int, Int) -> Settings -> Apecs.Display
glossDisplayMode windowTitle windowCenter Settings { displayMode, resolution }
  = let V2 width height = unDimensions resolution
    in  case displayMode of
          Fullscreen -> Apecs.FullScreen
          Windowed   -> Apecs.InWindow windowTitle (width, height) windowCenter

instance YamlSchema Settings where
  yamlSchema =
    unnamedObjectParser
      $   Settings
      <$> requiredField "displayMode" "The display mode to use."
      <*> requiredField "difficulty"  "Difficulty"
      <*> requiredField "resolution"  "Resolution"

instance ToJSON Settings

instance FromJSON Settings where
  parseJSON = viaYamlSchema
