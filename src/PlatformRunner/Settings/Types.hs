{-# LANGUAGE TemplateHaskell #-}
module PlatformRunner.Settings.Types
  ( DisplayMode(..)
  , toGlossDisplayMode
  , Difficulty(..)
  , Settings(..)
  , WindowDims(..)
  ) where

import qualified Apecs.Gloss                   as Apecs
                                                ( Display(..) )
import           Control.Lens                   ( makeLenses )
import           Data.Aeson
import           Data.Aeson.Types               ( prependFailure
                                                , typeMismatch
                                                )
import           Linear                         ( V2(V2) )
import           PlatformRunner.Import

encodingOptions :: Data.Aeson.Options
encodingOptions = defaultOptions { rejectUnknownFields = True }

data DisplayMode
  = Fullscreen
  | Windowed String (Int, Int) (Int, Int)
  deriving (Generic, Show)

instance ToJSON DisplayMode where
  toEncoding = genericToEncoding encodingOptions

instance FromJSON DisplayMode

toGlossDisplayMode :: DisplayMode -> Apecs.Display
toGlossDisplayMode = \case
  Fullscreen -> Apecs.FullScreen
  Windowed name (width, height) (x, y) ->
    Apecs.InWindow name (width, height) (x, y)

data Difficulty = Easy | Normal | Hard deriving (Generic)

instance Show Difficulty where
  show = \case
    Easy   -> "Easy"
    Normal -> "Normal"
    Hard   -> "Hard"

instance ToJSON Difficulty where
  toEncoding = genericToEncoding encodingOptions

instance FromJSON Difficulty

newtype WindowDims = WindowDims (V2 Int) deriving (Generic)

instance Show WindowDims where
  show (WindowDims (V2 width height)) = show width <> "x" <> show height

instance ToJSON WindowDims where
  toJSON (WindowDims (V2 width height)) =
    object ["width" .= width, "height" .= height]

instance FromJSON WindowDims where
  parseJSON (Object v) =
    WindowDims <$> (V2 <$> (v .: "width") <*> (v .: "height"))
  parseJSON invalid =
    prependFailure "parsing WindowDims failed, " (typeMismatch "Object" invalid)

data Settings = Settings
  { displayMode :: !DisplayMode
  , difficulty  :: !Difficulty
  , windowDims  :: !WindowDims
  }
  deriving (Generic, Show)

makeLenses ''Settings

instance ToJSON Settings where
  toEncoding = genericToEncoding encodingOptions

instance FromJSON Settings
