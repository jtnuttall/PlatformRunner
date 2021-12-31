{-# LANGUAGE TemplateHaskell #-}
module PlatformRunner.Settings.Types
  ( DisplayMode(..)
  , toGlossDisplayMode
  , Difficulty(..)
  , Settings(..)
  ) where

import qualified Apecs.Gloss                   as Apecs
                                                ( Display(..) )
import           Control.Lens                   ( makeLenses )
import           Data.Aeson
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

makeLenses ''DisplayMode

toGlossDisplayMode :: DisplayMode -> Apecs.Display
toGlossDisplayMode = \case
  Fullscreen -> Apecs.FullScreen
  Windowed name (width, height) (x, y) ->
    Apecs.InWindow name (width, height) (x, y)

data Difficulty = Easy | Normal | Hard deriving (Generic)

makeLenses ''Difficulty

instance Show Difficulty where
  show = \case
    Easy   -> "Easy"
    Normal -> "Normal"
    Hard   -> "Hard"

instance ToJSON Difficulty where
  toEncoding = genericToEncoding encodingOptions

instance FromJSON Difficulty

data Settings = Settings
  { displayMode :: DisplayMode
  , difficulty  :: Difficulty
  }
  deriving (Generic, Show)

makeLenses ''Settings

instance ToJSON Settings where
  toEncoding = genericToEncoding encodingOptions

instance FromJSON Settings
