{-# LANGUAGE TemplateHaskell #-}
module PlatformRunner.Types where

import           Data.Yaml
import           Lens.Micro.Platform            ( makeClassy )
import           Linear
import           RIO
import           YamlParse.Applicative

newtype Dimensions = Dimensions { unDimensions :: V2 Int }

makeClassy ''Dimensions

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
