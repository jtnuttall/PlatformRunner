module PlatformRunner.Level.Internal.Types where

import           Conduit
import           Data.Kind                      ( Type )
import           Data.Yaml
import           Linear
import           PlatformRunner.Prelude
import           PlatformRunner.Types
import           YamlParse.Applicative

data RelativeObjectDesc = PlatformDesc
  { offset  :: !(V2 Double)
  , extents :: !Dimensions
  }
  deriving Show

data ProceduralData = ProceduralData
  { proceduralSeed :: Vector Word8
  , proceduralAlgo :: Maybe Text
  }
  deriving Show

data WinCondition
  = Distance Double
  | DistanceInTime Double Double
  | Platform Int
  | None
  deriving Show

data LevelMetadata = LevelMetadata
  { levelDataPath       :: !(Maybe FilePath)
  , levelName           :: !Text
  , levelDescription    :: Maybe Text
  , levelProcedural     :: !(Maybe ProceduralData)
  , levelPlayerStartPos :: !(V2 Double)
  , winCondition        :: !WinCondition
  }
  deriving Show

newtype LevelSource m = LevelSource
  { unLevelSource :: SealedConduitT () RelativeObjectDesc m () }

-- instance Monad LevelSource

-- data LevelState = LevelState
--   { conduit :: !(LevelSource IO)
--   , lastPlatformPos :: !(V2 Double)
--   , levelData :: !(Vector RelativeObjectDesc)}

data Level = Level
  { metadata        :: !LevelMetadata
  , conduit         :: !(LevelSource IO)
  , lastPlatformPos :: !(V2 Double)
  , levelData       :: !(Vector RelativeObjectDesc)
  }

-- instance HasStateRef  


instance Show Level where
  show Level {..} =
    "Level { metadata = "
      <> show metadata
      <> ", "
      <> "conduit = [someConduit]"
      <> ", "
      <> "lastPlatformPos = "
      <> show lastPlatformPos
      <> ", "
      <> "levelData = "
      <> show levelData
      <> " }"
