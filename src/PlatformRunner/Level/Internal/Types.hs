module PlatformRunner.Level.Internal.Types where

import           Common.Import
import           Linear
import           PlatformRunner.Types

data RelativeItemDescriptor = PlatformDescriptor
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
