module PlatformRunner.Game.Constant
  ( PlatformRunnerSystem
  , Kinetic
  , PlatformRunnerConstants(..)
  , platformRunnerConstants
  , playerJumpVelocity
  ) where

import           Apecs
import           Linear
import           PlatformRunner.Game.World
import           PlatformRunner.Import
import           PlatformRunner.Settings.Types

type PlatformRunnerSystem env a = SystemT PlatformWorld (RIO env) a

type Kinetic = (Position, Velocity)

data PlatformRunnerConstants = PlatformRunnerConstants
  { playerBaseMass      :: Float -- ^ kg
  , gravityConstant     :: Float -- ^ m/s
  , playerMaxJumpHeight :: Float -- ^ m
  , playerBaseSpeed     :: Float -- ^ m/s
  , playerStartPos      :: V2 Float -- ^ V2 pixels?
  , foodScoreValue      :: Int
  , foodLengthValue     :: Int
  }
  deriving Show

-- | Use conservation of energy to calculate initial velocity scalar from a given
-- gravity constant and maximum trajectory height. Takes g's absolute value so should
-- always return a nonnegative real result.
playerJumpVelocity :: PlatformRunnerConstants -> V2 Float
playerJumpVelocity PlatformRunnerConstants {..} =
  V2 0 . sqrt $ abs gravityConstant * playerMaxJumpHeight

normal :: PlatformRunnerConstants
normal = PlatformRunnerConstants { playerBaseMass      = 100
                                 , gravityConstant     = 10
                                 , playerMaxJumpHeight = 2
                                 , playerBaseSpeed     = 1.0
                                 , playerStartPos      = V2 0 (-500)
                                 , foodScoreValue      = 5
                                 , foodLengthValue     = 1
                                 }

easy :: PlatformRunnerConstants
easy = normal { playerBaseSpeed = 0.8 }

hard :: PlatformRunnerConstants
hard = normal { playerBaseSpeed = 1.2 }

platformRunnerConstants :: Difficulty -> PlatformRunnerConstants
platformRunnerConstants = \case
  Easy   -> easy
  Normal -> normal
  Hard   -> hard
