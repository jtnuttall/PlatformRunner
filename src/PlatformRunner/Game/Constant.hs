module PlatformRunner.Game.Constant
  ( PlatformRunnerSystem
  , PlatformRunnerConstants(..)
  , platformRunnerConstants
  , playerJumpVelocity
  ) where

import           Apecs
import           Apecs.Physics                  ( WVec )
import           Linear
import           PlatformRunner.Game.World
import           PlatformRunner.Settings.Internal
import           RIO
import           RIO.State                      ( MonadState )

type PlatformRunnerSystem env a
  = (MonadState Settings (RIO env)) => SystemT PlatformWorld (RIO env) a

data PlatformRunnerConstants = PlatformRunnerConstants
  { gravityConstant     :: V2 Double -- ^ V2 m/s
  , playerMass          :: Double -- ^ kg
  , playerMoment        :: Double -- ^ kg-m^2
  , playerDimensions    :: V2 Double -- V2 m
  , playerMaxJumpHeight :: Double -- ^ m
  , playerBaseSpeed     :: Double -- ^ m/s
  , cameraScaleFactor   :: Double -- ^ pixel/m
  , coinBaseScore       :: Int -- ^ points
  }
  deriving Show

-- | Use conservation of energy to calculate initial velocity scalar from a 
-- given gravity constant and maximum trajectory height. Takes g's absolute value 
-- so should always return a nonnegative real result.
playerJumpVelocity :: PlatformRunnerConstants -> WVec
playerJumpVelocity PlatformRunnerConstants {..} =
  let V2 x gy = gravityConstant in V2 x . sqrt $ abs gy * playerMaxJumpHeight

infinity :: Fractional a => a
infinity = 1e1000

normal :: PlatformRunnerConstants
normal = PlatformRunnerConstants { gravityConstant     = V2 0 (-9.81)
                                 , playerMass          = 100
                                 , playerMoment        = infinity
                                 , playerDimensions    = V2 0.5 2
                                 , playerMaxJumpHeight = 5
                                 , playerBaseSpeed     = 10
                                 , cameraScaleFactor   = 1
                                 , coinBaseScore       = 1
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
