module PlatformRunner.Level.Generated.FlatWorld
  ( flatWorld
  ) where

import           Common.Import
import           Conduit
import           Linear                         ( V2(V2) )
import           PlatformRunner.Level.Internal.STM
                                                ( InitialLevelParameters(..)
                                                , LevelProducer
                                                , toLevelProducer
                                                )
import           PlatformRunner.Level.Internal.Types
import           PlatformRunner.Types
import           System.Random.MWC
import           System.Random.Stateful

flatWorldProducer :: StatefulGen g m => g -> LevelProducer m
flatWorldProducer g = toLevelProducer . repeatMC $ do
  offsetX  <- uniformRM (5, 20) g
  extentsX <- uniformRM (5, 15) g

  return $ PlatformDescriptor { offset  = V2 offsetX 0
                              , extents = Dimensions (V2 extentsX 3)
                              }

flatworldMetadata :: WinCondition -> LevelMetadata
flatworldMetadata wc = LevelMetadata
  { levelDataPath       = Nothing
  , levelName           = "Infinite Flat World (WC: " <> tshow wc <> ")"
  , levelDescription    = Just "Testing ground"
  , levelProcedural     = Nothing
  , levelPlayerStartPos = V2 1.5 5
  , winCondition        = wc
  }

flatWorldWith
  :: (StatefulGen g m) => g -> WinCondition -> InitialLevelParameters m
flatWorldWith g wc = InitialLevelParameters { initialRequest = 400
                                            , metadata = flatworldMetadata None
                                            , producer = flatWorldProducer g
                                            }

flatWorld :: StatefulGen g m => g -> InitialLevelParameters m
flatWorld g = flatWorldWith g None
