module PlatformRunner.Level.Generated.FlatWorld
  ( --flatWorld
  ) where

import           Conduit
import           Lens.Micro.Platform
import           Linear                         ( V2(V2) )
import           PlatformRunner.Level.Internal.Types
import           PlatformRunner.Types
import           RIO
import           System.Random.SplitMix
import           Utility.Math                   ( expandRange )

-- | Mostly for debugging purposes
-- flatWorldWith :: (Monad m) => SMGen -> WinCondition -> Level
-- flatWorldWith gen wc = Level
--   { metadata        = LevelMetadata
--                         { levelDataPath       = Nothing
--                         , levelName = "Infinite Flat World (WC: " <> tshow wc <> ")"
--                         , levelDescription    = Just "Testing ground"
--                         , levelProcedural     = Nothing
--                         , levelPlayerStartPos = V2 1.5 5
--                         , winCondition        = wc
--                         }
--   , conduit         = sealConduitT $ unfoldC
--                         (\g ->
--                           let (offsetX, g') = over _1 ((+ 5) . (* 20)) (nextDouble g)
--                               (extentsX, g'') =
--                                 over _1 ((+ 5) . (* 15)) (nextDouble g')
--                           in  Just
--                                 ( PlatformDesc
--                                   { offset  = V2 offsetX 0
--                                   , extents = Dimensions (V2 (round extentsX) 3)
--                                   }
--                                 , g''
--                                 )
--                         )
--                         gen
--   , lastPlatformPos = 0
--   , levelData       = mempty
--   }

-- flatWorld :: Monad m => SMGen -> Level
-- flatWorld g = flatWorldWith g None
