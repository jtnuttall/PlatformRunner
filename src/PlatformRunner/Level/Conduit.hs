module PlatformRunner.Level.Conduit
  ( --fetchNextScreen
  ) where

import           Apecs.Physics
import           Conduit
import           Linear
import           PlatformRunner.Level.Internal.Types
import           PlatformRunner.Types           ( Dimensions(unDimensions) )
import           RIO
import qualified RIO.Vector                    as V

extentsAsV2 :: Num a => Dimensions -> V2 a
extentsAsV2 = fmap fromIntegral . unDimensions

-- -- fetchNextScreen
-- --   :: ( HasAppScreenSize env
-- --      , MonadIO m
-- --      , MonadReader env m
-- --      , MonadState Settings m
-- --      , Has w (SystemT w m) Camera
-- --      , Has w m Camera
-- --      )
-- --   => Level
-- --   -> SystemT w m Level

-- fetchNextScreen
--   :: (MonadIO m, PrimMonad m)
--   => V2 Double
--   -> V2 Double
--   -> Double
--   -> Level m
--   -> m (Level m)
-- fetchNextScreen gameDims cameraPos scale level@Level {..} = do
--   let
--     gameDims' = gameDims ^/ scale
--     V2 minX _ = cameraPos - (gameDims' ^* 0.5)

--     absCoords =
--       V.prescanr' subtract lastPlatformPos
--         . fmap (extentsAsV2 . extents)
--         $ levelData

--     lowestOnScreenIndex =
--       subtract 1 <$> V.findIndex (\(V2 x _) -> x > minX) absCoords

--     levelDataSlice = case lowestOnScreenIndex of
--       Nothing -> levelData
--       Just i  -> V.drop (i - 1) levelData

--     V2 mx _ = cameraPos + (gameDims' ^* 1.5)

--     inNextScreen lastPos = \case
--       PlatformDesc {..} -> let V2 px _ = lastPos + offset in px < mx

--     accumNextScreen relObj lastPos
--       | inNextScreen lastPos relObj = Right (lastPos + offset relObj, relObj)
--       | otherwise                   = Left lastPos

--     sink = void (mapAccumWhileC accumNextScreen lastPlatformPos) .| sinkVector

--   (source', newObjVec) <- conduit $$++ sink

--   let newLastPlatformPos = V.foldl' (+) lastPlatformPos (offset <$> newObjVec)

--   return $ level { conduit         = source'
--                  , lastPlatformPos = newLastPlatformPos
--                  , levelData       = levelDataSlice <> newObjVec
--                  }
