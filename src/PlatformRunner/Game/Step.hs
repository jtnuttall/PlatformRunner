{-# LANGUAGE NumericUnderscores #-}
module PlatformRunner.Game.Step where

import           Apecs
import           Apecs.Gloss                    ( Camera(Camera) )
import           Apecs.Physics
import           Common.Import
import           Linear                         ( Epsilon(nearZero) )
import           PlatformRunner.Components.Entity
import           PlatformRunner.Components.Global
import           PlatformRunner.Env
import           PlatformRunner.Game.Collision
import           PlatformRunner.Game.Constant
import           PlatformRunner.Game.World      ( PlatformWorld(PlatformWorld) )
import           PlatformRunner.Level
import           PlatformRunner.Types

baseViewDimensions :: Dimensions
baseViewDimensions = Dimensions (V2 80 45)

playerStartPos :: V2 Double
playerStartPos = V2 10 1

newPlayer
  :: (HasConfigElem env PlatformRunnerConstants)
  => V2 Double
  -> PlatformRunnerSystem env Entity
newPlayer levelPlayerStartPos = do
  PlatformRunnerConstants {..} <- lift viewConfig

  playerEty                    <- newEntity
    ( Player
    , DynamicBody
    , BodyMass playerMass
    , Moment playerMoment
    , Position levelPlayerStartPos
    , Velocity (V2 playerBaseSpeed 0)
    , playerFilter
    )

  newEntity_ (Shape playerEty $ cRectangle playerDimensions)

  return playerEty


newPlatform
  :: (HasConfigElem env PlatformRunnerConstants)
  => V2 Double
  -> RelativeItemDescriptor
  -> PlatformRunnerSystem env (V2 Double)
newPlatform lastPos = \case
  PlatformDescriptor {..} -> do
    constants   <- lift $ viewConfig @PlatformRunnerConstants

    platformEty <- newEntity
      (Platform, StaticBody, Position (lastPos + offset), platformFilter)

    newEntity_
      (Shape platformEty . cRectangle . fmap fromIntegral $ unDimensions extents
      )

    return $ lastPos + offset

initializeSystem
  :: ( HasLogFunc env
     , HasConfigElem env PlatformRunnerConstants
     , HasConfigElem env LevelMetadata
     , HasConfigElem env (PullUpdate IO)
     )
  => PlatformRunnerSystem env ()
initializeSystem = do
  constants <- lift viewConfig

  lift $ logDebug $ "Initializing System with: " <> displayShow constants

  modify global $ \(Gravity _) -> Gravity (gravityConstant constants)

  cameraEty <- newEntity
    (Camera (realToFrac <$> playerStartPos)
            (realToFrac $ cameraScaleFactor constants)
    )

  -- initialLevel <- lift $ initializeLevel flatWorld
  -- levelData    <- lift $ pullLevelData 200

  -- initialLevel <- case levelData of
  --   Closed     -> throwString "closed"
  --   Waiting    -> throwString "waiting"
  --   Result vec -> return vec

  syncLevelData


  initialPosition <- lift $ levelPlayerStartPos <$> viewConfig @LevelMetadata
  playerEty       <- newPlayer initialPosition


  cHandlerEtys    <- newEntity =<< createCollisionHandler

  return ()

syncLevelData
  :: ( HasConfigElem env PlatformRunnerConstants
     , HasConfigElem env (PullUpdate IO)
     )
  => PlatformRunnerSystem env ()
syncLevelData = do
  Time t            <- get global
  Camera (V2 x _) _ <- get global

  when (nearZero (t / 10_000)) $ do
    levelData    <- lift $ pullLevelData 200

    initialLevel <- case levelData of
      Closed     -> throwString "closed"
      Waiting    -> throwString "waiting"
      Result vec -> return vec

    foldM_ newPlatform (V2 0 0) initialLevel


clearPlatforms :: PlatformRunnerSystem env ()
clearPlatforms = do
  let (V2 windowWidth _) =
        fmap fromIntegral . unDimensions $ Dimensions $ V2 680 320

  cmap $ \(Platform, Position (V2 x _)) ->
    if x < 0 || x > windowWidth then Nothing else Just Platform

incTime :: Double -> PlatformRunnerSystem env ()
incTime dT = modify global $ \(Time t) -> Time (t + dT)

moveCamera :: PlatformRunnerSystem env ()
moveCamera = cmapM_ $ \(Player, Position p) ->
  modify global (\(Camera _ s) -> Camera (realToFrac <$> p) s)

step
  :: ( HasConfigElem env (PullUpdate IO)
     , HasConfigElem env PlatformRunnerConstants
     )
  => Double
  -> PlatformRunnerSystem env ()
step dT = do
  incTime dT
  moveCamera
  -- clearPlatforms
  stepPhysics dT
  syncLevelData
