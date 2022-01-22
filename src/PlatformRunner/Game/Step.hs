{-# LANGUAGE NumericUnderscores #-}
module PlatformRunner.Game.Step where

import           Apecs
import           Apecs.Gloss                    ( Camera(Camera) )
import           Apecs.Physics
import           Common.Import           hiding ( set )
import           Data.Coerce                    ( coerce )
import           Linear                         ( Epsilon(nearZero) )
import           PlatformRunner.Components.Entity
import           PlatformRunner.Components.Global
import           PlatformRunner.Env
import           PlatformRunner.Game.Collision
import           PlatformRunner.Game.Constant
import           PlatformRunner.Game.World      ( PlatformWorld(PlatformWorld) )
import           PlatformRunner.Level
import           PlatformRunner.Settings        ( getGameDimensions )
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
     , HasConfigElem env ScreenSize
     )
  => PlatformRunnerSystem env ()
initializeSystem = do
  constants <- lift viewConfig

  lift $ logDebug $ "Initializing System with: " <> displayShow constants

  modify global $ \(Gravity _) -> Gravity (gravityConstant constants)

  newEntity_
    (Camera (realToFrac <$> playerStartPos)
            (realToFrac $ cameraScaleFactor constants)
    )

  syncLevelData

  initialPosition <- lift $ levelPlayerStartPos <$> viewConfig @LevelMetadata
  void $ newPlayer initialPosition


  newEntity_ =<< createCollisionHandler

  return ()

needPlatformsUntil
  :: (HasConfigElem env ScreenSize) => PlatformRunnerSystem env (Maybe Double)
needPlatformsUntil = do
  Camera cameraPos _                        <- get global
  Dimensions           gameDims             <- lift getGameDimensions
  LastReceivedPlatform lastRecievedPlatform <- get global

  let V2 cameraX   _ = realToFrac <$> cameraPos
      V2 gameWidth _ = fromIntegral <$> gameDims
      V2 lastX     _ = lastRecievedPlatform
      triggerX       = cameraX + gameWidth

  return
    $ if triggerX > lastX then Just (cameraX + (gameWidth * 1.5)) else Nothing

syncLevelData
  :: ( HasConfigElem env PlatformRunnerConstants
     , HasConfigElem env (PullUpdate IO)
     , HasConfigElem env ScreenSize
     , HasLogFunc env
     )
  => PlatformRunnerSystem env ()
syncLevelData = needPlatformsUntil >>= \case
  Nothing    -> lift $ logError "level data synced when no platforms needed"
  Just needX -> do
    LastReceivedPlatform lastRecievedPlatform <- get global

    let V2 lastX lastY = lastRecievedPlatform

    lift (pullLevelData needX) >>= \case
      Closed  -> throwString "mailbox is closed :("
      Waiting -> do
        lift $ logDebug "waiting..."
        return ()
      Result levelData -> do
        newLastPos <- foldM newPlatform (V2 lastX lastY) levelData
        set global (WaitingForPlatforms False)
        set global (LastReceivedPlatform newLastPos)

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
     , HasConfigElem env ScreenSize
     , HasLogFunc env
     )
  => Double
  -> PlatformRunnerSystem env ()
step dT = do
  WaitingForPlatforms waiting <- get global
  needNext                    <- isJust <$> needPlatformsUntil

  when needNext syncLevelData

  unless waiting $ do
    incTime dT
    moveCamera
    -- clearPlatforms
    stepPhysics dT
