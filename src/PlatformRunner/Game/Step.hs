module PlatformRunner.Game.Step where

import           Apecs
import           Apecs.Gloss                    ( Camera(Camera) )
import           Apecs.Physics
import           PlatformRunner.Components.Entity
import           PlatformRunner.Components.Global
import           PlatformRunner.Env
import           PlatformRunner.Game.Collision
import           PlatformRunner.Game.Constant
import           PlatformRunner.Level
import           PlatformRunner.Prelude
import           PlatformRunner.Settings        ( Settings
                                                , getGameDimensions
                                                )
import           PlatformRunner.Types
import           System.Random.SplitMix         ( SMGen
                                                , newSMGen
                                                )

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
  -> RelativeObjectDesc
  -> PlatformRunnerSystem env (V2 Double)
newPlatform lastPos = \case
  PlatformDesc {..} -> do
    constants   <- lift $ viewConfig @PlatformRunnerConstants

    platformEty <- newEntity
      (Platform, StaticBody, Position (lastPos + offset), platformFilter)

    newEntity_
      (Shape platformEty . cRectangle . fmap fromIntegral $ unDimensions extents
      )

    return $ lastPos + offset

-- initializeLevel
--   :: (HasLogFunc env, HasAppScreenSize env, HasGameConstants env)
--   => (SMGen -> Level (RIO env))
--   -> RIO env ()
-- initializeLevel levelGen = do
--   constants           <- view gameConstantsL
--   g                   <- liftIO newSMGen
--   Dimensions gameDims <- getGameDimensions
--   levelRef            <- view levelRefL
--   firstScreen         <- fetchNextScreen (fromIntegral <$> gameDims)
--                                          playerStartPos
--                                          (cameraScaleFactor constants)
--                                          (levelGen g)

--   writeSomeRef levelRef (Just firstScreen)

-- getNextScreen
--   :: (HasLogFunc env, HasAppScreenSize env, HasGameConstants env)
--   => PlatformRunnerSystem env ()
-- getNextScreen = do
--   constants           <- lift $ view gameConstantsL
--   Dimensions gameDims <- lift getGameDimensions
--   levelRef            <- lift $ view levelRefL
--   expectLevel         <- readSomeRef levelRef
--   Camera cameraPos _  <- get global

--   case expectLevel of
--     Nothing    -> throwString "Level was unexpectedly Nothing"
--     Just level -> do
--       screen <- lift $ fetchNextScreen (fromIntegral <$> gameDims)
--                                        (realToFrac <$> cameraPos)
--                                        (cameraScaleFactor constants)
--                                        level
--       lift $ writeSomeRef levelRef (Just screen)

initializeSystem
  :: (HasLogFunc env, HasConfigElem env PlatformRunnerConstants)
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

  -- playerEty    <- newPlayer (levelPlayerStartPos . metadata $ initialLevel)

-- foldM_ newPlatform (V2 0 0) (levelData initialLevel)

  cHandlerEtys <- newEntity =<< createCollisionHandler

  return ()

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

step :: Double -> PlatformRunnerSystem env ()
step dT = do
  incTime dT
  moveCamera
  -- clearPlatforms
  stepPhysics dT
