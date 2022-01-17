module PlatformRunner.Game.Collision where

import           Apecs.Physics
import           Common.Import
import           PlatformRunner.Components.Entity
import           PlatformRunner.Env
import           PlatformRunner.Game.Constant   ( PlatformRunnerConstants(..)
                                                , PlatformRunnerSystem
                                                )
import           PlatformRunner.Game.World      ( PlatformWorld(PlatformWorld) )

playerCategory :: Int
playerCategory = 1

enemyCategory :: Int
enemyCategory = 2

platformCategory :: Int
platformCategory = 3

obstacleCategory :: Int
obstacleCategory = 4

playerFilter :: CollisionFilter
playerFilter = CollisionFilter
  { filterGroup      = 1
  , filterCategories = maskList [playerCategory]
  , filterMask       = maskList [platformCategory, obstacleCategory]
  }

enemyFilter :: CollisionFilter
enemyFilter = CollisionFilter
  { filterGroup      = 2
  , filterCategories = maskList [enemyCategory]
  , filterMask       = maskList
    [playerCategory, enemyCategory, platformCategory, obstacleCategory]
  }

platformFilter :: CollisionFilter
platformFilter = CollisionFilter
  { filterGroup      = 3
  , filterCategories = maskList [platformCategory]
  , filterMask       = maskList [playerCategory, enemyCategory]
  }

obstacleFilter :: CollisionFilter
obstacleFilter = CollisionFilter
  { filterGroup = 4
  , filterCategories = maskList [obstacleCategory]
  , filterMask = maskList [playerCategory, enemyCategory, platformCategory]
  }

createCollisionHandler
  :: (HasConfigElem env PlatformRunnerConstants, MonadIO m, MonadReader env m)
  => SystemT PlatformWorld m CollisionHandler
createCollisionHandler = do
  constants <- lift viewConfig

  beginCB'  <- mkBeginCB $ \Collision {..} -> do
    aIsPlatform <- exists collisionBodyA (Proxy @Platform)
    bIsPlayer   <- exists collisionBodyB (Proxy @Player)


    when (aIsPlatform && bIsPlayer) $ do
      if collisionNormal == up
        then do
          cmap
            (\(Player, Velocity (V2 _ vy0)) ->
              ( Not @IsFlying
              , Moment (playerMoment constants)
              , Velocity $ V2 (playerBaseSpeed constants) vy0
              )
            )
        else do
          cmap
            (\(Player, Velocity (V2 _ vy0)) ->
              (Moment 5, Velocity $ V2 (playerBaseSpeed constants) vy0)
            )

    return True

  separateCB' <- mkSeparateCB $ \Collision {..} -> do
    aIsPlatform <- exists collisionBodyA (Proxy @Platform)
    bIsPlayer   <- exists collisionBodyB (Proxy @Player)

    when (aIsPlatform && bIsPlayer) $ do
      if collisionNormal == up
        then cmap (\Player -> IsFlying)
        else cmap (\Player -> Moment 10)

  return $ defaultHandler { beginCB    = Just beginCB'
                          , separateCB = Just separateCB'
                          }
